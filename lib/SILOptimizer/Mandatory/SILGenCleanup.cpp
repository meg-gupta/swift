//===--- SILGenCleanup.cpp ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Perform peephole-style "cleanup" to aid SIL diagnostic passes.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "silgen-cleanup"

#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/OSSALifetimeCompletion.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/PrunedLiveness.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/DeadEndBlocksAnalysis.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/BasicBlockOptUtils.h"
#include "swift/SILOptimizer/Utils/CanonicalizeInstruction.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "llvm/ADT/PostOrderIterator.h"

using namespace swift;

// Define a CanonicalizeInstruction subclass for use in SILGenCleanup.
struct SILGenCanonicalize final : CanonicalizeInstruction {
  bool changed = false;
  llvm::SmallPtrSet<SILInstruction *, 16> deadOperands;

  SILGenCanonicalize(DeadEndBlocks &deadEndBlocks)
      : CanonicalizeInstruction(DEBUG_TYPE, deadEndBlocks) {}

  void notifyNewInstruction(SILInstruction *) override { changed = true; }

  // Just delete the given 'inst' and record its operands. The callback isn't
  // allowed to mutate any other instructions.
  void killInstruction(SILInstruction *inst) override {
    deadOperands.erase(inst);
    for (auto &operand : inst->getAllOperands()) {
      if (auto *operInst = operand.get()->getDefiningInstruction())
        deadOperands.insert(operInst);
    }
    inst->eraseFromParent();
    changed = true;
  }

  void notifyHasNewUsers(SILValue) override { changed = true; }

  /// Delete trivially dead instructions in non-deterministic order.
  ///
  /// We either have that nextII is endII or if nextII is not endII then endII
  /// is nextII->getParent()->end().
  SILBasicBlock::iterator deleteDeadOperands(SILBasicBlock::iterator nextII,
                                             SILBasicBlock::iterator endII) {
    auto callbacks = InstModCallbacks().onDelete([&](SILInstruction *deadInst) {
      LLVM_DEBUG(llvm::dbgs() << "Trivially dead: " << *deadInst);

      // If nextII is the instruction we are going to delete, move nextII past
      // it.
      if (deadInst->getIterator() == nextII)
        ++nextII;

      // Then remove the instruction from the set and delete it.
      deadOperands.erase(deadInst);
      deadInst->eraseFromParent();
    });

    while (!deadOperands.empty()) {
      SILInstruction *deadOperInst = *deadOperands.begin();

      // Make sure at least the first instruction is removed from the set.
      deadOperands.erase(deadOperInst);

      // Then delete this instruction/everything else that we can.
      eliminateDeadInstruction(deadOperInst, callbacks);
    }
    return nextII;
  }
};

//===----------------------------------------------------------------------===//
// SILGenCleanup: Top-Level Module Transform
//===----------------------------------------------------------------------===//

namespace {

// SILGenCleanup must run on all functions that will be seen by any analysis
// used by diagnostics before transforming the function that requires the
// analysis. e.g. Closures need to be cleaned up before the closure's parent can
// be diagnosed.
//
// TODO: This pass can be converted to a function transform if the mandatory
// pipeline runs in bottom-up closure order.
struct SILGenCleanup : SILModuleTransform {
  void run() override;

  bool completeOSSALifetimes(SILFunction *function);
  template <typename Range>
  bool completeLifetimesInRange(Range const &range,
                                OSSALifetimeCompletion &completion,
                                BasicBlockSet &completed);
};

/// Progress made so far on a walk beginning at an unreachable-terminated block.
struct Walk {
  SILBasicBlock *current;
  SmallPtrSet<SILBasicBlock *, 16> seen;
};

/// Populate `roots` with the last blocks that are discovered via backwards
/// walks along any non-repeating paths starting at the ends in `walks`.
void collectReachableRoots(SILFunction *function, SmallVectorImpl<Walk> &walks,
                           StackList<SILBasicBlock *> &roots) {
  assert(!walks.empty());
  assert(roots.empty());

  // Always include the entry block as a root.  Currently SILGen will emit
  // consumes in unreachable blocks of values defined in reachable blocks (e.g.
  // test/SILGen/unreachable_code.swift:testUnreachableCatchClause).
  // TODO: [fix_silgen_destroy_unreachable] Fix SILGen not to emit such
  //                                        destroys and don't add the entry
  //                                        block to roots here.
  roots.push_back(function->getEntryBlock());

  StackList<SILBasicBlock *> backedges(function);

  BasicBlockSet visited(function);
  while (!walks.empty()) {
    auto walk = walks.pop_back_val();

    SILBasicBlock *current = walk.current;

    while (auto *block = current) {
      current = nullptr;
      bool previouslyVisited = visited.insert(block);

      if (!walk.seen.insert(block).second) {
        // This block was seen on _this_ path.
        backedges.push_back(block);
        continue;
      }

      if (!previouslyVisited) {
        // This block was seen on another path.  Stop walking.
        continue;
      }

      // No predecessors?  Found a root.
      if (block->pred_empty()) {
        if (block == function->getEntryBlock()) {
          // TODO: [fix_silgen_destroy_unreachable] Remove this condition.
          continue;
        }
        roots.push_back(block);
        continue;
      }

      // At least one predecessor.  Continue walking this path from the first
      // predecessor and add all the others to "ends" for subsequent walking.

      for (auto pair : llvm::enumerate(block->getPredecessorBlocks())) {
        auto *predecessor = pair.value();
        if (pair.index() == 0) {
          // Continue this walk from `predecessor`.
          current = predecessor;
        } else {
          // Clone this walk to another that continues from `predecessor`.
          walks.push_back({predecessor, walk.seen});
        }
      }
    }
  }

  // Only complete starting from backedges after completing starting from all
  // nodes without predecessors.  This ensures that completion is done with
  // respect to the post-order based at a node without predecessors rather than
  // one based at a backedge when both a node without predecessor and a
  // backedge are discovered from the same end.
  for (auto *backedge : backedges) {
    roots.push_back(backedge);
  }
}

bool SILGenCleanup::completeOSSALifetimes(SILFunction *function) {
  if (!getModule()->getOptions().OSSACompleteLifetimes)
    return false;

  LLVM_DEBUG(llvm::dbgs() << "Completing lifetimes in " << function->getName()
                          << "\n");

  // First, collect all blocks terminated in unreachable.  Enables bailing out
  // if there are none.
  SmallVector<Walk, 32> walks;
  function->visitUnreachableTerminatedBlocks([&walks](auto &block) {
    walks.push_back({&block, {}});
  });

  if (walks.empty()) {
    // There are no unreachable-terminated blocks, so there are no lifetimes to
    // complete.  (SILGen may emit incomplete lifetimes, but not underconsumed
    // lifetimes.)
    return false;
  }

  // Lifetimes must be completed in unreachable blocks that are reachable via
  // backwards walk from unreachable instructions.  First, check whether there
  // are any unreachable blocks.
  ReachableBlocks reachableBlocks(function);
  reachableBlocks.compute();
  StackList<SILBasicBlock *> roots(function);
  if (!reachableBlocks.hasUnreachableBlocks()) {
    // There are no blocks that are unreachable from the entry block.  Thus,
    // every block will be completed when completing the post-order of the
    // entry block.
    roots.push_back(function->getEntryBlock());
  } else {
    // There are unreachable blocks.  Determine the roots that can be reached
    // when walking from the unreachable blocks.
    collectReachableRoots(function, walks, roots);
  }

  bool changed = false;
  DeadEndBlocks *deb = getAnalysis<DeadEndBlocksAnalysis>()->get(function);
  OSSALifetimeCompletion completion(function, /*DomInfo*/ nullptr, *deb);
  BasicBlockSet completed(function);
  for (auto *root : roots) {
    if (root == function->getEntryBlock()) {
      assert(!completed.contains(root));
      // When completing from the entry block, prefer the PostOrderAnalysis so
      // the result is cached.
      PostOrderFunctionInfo *postOrder =
          getAnalysis<PostOrderAnalysis>()->get(function);
      changed |= completeLifetimesInRange(postOrder->getPostOrder(), completion,
                                          completed);
    }
    if (completed.contains(root)) {
      // This block has already been completed in some other post-order
      // traversal.  Thus the entire post-order rooted at it has already been
      // completed.
      continue;
    }
    changed |= completeLifetimesInRange(
        make_range(po_begin(root), po_end(root)), completion, completed);
  }
  function->verifyOwnership(/*deadEndBlocks=*/nullptr);
  return changed;
}

template <typename Range>
bool SILGenCleanup::completeLifetimesInRange(Range const &range,
                                             OSSALifetimeCompletion &completion,
                                             BasicBlockSet &completed) {
  bool changed = false;
  for (auto *block : range) {
    if (!completed.insert(block))
      continue;
    LLVM_DEBUG(llvm::dbgs()
               << "Completing lifetimes in bb" << block->getDebugID() << "\n");
    for (SILInstruction &inst : reverse(*block)) {
      for (auto result : inst.getResults()) {
        LLVM_DEBUG(llvm::dbgs() << "completing " << result << "\n");
        if (completion.completeOSSALifetime(
                result, OSSALifetimeCompletion::Boundary::Availability) ==
            LifetimeCompletion::WasCompleted) {
          LLVM_DEBUG(llvm::dbgs() << "\tcompleted!\n");
          changed = true;
        }
      }
    }
    for (SILArgument *arg : block->getArguments()) {
      LLVM_DEBUG(llvm::dbgs() << "completing " << *arg << "\n");
      assert(!arg->isReborrow() && "reborrows not legal at this SIL stage");
      if (completion.completeOSSALifetime(
              arg, OSSALifetimeCompletion::Boundary::Availability) ==
          LifetimeCompletion::WasCompleted) {
        LLVM_DEBUG(llvm::dbgs() << "\tcompleted!\n");
        changed = true;
      }
    }
  }
  return changed;
}

void SILGenCleanup::run() {
  auto &module = *getModule();
  for (auto &function : module) {
    if (!function.isDefinition())
      continue;

    PrettyStackTraceSILFunction stackTrace("silgen cleanup", &function);

    LLVM_DEBUG(llvm::dbgs()
               << "\nRunning SILGenCleanup on " << function.getName() << "\n");

    bool changed = completeOSSALifetimes(&function);
    DeadEndBlocks deadEndBlocks(&function);
    SILGenCanonicalize sgCanonicalize(deadEndBlocks);

    // Iterate over all blocks even if they aren't reachable. No phi-less
    // dataflow cycles should have been created yet, and these transformations
    // are simple enough they shouldn't be affected by cycles.
    for (auto &bb : function) {
      for (auto ii = bb.begin(), ie = bb.end(); ii != ie;) {
        ii = sgCanonicalize.canonicalize(&*ii);
        ii = sgCanonicalize.deleteDeadOperands(ii, ie);
      }
    }
    changed |= sgCanonicalize.changed;
    if (changed) {
      auto invalidKind = SILAnalysis::InvalidationKind::Instructions;
      invalidateAnalysis(&function, invalidKind);
    }
  }
}

} // end anonymous namespace

SILTransform *swift::createSILGenCleanup() { return new SILGenCleanup(); }
