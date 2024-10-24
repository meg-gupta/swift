// RUN: %target-swift-frontend -O -emit-sil -Xllvm -sil-disable-pass=RedundantLoadElimination %s | %FileCheck %s

// REQUIRES: swift_in_compiler

let globalLetArray = [1, 2, 3, 4]

@inline(never)
func useGlobal() {
  print(globalLetArray)
}

// RLE should eliminate the redundant array load - rdar://138519664
// CHECK-LABEL: sil hidden @$s7abcopts5test15indexS2i_tF : $@convention(thin) (Int) -> Int {
// cond_fail {{.*}} "Index out of range" // cond_fail not merged because load of arr.count is blocking
// cond_fail {{.*}} "Index out of range"
// CHECK-LABEL: } // end sil function '$s7abcopts5test15indexS2i_tF'
func test1(index: Int) -> Int {
  let elem1 = globalLetArray[index]
  useGlobal()
  let elem2 = globalLetArray[index]
  return elem1 + elem2
}

struct Wrapper {
  let arr: Array<Int>
  init() {
    arr = [1, 2, 3, 4]
  }
}

// RLE should eliminate the redundant array load - rdar://138519664
// CHECK-LABEL: sil hidden @$s7abcopts5test25indexS2i_tF : $@convention(thin) (Int) -> Int {
// cond_fail {{.*}} "Index out of range"
// CHECK-LABEL: } // end sil function '$s7abcopts5test25indexS2i_tF'
func test2(index: Int) -> Int {
  let w = Wrapper()
  let elem1 = w.arr[index]
  useGlobal()
  let elem2 = w.arr[index]
  return elem1 + elem2
}

