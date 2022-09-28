target_sources(swiftSIL PRIVATE
  DebugInfoVerifier.cpp
  LoadBorrowImmutabilityChecker.cpp
  LinearLifetimeChecker.cpp
  MemoryLifetimeVerifier.cpp
  OwnershipPhiVerifier.cpp
  SILOwnershipVerifier.cpp
  SILVerifier.cpp)
