#ifndef SWIFT_SEMA_TYPEFULLYINHABITED_H
#define SWIFT_SEMA_TYPEFULLYINHABITED_H

#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/TypeCheckRequests.h"

namespace swift {
class ProtocolConformance;

bool checkConvertibleToBytesConformance(ProtocolConformance *conformance);
bool checkConvertibleFromBytesConformance(ProtocolConformance *conformance);
} // end namespace swift

#endif // !SWIFT_SEMA_TYPEFULLYINHABITED_H
