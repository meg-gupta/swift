// RUN: %swift-frontend %s -enable-import-ptrauth-field-function-pointers -emit-ir -target arm64e-apple-ios13.0 -I %S/Inputs/ -validate-tbd-against-ir=none 2>&1 | %FileCheck %s
// REQUIRES: CPU=arm64e
// REQUIRES: OS=ios

import PointerAuth

// CHECK: define hidden swiftcc i32 @"$s25ptrauth_field_fptr_import05test_B8_fn_reads5Int32VyF"() #0 {
// CHECK: [[LD:%.*]] = load i64, i64* bitcast (%struct.SecureStruct** @ptr_to_secure_struct to i64*), align 8
// CHECK: 5:                                                ; preds = %entry
// CHECK:   [[CAST0:%.*]] = inttoptr i64 [[LD]] to i8*
// CHECK:   br label %12
// CHECK: 12:
// CHECK:   [[SECURESTRUCT:%.*]] = phi i8* [ [[CAST0]], %5 ]
// CHECK:   [[CAST1:%.*]] = bitcast i8* [[SECURESTRUCT]] to %TSo12SecureStructV*
// CHECK:   %.secure_func_ptr = getelementptr inbounds %TSo12SecureStructV, %TSo12SecureStructV* %14, i32 0, i32 0
// CHECK:   [[CAST2:%.*]] = bitcast %Ts5Int32VIetCd_Sg* %.secure_func_ptr to i64*
// CHECK:   [[PTR:%.*]] = load i64*, i64** [[CAST2]], align 8
// CHECK:   [[SIGNEDINT:%.*]] = ptrtoint i64* [[PTR]] to i64
// CHECK:   [[AUTHVAL:%.*]] = call i64 @llvm.ptrauth.auth(i64 [[SIGNEDINT]], i32 2, i64 88)
// CHECK:   [[AUTHPTR:%.*]] = inttoptr i64 [[AUTHVAL]] to i64*
// CHECK:   [[TMPCAST1:%.*]] = bitcast %Ts5Int32VIetCd_Sg* %ptrauth.temp to i64**
// CHECK:   store i64* [[AUTHPTR]], i64** [[TMPCAST1]], align 8
// CHECK:   [[TMPCAST2:%.*]] = bitcast %Ts5Int32VIetCd_Sg* %ptrauth.temp to i64*
// CHECK:   [[FUNCPTR:%.*]] = load i64, i64* [[TMPCAST2]], align 8
func test_field_fn_read() -> Int32 {
  let fn = ptr_to_secure_struct!.pointee.secure_func_ptr!
  return fn()
}

// CHECK-LABEL: define hidden swiftcc void @"$s25ptrauth_field_fptr_import05test_B14_fn_ptr_modifyyyF"() #0 {
// CHECK: 11:                                               ; preds = %4
// CHECK:   [[SECURESTRUCT:%.*]] = phi i8* [ %5, %4 ]
// CHECK:   [[CAST1:%.*]] = bitcast i8* [[SECURESTRUCT]] to %TSo12SecureStructV*
// CHECK:   %.secure_func_ptr = getelementptr inbounds %TSo12SecureStructV, %TSo12SecureStructV* [[CAST1]], i32 0, i32 0
// CHECK:   [[CAST2:%.*]] = bitcast %Ts5Int32VIetCd_Sg* %ptrauth.temp to i64*
// CHECK:   store i64 ptrtoint ({ i8*, i32, i64, i64 }* @returnInt.ptrauth to i64), i64* [[CAST2]], align 8
// CHECK:   [[CAST3:%.*]] = bitcast %Ts5Int32VIetCd_Sg* %ptrauth.temp to i64**
// CHECK:   [[LD:%.*]] = load i64*, i64** [[CAST3]], align 8
// CHECK:   [[CAST4:%.*]] = ptrtoint i64* [[LD]] to i64
// CHECK:   [[SIGN:%.*]] = call i64 @llvm.ptrauth.sign(i64 [[CAST4]], i32 2, i64 88)
// CHECK:   [[CAST5:%.*]] = inttoptr i64 [[SIGN]] to i64*
// CHECK:   [[CAST6:%.*]] = bitcast %Ts5Int32VIetCd_Sg* %.secure_func_ptr to i64**
// CHECK:   store i64* [[CAST5]], i64** [[CAST6]], align 8
func test_field_fn_ptr_modify() {
  ptr_to_secure_struct!.pointee.secure_func_ptr = returnInt
}

print(test_field_fn_read())
print(test_field_fn_ptr_modify())
