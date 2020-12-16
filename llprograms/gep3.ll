// Expected: 3
%arr = type [2 x i64]
@tmp = global [2 x %arr] [ %arr [ i64 1, i64 2 ], %arr [ i64 3, i64 4 ] ]

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = alloca i64
  %2 = getelementptr [2 x %arr], [2 x %arr]* @tmp, i32 0, i32 1, i32 0
  %3 = load i64, i64* %2
  ret i64 %3
}
