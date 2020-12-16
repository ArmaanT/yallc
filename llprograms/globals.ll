// Expected: 20
@gbl = global i64 12

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = load i64, i64* @gbl
  %2 = add i64 %1, 8
  store i64 %2, i64* @gbl
  %3 = load i64, i64* @gbl
  ret i64 %3
}
