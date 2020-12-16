// Expected: 1
define i64 @main(i64 %argc, i8** %arcv) {
  %1 = icmp eq i64 1, 1
  ret i64 %1
}
