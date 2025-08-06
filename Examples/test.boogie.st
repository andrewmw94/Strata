program Boogie;

var count: int;

procedure square(x : int) returns (ret : int)
spec {
}
{
  // global declaration
  count := count + 1;
  ret := (x * x);
};

procedure sum_1_to_n(n: int) returns (ret: int)
spec {
    requires n >= 0;
}
{
    var i: int;
    var s: int;
    
    i := 0;
    s := 0;

    while (i < n + 1)
    invariant 0 <= i <= n + 1;
    invariant s == i * (i - 1) / 2;
    {
        s := s + i;
        i := i + 1;
    }

    assert s == n * (n + 1) / 2;
    ret := s;
}