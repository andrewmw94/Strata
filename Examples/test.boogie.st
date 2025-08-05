program Boogie;
var count: int;

procedure double(x : int) returns (ret : int)
spec {
  modifies count;
  ensures (ret == 2*x);
  ensures (old(count) + 1 == count);
}
{
  count := count + 1;
  ret := x + x;
};

procedure coprime(x: int, y: int) returns (ret: bool)
spec {
}
{
    var local_x: int;
    var local_y: int;
    var i: int;
    var tmp: int;

    // Initialize local variables
    local_x := x;
    local_y := y;

    // Swap if x < y
    if (local_x < local_y) {
        tmp := local_x;
        local_x := local_y;
        local_y := tmp;
    }

    // At this point, local_x >= local_y
    assert local_x >= local_y;

    i := local_y;
       
    ret := true;
};

