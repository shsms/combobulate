// -*- combobulate-test-point-overlays: ((1 outline 153) (2 outline 188) (3 outline 206) (4 outline 217)); eval: (combobulate-test-fixture-mode t); -*-
fn go(x: Option<i32>) -> i32 {
    match x {
        Some(v) => v,
        None => 0,
    }
}
