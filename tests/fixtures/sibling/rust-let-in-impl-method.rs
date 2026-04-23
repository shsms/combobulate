// -*- combobulate-test-point-overlays: ((1 outline 177) (2 outline 202) (3 outline 227)); eval: (combobulate-test-fixture-mode t); -*-
impl X {
    fn new() -> Self {
        let mut ctx = 1;
        build(&mut ctx);
        ctx
    }

    fn other() {}
}
