// -*- combobulate-test-point-overlays: ((1 outline 185) (2 outline 197) (3 outline 209)); eval: (combobulate-test-fixture-mode t); -*-
fn process<T, U, V>(t: T, u: U, v: V)
where
    T: Foo,
    U: Bar,
    V: Baz,
{
    unimplemented!()
}
