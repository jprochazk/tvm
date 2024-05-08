macro_rules! binary_op {
    ($stack:ident, $token:ident, $lhs:ident, $rhs:ident, $op:tt, =$dst:ident) => {{
        let lhs = $stack.get($lhs, $token);
        let rhs = $stack.get($rhs, $token);
        let result = lhs $op rhs;
        $stack.set($dst, result);
    }}
}
