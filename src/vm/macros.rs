macro_rules! binary_op {
    ($self:ident, $token:ident, $lhs:ident, $rhs:ident, $op:tt, =$dst:ident) => {{
        let lhs = $self.vm.vstack.get($lhs, $token);
        let rhs = $self.vm.vstack.get($rhs, $token);
        let result = lhs $op rhs;
        $self.vm.vstack.set($dst, result);
    }}
}
