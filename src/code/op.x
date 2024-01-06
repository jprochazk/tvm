def nop
end

def mov
  src: Reg
  dst: Reg
end

def load_cst
  src: Cst
  dst: Reg
end

def load_unit
  dst: Reg
end

def load_smi
  val: Smi
  dst: Reg
end

def load_true
  dst: Reg
end

def load_false
  dst: Reg
end

def ret
end

def retv
  src: Reg
end

def stop
end
