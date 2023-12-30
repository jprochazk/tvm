def nop
end

def mov
  src: Register
  dst: Register
end

def load_cst
  src: Constant
  dst: Register
end

def load_smi
  val: i8
  dst: Register
end

def load_true
  dst: Register
end

def load_false
  dst: Register
end
