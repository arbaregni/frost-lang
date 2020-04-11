/// Writes the specified label to the program text
macro_rules! write_label {
    ($prgm:expr, $label:expr) => {
        $prgm.text.push_str(&format!("{}:\n", $label));
    }
}
/// Writes the specified assembly instruction to the program text
macro_rules! make_instr {
    ($prgm:expr, $prefix:expr, $dest:expr, $offset:expr, ( $reg:expr )) => {
        $prgm.text.push_str(&format!("    {} {},{}({})\n", $prefix, $dest, $offset, $reg))
    };
    ($prgm:expr, $prefix:expr) => {
        $prgm.text.push_str(&format!("    {}\n", $prefix))
    };
    ($prgm:expr, $prefix:expr, $reg:expr) => {
        $prgm.text.push_str(&format!("    {} {}\n", $prefix, $reg))
    };
    ($prgm:expr, $prefix:expr, $dest:expr, $reg:expr) => {
        $prgm.text.push_str(&format!("    {} {},{}\n", $prefix, $dest,$reg))
    };
    ($prgm:expr, $prefix:expr, $dest:expr, $reg0:expr, $reg1:expr) => {
        $prgm.text.push_str(&format!("    {} {},{},{}\n", $prefix, $dest, $reg0, $reg1))
    };
}

/// Evaluates to a string containing the register for the specified variable
/// Possibly writes lines to load the value into a register
macro_rules! get_reg {
    ($prgm:expr, $varbl:expr) => {
        $prgm.reg_alloc.get($varbl)
    }
}

macro_rules! set_reg {
    ($prgm:expr, $reg:expr, $val:expr) => {
        match $val {
            Val::Varbl(varbl) => make_instr!($prgm, "move", $reg, $prgm.reg_alloc.get(varbl)),
            Val::Const(n) => make_instr!($prgm, "li", $reg, *n),
            Val::Nothing => { /* no op */ },
        }
    }
}