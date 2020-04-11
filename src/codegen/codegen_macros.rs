/// Writes the specified label to the program text
macro_rules! write_label {
    ($prgm:expr, $label:expr) => {
        $prgm.text.push_str(&format!("{}:\n", $label));
    };
    ($prgm:expr, $label:expr ; $comment:expr) => {
        $prgm.text.push_str(&format!("{}:        # {}\n", $label, $comment));
    }
}
/// The underlying macro that actually generates the formatting code
macro_rules! make_instr {
    // load/store where we reference an address offset from a register
    ($prgm:expr, $name:expr, $dest:expr, $offset:expr, ( $reg:expr )) => {
        $prgm.text.push_str(&format!("    {:4} {},{}({})\n", $name, $dest, $offset, $reg));
    };
    // commented version
    ($prgm:expr, $name:expr, $dest:expr, $offset:expr, ( $reg:expr ) ; $comment:expr) => {
        $prgm.text.push_str(&format!("    {:4} {},{}({})", $name, $dest, $offset, $reg));
        $prgm.text.push_str(&format!("    # {}\n", $comment));
    };
    
    // zero argument instructions
    ($prgm:expr, $name:expr) => {
        $prgm.text.push_str(&format!("    {:4}\n", $name));
    };
    // commented version
    ($prgm:expr, $name:expr ; $comment:expr) => {
        $prgm.text.push_str(&format!("    {:4}", $name));
        $prgm.text.push_str(&format!("    # {}\n", $comment));
    };
    
    // one argument instructions
    ($prgm:expr, $name:expr, $reg:expr) => {
        $prgm.text.push_str(&format!("    {:4} {}\n", $name, $reg));
    };
    // commented version
    ($prgm:expr, $name:expr, $reg:expr ; $comment:expr) => {
        $prgm.text.push_str(&format!("    {:4} {}", $name, $reg));
        $prgm.text.push_str(&format!("    # {}\n", $comment));
    };
    
    // two argument instructions
    ($prgm:expr, $name:expr, $dest:expr, $reg:expr) => {
        $prgm.text.push_str(&format!("    {:4} {},{}\n", $name, $dest,$reg));
    };
    // commented version
    ($prgm:expr, $name:expr, $dest:expr, $reg:expr ; $comment:expr) => {
        $prgm.text.push_str(&format!("    {:4} {},{}", $name, $dest,$reg));
        $prgm.text.push_str(&format!("    # {}\n", $comment));
    };

    // three argument instructions
    ($prgm:expr, $name:expr, $dest:expr, $reg0:expr, $reg1:expr) => {
        $prgm.text.push_str(&format!("    {:4} {},{},{}\n", $name, $dest, $reg0, $reg1));
    };
    // commented version
    ($prgm:expr, $name:expr, $dest:expr, $reg0:expr, $reg1:expr ; $comment:expr) => {
        $prgm.text.push_str(&format!("    {:4} {},{},{}", $name, $dest, $reg0, $reg1));
        $prgm.text.push_str(&format!("    # {}\n", $comment));
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
            Val::Varbl(varbl) => {
                make_instr!($prgm, "move", $reg, $prgm.reg_alloc.get(varbl));
            }
            Val::Const(n) => {
                make_instr!($prgm, "li", $reg, *n);
            }
            Val::Nothing => { /* no op */ },
        }
    };
    ($prgm:expr, $reg:expr, $val:expr ; $comment:expr) => {
         match $val {
            Val::Varbl(varbl) => {
                make_instr!($prgm, "move", $reg, $prgm.reg_alloc.get(varbl) ; $comment);
            }
            Val::Const(n) => {
                make_instr!($prgm, "li", $reg, *n ; $comment);
            }
            Val::Nothing => { /* no op */ },
        }
    };
}