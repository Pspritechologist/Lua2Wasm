mod parsing;
mod bytecode;
mod debug;

use anyhow::{Result, anyhow};
use luant_lexer::IdentKey;
use slotmap::SlotMap;
use walrus::{FunctionBuilder, Module};
