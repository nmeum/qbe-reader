#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BaseType {
    Word,
    Long,
    Single,
    Double,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ExtType {
    Base(BaseType),
    Byte,
    Halfword,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SubWordType {
    SignedByte,
    UnsignedByte,
    SignedHalf,
    UnsignedHalf,
}

impl SubWordType {
    pub fn is_signed(&self) -> bool {
        match self {
            SubWordType::SignedByte => true,
            SubWordType::UnsignedByte => false,
            SubWordType::SignedHalf => true,
            SubWordType::UnsignedHalf => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SubLongType {
    SubWord(SubWordType),
    SignedWord,
    UnsignedWord,
}

impl SubLongType {
    pub fn is_signed(&self) -> bool {
        match self {
            SubLongType::SubWord(x) => x.is_signed(),
            SubLongType::SignedWord => true,
            SubLongType::UnsignedWord => false,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum SubType {
    ExtType(ExtType),
    UserDef(String),
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Base(BaseType),
    SubWordType(SubWordType),
    UserDef(String),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LoadType {
    Base(BaseType),
    SubLong(SubLongType),
}

impl LoadType {
    pub fn is_signed(&self) -> bool {
        match self {
            LoadType::Base(_) => false,
            LoadType::SubLong(x) => x.is_signed(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Const {
    Number(i64),
    SFP(f32),
    DFP(f64),
    Global(String),
}

#[derive(Debug, PartialEq)]
pub enum DynConst {
    Const(Const),
    Thread(String),
}

#[derive(Debug, PartialEq)]
pub enum Linkage {
    Export,
    Thread,
    Section(String, Option<String>),
}

#[derive(Debug, PartialEq)]
pub struct TypeDef {
    pub name: String,
    pub defn: AggregateType,
}

#[derive(Debug, PartialEq)]
pub enum AggregateType {
    Regular(Option<u64>, Vec<(SubType, u64)>),
    Opaque(u64, u64),
}

#[derive(Debug, PartialEq)]
pub struct DataDef {
    pub linkage: Vec<Linkage>,
    pub name: String,
    pub align: Option<u64>,
    pub objs: Vec<DataObj>,
}

#[derive(Debug, PartialEq)]
pub enum DataObj {
    DataItem(ExtType, Vec<DataItem>),
    ZeroFill(u64),
}

#[derive(Debug, PartialEq)]
pub enum DataItem {
    Symbol(String, Option<u64>),
    String(String),
    Const(Const),
}

#[derive(Debug, PartialEq)]
pub struct FuncDef {
    pub linkage: Vec<Linkage>,
    pub name: String,
    pub abity: Option<Type>,
    pub params: Vec<FuncParam>,
    pub body: Vec<Block>,
}

#[derive(Debug, PartialEq)]
pub enum FuncParam {
    Regular(Type, String),
    Env(String),
    Variadic,
}

impl FuncParam {
    pub fn get_name(&self) -> Option<&str> {
        match self {
            FuncParam::Regular(_, n) => Some(n),
            FuncParam::Env(n) => Some(n),
            FuncParam::Variadic => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub label: String,
    pub inst: Vec<Statement>,
    pub jump: Option<JumpInstr>,
}

#[derive(Debug, PartialEq)]
pub enum Value {
    LocalVar(String),
    Const(DynConst),
}

#[derive(Debug, PartialEq)]
pub enum JumpInstr {
    Jump(String),               // jmp
    Jnz(Value, String, String), // jnz
    Return(Option<Value>),      // ret
    Halt,                       // hlt
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CmpOp {
    Eq,
    Ne,
    Sle,
    Slt,
    Sge,
    Sgt,
    Ule,
    Ult,
    Uge,
    Ugt,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum AllocAlign {
    Word,     // Align on 4-byte boundary
    Long,     // Align on 8-byte boundary
    LongLong, // Align on 16-byte boundary
}

impl AllocAlign {
    pub fn byte_align(&self) -> u8 {
        match self {
            AllocAlign::Word => 4,
            AllocAlign::Long => 8,
            AllocAlign::LongLong => 16,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Instr {
    // Arithmetic and Bits
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Neg(Value),
    UDiv(Value, Value),
    Rem(Value, Value),
    URem(Value, Value),
    Or(Value, Value),
    Xor(Value, Value),
    And(Value, Value),
    Sar(Value, Value),
    Shr(Value, Value),
    Shl(Value, Value),

    // Memory
    Load(LoadType, Value),

    // Stack Allocation
    Alloc(AllocAlign, u64),

    // Comparision
    Compare(BaseType, CmpOp, Value, Value),

    // Conversions
    Ext(SubLongType, Value),
}

#[derive(Debug, PartialEq)]
pub enum VolatileInstr {
    Store(ExtType, Value, Value),
    Blit(Value, Value, u64),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Assign(String, BaseType, Instr),
    Call(String, Type, String, Vec<FuncParam>),
    Volatile(VolatileInstr),
}
