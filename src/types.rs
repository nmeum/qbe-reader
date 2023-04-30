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

#[derive(Debug, PartialEq)]
pub enum SubType {
    ExtType(ExtType),
    UserDef(String),
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Base(BaseType),
    SubWordType(SubWordType),
    Ident(String),
}

#[derive(Debug, PartialEq)]
pub enum Const {
    Number(i64),
    SFP(f32),
    DFP(f64),
    Ident(String),
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
    pub linkage: Linkage,
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

#[derive(Debug, PartialEq)]
pub struct Block {
    pub label: String,
    pub jump: Instr,
}

#[derive(Debug, PartialEq)]
pub enum Instr {
    Jump(String), // jmp
    Halt,         // hlt
}