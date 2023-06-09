use crate::types::*;
use crate::util::*;
use std::collections::HashMap;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, none_of},
    combinator::{map_res, opt, recognize},
    error::{FromExternalError, ParseError},
    multi::{many0, many0_count, many1, separated_list1},
    number::complete::{double, float},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

// STRING := '"' .... '"'
fn string(input: &str) -> IResult<&str, String> {
    // TODO: Does qbe support escaping in strings?
    map_res(
        delimited(char('"'), recognize(many0(none_of("\""))), char('"')),
        |s| -> Result<String, ()> { Ok(String::from(s)) },
    )(input)
}

// See https://c9x.me/git/qbe.git/tree/parse.c?h=v1.1#n302
fn ident(input: &str) -> IResult<&str, String> {
    map_res(
        recognize(pair(
            alt((alpha1, tag("."), tag("_"))),
            many0_count(alt((alphanumeric1, tag("$"), tag("."), tag("_")))),
        )),
        |s| -> Result<String, ()> { Ok(String::from(s)) },
    )(input)
}

// See https://c9x.me/compile/doc/il-v1.1.html#Sigils
fn global(input: &str) -> IResult<&str, String> {
    preceded(char('$'), ident)(input)
}
fn userdef(input: &str) -> IResult<&str, String> {
    preceded(char(':'), ident)(input)
}
fn local(input: &str) -> IResult<&str, String> {
    preceded(char('%'), ident)(input)
}
fn label(input: &str) -> IResult<&str, String> {
    preceded(char('@'), ident)(input)
}

////////////////////////////////////////////////////////////////////////

// BASETY := 'w' | 'l' | 's' | 'd'
fn base_type(input: &str) -> IResult<&str, BaseType> {
    alt((
        bind(char('w'), BaseType::Word),
        bind(char('l'), BaseType::Long),
        bind(char('s'), BaseType::Single),
        bind(char('d'), BaseType::Double),
    ))(input)
}

// EXTTY := BASETY | 'b' | 'h'
fn ext_type(input: &str) -> IResult<&str, ExtType> {
    alt((
        bind(char('b'), ExtType::Byte),
        bind(char('h'), ExtType::Halfword),
        map_res(base_type, |ty| -> Result<ExtType, ()> {
            Ok(ExtType::Base(ty))
        }),
    ))(input)
}

// CONST :=
//     ['-'] NUMBER  # Decimal integer
//   | 's_' FP       # Single-precision float
//   | 'd_' FP       # Double-precision float
//   | $IDENT        # Global symbol
fn constant(input: &str) -> IResult<&str, Const> {
    // TODO: Ensure that floating point parser matches what QBE's parser does.
    // See: https://c9x.me/git/qbe.git/tree/parse.c?h=v1.1#n245
    alt((
        map_res(
            tuple((opt(char('-')), ws(parse_i64))),
            |(pfx, n)| -> Result<Const, ()> {
                match pfx {
                    Some(_) => Ok(Const::Number(-n)),
                    None => Ok(Const::Number(n)),
                }
            },
        ),
        preceded(
            tag("s_"),
            map_res(float, |f| -> Result<Const, ()> { Ok(Const::SFP(f)) }),
        ),
        preceded(
            tag("d_"),
            map_res(double, |f| -> Result<Const, ()> { Ok(Const::DFP(f)) }),
        ),
        map_res(global, |s| -> Result<Const, ()> { Ok(Const::Global(s)) }),
    ))(input)
}

// DYNCONST :=
//     CONST
//   | 'thread' $IDENT  # Thread-local symbol
fn dynconstant(input: &str) -> IResult<&str, DynConst> {
    alt((
        map_res(constant, |c| -> Result<DynConst, ()> {
            Ok(DynConst::Const(c))
        }),
        map_res(
            preceded(tag("thread"), ws(global)),
            |i| -> Result<DynConst, ()> { Ok(DynConst::Thread(i)) },
        ),
    ))(input)
}

// LINKAGE :=
//     'export'
//   | 'thread'
//   | 'section' SECNAME
//   | 'section' SECNAME SECFLAGS
//
// SECNAME  := '"' .... '"'
// SECFLAGS := '"' .... '"'
fn _linkage(input: &str) -> IResult<&str, Linkage> {
    alt((
        map_res(tag("export"), |_| -> Result<Linkage, ()> {
            Ok(Linkage::Export)
        }),
        map_res(tag("thread"), |_| -> Result<Linkage, ()> {
            Ok(Linkage::Thread)
        }),
        map_res(
            tuple((tag("section"), ws(string), opt(string))),
            |(_, secname, secflags)| -> Result<Linkage, ()> {
                Ok(Linkage::Section(secname, secflags))
            },
        ),
    ))(input)
}

// Parse linkage terminated with one or more newline characters.
fn linkage(input: &str) -> IResult<&str, Linkage> {
    terminated(ws(_linkage), newline0)(input)
}

// TYPEDEF := REGTY | OPAQUETY
pub fn typedef(input: &str) -> IResult<&str, TypeDef> {
    alt((regty, opaque))(input)
}

// REGTY :=
//     'type' :IDENT '=' ['align' NUMBER]
//     '{'
//         ( SUBTY [NUMBER] ),
//     '}'
fn regty(input: &str) -> IResult<&str, TypeDef> {
    map_res(
        tuple((
            tag("type"),
            ws(userdef),
            char('='),
            opt(preceded(ws(tag("align")), parse_u64)),
            delimited(ws(char('{')), members, ws(char('}'))),
        )),
        |(_, id, _, align, members)| -> Result<TypeDef, ()> {
            Ok(TypeDef {
                name: id,
                defn: AggregateType::Regular(align, members),
            })
        },
    )(input)
}

// SUBTY := EXTTY | :IDENT
fn subty(input: &str) -> IResult<&str, SubType> {
    alt((
        map_res(ext_type, |ty| -> Result<SubType, ()> {
            Ok(SubType::ExtType(ty))
        }),
        map_res(userdef, |id| -> Result<SubType, ()> {
            Ok(SubType::UserDef(id))
        }),
    ))(input)
}

// MEMBERS := ( MEMBER ),
fn members(input: &str) -> IResult<&str, Vec<(SubType, u64)>> {
    separated_list1(ws(char(',')), member)(input)
}

// MEMBER := SUBTY [NUMBER]
fn member(input: &str) -> IResult<&str, (SubType, u64)> {
    map_res(
        pair(subty, opt(ws(parse_u64))),
        |(ty, n)| -> Result<(SubType, u64), ()> { Ok((ty, n.unwrap_or(1))) },
    )(input)
}

// OPAQUETY := 'type' :IDENT '=' 'align' NUMBER '{' NUMBER '}'
fn opaque(input: &str) -> IResult<&str, TypeDef> {
    map_res(
        tuple((
            tag("type"),
            ws(userdef),
            char('='),
            ws(tag("align")),
            parse_u64,
            delimited(ws(char('{')), ws(parse_u64), char('}')),
        )),
        |(_, id, _, _, size, align)| -> Result<TypeDef, ()> {
            Ok(TypeDef {
                name: id,
                defn: AggregateType::Opaque(size, align),
            })
        },
    )(input)
}

// DATADEF :=
//     LINKAGE*
//     'data' $IDENT '=' ['align' NUMBER]
//     '{'
//         ( EXTTY DATAITEM+
//         | 'z'   NUMBER ),
//     '}'
pub fn datadef(input: &str) -> IResult<&str, DataDef> {
    map_res(
        tuple((
            many0(ws(linkage)),
            ws(tag("data")),
            global,
            ws(char('=')),
            opt(preceded(tag("align"), parse_u64)),
            delimited(ws(char('{')), data_objs, char('}')),
        )),
        |(lnk, _, ident, _, align, objs)| -> Result<DataDef, ()> {
            Ok(DataDef {
                linkage: lnk,
                name: ident,
                align: align,
                objs: objs,
            })
        },
    )(input)
}

// DATAOBJS := ( DATAOBJ ),
fn data_objs(input: &str) -> IResult<&str, Vec<DataObj>> {
    separated_list1(ws(char(',')), data_obj)(input)
}

// DATAOBJ := EXTTY DATAITEM+
//          | 'z'   NUMBER
fn data_obj(input: &str) -> IResult<&str, DataObj> {
    alt((
        map_res(
            pair(ext_type, many1(ws(data_item))),
            |(ty, items)| -> Result<DataObj, ()> { Ok(DataObj::DataItem(ty, items)) },
        ),
        map_res(
            preceded(char('z'), ws(parse_u64)),
            |n| -> Result<DataObj, ()> { Ok(DataObj::ZeroFill(n)) },
        ),
    ))(input)
}

// DATAITEM :=
//     $IDENT ['+' NUMBER]  # Symbol and offset
//   |  '"' ... '"'         # String
//   |  CONST               # Constant
fn data_item(input: &str) -> IResult<&str, DataItem> {
    alt((
        map_res(
            pair(global, opt(preceded(opt(ws(char('+'))), ws(parse_u64)))),
            |(ident, off)| -> Result<DataItem, ()> { Ok(DataItem::Symbol(ident, off)) },
        ),
        map_res(string, |s| -> Result<DataItem, ()> {
            Ok(DataItem::String(s))
        }),
        map_res(constant, |c| -> Result<DataItem, ()> {
            Ok(DataItem::Const(c))
        }),
    ))(input)
}

// FUNCDEF :=
//     LINKAGE*
//     'function' [ABITY] $IDENT PARAMS [NL]
//     '{' NL
//         BODY
//     '}'
pub fn funcdef(input: &str) -> IResult<&str, FuncDef> {
    map_res(
        tuple((
            many0(ws(linkage)),
            ws(tag("function")),
            opt(abity),
            ws(global),
            params,
            ws(newline0),
            delimited(ws(char('{')), preceded(newline1, body), char('}')),
        )),
        |(lnk, _, ret, id, params, _, body)| -> Result<FuncDef, ()> {
            Ok(FuncDef {
                linkage: lnk,
                name: id,
                abity: ret,
                params: params,
                body: body,
            })
        },
    )(input)
}

// SUBWTY := 'sb' | 'ub' | 'sh' | 'uh'
fn sub_word(input: &str) -> IResult<&str, SubWordType> {
    alt((
        str("sb", SubWordType::SignedByte),
        str("ub", SubWordType::UnsignedHalf),
        str("sh", SubWordType::SignedHalf),
        str("uh", SubWordType::UnsignedHalf),
    ))(input)
}

fn sub_long(input: &str) -> IResult<&str, SubLongType> {
    alt((
        str("uw", SubLongType::UnsignedWord),
        str("sw", SubLongType::SignedWord),
        map_res(sub_word, |ty| -> Result<SubLongType, ()> {
            Ok(SubLongType::SubWord(ty))
        }),
    ))(input)
}

// See https://c9x.me/compile/doc/il-v1.1.html#Memory
fn load_type(input: &str) -> IResult<&str, LoadType> {
    alt((
        map_res(sub_long, |ty| -> Result<LoadType, ()> {
            Ok(LoadType::SubLong(ty))
        }),
        map_res(base_type, |ty| -> Result<LoadType, ()> {
            Ok(LoadType::Base(ty))
        }),
    ))(input)
}

// ABITY  := BASETY | SUBWTY | :IDENT
fn abity(input: &str) -> IResult<&str, Type> {
    alt((
        map_res(base_type, |ty| -> Result<Type, ()> { Ok(Type::Base(ty)) }),
        map_res(sub_word, |ty| -> Result<Type, ()> {
            Ok(Type::SubWordType(ty))
        }),
        map_res(userdef, |str| -> Result<Type, ()> {
            Ok(Type::UserDef(str))
        }),
    ))(input)
}

// PARAM :=
//     ABITY %IDENT  # Regular parameter
//   | 'env' %IDENT  # Environment parameter (first)
//   | '...'         # Variadic marker (last)
fn param(input: &str) -> IResult<&str, FuncParam> {
    // TODO: Ensure that env is the first parameter
    // TODO: Ensure that variadic marker is the last parameter
    alt((
        map_res(
            pair(abity, ws(local)),
            |(ty, id)| -> Result<FuncParam, ()> { Ok(FuncParam::Regular(ty, id)) },
        ),
        map_res(
            preceded(tag("env"), ws(local)),
            |id| -> Result<FuncParam, ()> { Ok(FuncParam::Env(id)) },
        ),
        map_res(tag("..."), |_| -> Result<FuncParam, ()> {
            Ok(FuncParam::Variadic)
        }),
    ))(input)
}

// PARAMS = '(' (PARAM), ')'
fn _params(input: &str) -> IResult<&str, Vec<FuncParam>> {
    separated_list1(ws(char(',')), param)(input)
}
fn params(input: &str) -> IResult<&str, Vec<FuncParam>> {
    delimited(char('('), ws(_params), char(')'))(input)
}

// BODY = BLOCK+
fn body(input: &str) -> IResult<&str, Vec<Block>> {
    many1(block)(input)
}

// BLOCK :=
//    @IDENT NL     # Block label
//    ( PHI NL )*   # Phi instructions
//    ( INST NL )*  # Regular instructions
//    JUMP NL       # Jump or return
fn block(input: &str) -> IResult<&str, Block> {
    map_res(
        tuple((
            terminated(label, ws(newline1)),
            many0(terminated(phi, ws(newline1))),
            many0(terminated(stat, ws(newline1))),
            opt(terminated(jump, ws(newline1))),
        )),
        |(label, phis, inst, jump)| -> Result<Block, ()> {
            Ok(Block {
                label: label,
                phi: phis,
                inst: inst,
                jump: jump,
            })
        },
    )(input)
}

// JUMP :=
//     'jmp' @IDENT               # Unconditional
//   | 'jnz' VAL, @IDENT, @IDENT  # Conditional
//   | 'ret' [VAL]                # Return
//   | 'hlt'                      # Termination
fn jump(input: &str) -> IResult<&str, JumpInstr> {
    alt((
        map_res(
            preceded(tag("jmp"), ws(label)),
            |l| -> Result<JumpInstr, ()> { Ok(JumpInstr::Jump(l)) },
        ),
        map_res(
            tuple((
                tag("jnz"),
                ws(value),
                char(','),
                ws(label),
                char(','),
                ws(label),
            )),
            |(_, v, _, l1, _, l2)| -> Result<JumpInstr, ()> { Ok(JumpInstr::Jnz(v, l1, l2)) },
        ),
        map_res(
            preceded(tag("ret"), opt(ws(value))),
            |v| -> Result<JumpInstr, ()> { Ok(JumpInstr::Return(v)) },
        ),
        map_res(tag("hlt"), |_| -> Result<JumpInstr, ()> {
            Ok(JumpInstr::Halt)
        }),
    ))(input)
}

// TODO: This is not documented in the BNF grammar.
fn value(input: &str) -> IResult<&str, Value> {
    alt((
        map_res(local, |var| -> Result<Value, ()> {
            Ok(Value::LocalVar(var))
        }),
        map_res(dynconstant, |cnst| -> Result<Value, ()> {
            Ok(Value::Const(cnst))
        }),
    ))(input)
}

// PHI_LABELS := 'phi' ( @IDENT VAL ),
pub fn phi_labels(input: &str) -> IResult<&str, HashMap<String, Value>> {
    map_res(
        preceded(
            ws(tag("phi")),
            separated_list1(ws(char(',')), pair(label, ws(value))),
        ),
        |xs: Vec<(String, Value)>| -> Result<HashMap<String, Value>, ()> {
            Ok(HashMap::from_iter(xs.into_iter()))
        },
    )(input)
}

// PHI := %IDENT '=' BASETY PHI_LABELS
pub fn phi(input: &str) -> IResult<&str, Phi> {
    map_res(
        tuple((local, ws(char('=')), base_type, ws(phi_labels))),
        |(dest, _, ty, labels)| -> Result<Phi, ()> {
            Ok(Phi {
                ident: dest,
                base_type: ty,
                labels,
            })
        },
    )(input)
}

// See https://c9x.me/compile/doc/il-v1.1.html#Comparisons
pub fn compare_op(input: &str) -> IResult<&str, CmpOp> {
    // TODO: Extend this for floating point comparisons.
    alt((
        str("eq", CmpOp::Eq),
        str("ne", CmpOp::Ne),
        str("sle", CmpOp::Sle),
        str("slt", CmpOp::Slt),
        str("sge", CmpOp::Sge),
        str("sgt", CmpOp::Sgt),
        str("ule", CmpOp::Ule),
        str("ult", CmpOp::Ult),
        str("uge", CmpOp::Uge),
        str("ugt", CmpOp::Ugt),
    ))(input)
}

fn instr_tuple<'a, F: 'a, O, E: ParseError<&'a str> + FromExternalError<&'a str, ()>>(
    name: &'a str,
    arg1: F,
    arg2: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, (O, O), E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    pair(preceded(tag(name), ws(arg1)), preceded(char(','), ws(arg2)))
}

macro_rules! make_instr {
    { $NAME:expr, $CONS:expr } => {
        map_res(
            instr_tuple($NAME, value, value),
            |(v1, v2)| -> Result<Instr, ()> {
                Ok($CONS(v1, v2))
            },
        )
    };
}

// ALLOC_ALIGN = 4 | 8 | 16
fn alloc_align(input: &str) -> IResult<&str, AllocAlign> {
    alt((
        str("4", AllocAlign::Word),
        str("8", AllocAlign::Long),
        str("16", AllocAlign::LongLong),
    ))(input)
}

// See https://c9x.me/compile/doc/il-v1.1.html#Instructions
fn instr(input: &str) -> IResult<&str, Instr> {
    alt((
        make_instr!("add", Instr::Add),
        make_instr!("sub", Instr::Sub),
        make_instr!("mul", Instr::Mul),
        map_res(preceded(tag("neg"), ws(value)), |v| -> Result<Instr, ()> {
            Ok(Instr::Neg(v))
        }),
        make_instr!("udiv", Instr::UDiv),
        make_instr!("rem", Instr::Rem),
        make_instr!("urem", Instr::URem),
        make_instr!("or", Instr::Or),
        make_instr!("xor", Instr::Xor),
        make_instr!("and", Instr::And),
        make_instr!("sar", Instr::Sar),
        make_instr!("shr", Instr::Shr),
        make_instr!("shl", Instr::Shl),
        map_res(
            pair(preceded(tag("load"), load_type), ws(value)),
            |(ty, addr)| -> Result<Instr, ()> { Ok(Instr::Load(ty, addr)) },
        ),
        map_res(
            pair(preceded(tag("alloc"), alloc_align), ws(parse_u64)),
            |(align, amount)| -> Result<Instr, ()> { Ok(Instr::Alloc(align, amount)) },
        ),
        map_res(
            tuple((
                char('c'),
                compare_op,
                base_type,
                ws(value),
                char(','),
                ws(value),
            )),
            |(_, op, ty, v1, _, v2)| -> Result<Instr, ()> { Ok(Instr::Compare(ty, op, v1, v2)) },
        ),
        map_res(
            tuple((tag("ext"), sub_long, ws(value))),
            |(_, ty, v)| -> Result<Instr, ()> { Ok(Instr::Ext(ty, v)) },
        ),
    ))(input)
}

// TODO: This is not documented in the BNF grammar.
fn stat(input: &str) -> IResult<&str, Statement> {
    alt((
        map_res(
            tuple((local, ws(char('=')), base_type, ws(instr))),
            |(dest, _, ty, inst)| -> Result<Statement, ()> {
                Ok(Statement::Assign(dest, ty, inst))
            },
        ),
        map_res(
            tuple((
                preceded(tag("store"), ext_type),
                ws(value),
                char(','),
                ws(value),
            )),
            |(ty, value, _, addr)| -> Result<Statement, ()> {
                let instr = VolatileInstr::Store(ty, value, addr);
                Ok(Statement::Volatile(instr))
            },
        ),
        map_res(
            tuple((
                tag("blit"),
                ws(value),
                char(','),
                ws(value),
                char(','),
                ws(parse_u64),
            )),
            |(_, src, _, dst, _, n)| -> Result<Statement, ()> {
                let instr = VolatileInstr::Blit(src, dst, n);
                Ok(Statement::Volatile(instr))
            },
        ),
        map_res(
            tuple((
                local,
                ws(char('=')),
                abity,
                ws(tag("call")),
                global,
                ws(params),
            )),
            |(dest, _, ty, _, fname, params)| -> Result<Statement, ()> {
                Ok(Statement::Call(dest, ty, fname, params))
            },
        ),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string() {
        assert_eq!(string("\"foobar\""), Ok(("", String::from("foobar"))));
        assert_eq!(string("\"\""), Ok(("", String::from(""))));
    }

    #[test]
    fn test_linkage() {
        // Primitive linkage directives
        assert_eq!(linkage("export"), Ok(("", Linkage::Export)));
        assert_eq!(linkage("thread"), Ok(("", Linkage::Thread)));

        // Section linkage directive
        assert_eq!(
            linkage("section \"foobar\"\n"),
            Ok(("", Linkage::Section(String::from("foobar"), None)))
        );
        assert_eq!(
            linkage(" section \"foo\"	\"bar\" \n"),
            Ok((
                "",
                Linkage::Section(String::from("foo"), Some(String::from("bar")))
            ))
        );
    }

    #[test]
    fn test_types() {
        // Base types
        assert_eq!(base_type("w"), Ok(("", BaseType::Word)));
        assert_eq!(base_type("l"), Ok(("", BaseType::Long)));
        assert_eq!(base_type("s"), Ok(("", BaseType::Single)));
        assert_eq!(base_type("d"), Ok(("", BaseType::Double)));

        // Extented types
        assert_eq!(ext_type("s"), Ok(("", ExtType::Base(BaseType::Single))));
        assert_eq!(ext_type("h"), Ok(("", ExtType::Halfword)));
    }

    #[test]
    fn test_ident() {
        assert_eq!(ident("_foo"), Ok(("", String::from("_foo"))));
        assert_eq!(ident("foobar"), Ok(("", String::from("foobar"))));
        assert_eq!(ident("foo$_.23__"), Ok(("", String::from("foo$_.23__"))));
    }

    #[test]
    fn test_constant() {
        // Number
        assert_eq!(constant("23"), Ok(("", Const::Number(23))));
        assert_eq!(constant("-5"), Ok(("", Const::Number(-5))));

        // Identifier
        assert_eq!(
            constant("$foobar"),
            Ok(("", Const::Global(String::from("foobar"))))
        );
        assert_eq!(
            constant("$_fo$$r"),
            Ok(("", Const::Global(String::from("_fo$$r"))))
        );

        // Float
        assert_eq!(constant("s_23.42"), Ok(("", Const::SFP(23.42))));
        assert_eq!(constant("s_23."), Ok(("", Const::SFP(23.))));

        // Double
        assert_eq!(constant("d_11e-1"), Ok(("", Const::DFP(1.1))));
    }

    #[test]
    fn test_dynconst() {
        // Constant
        assert_eq!(
            dynconstant("-   23"),
            Ok(("", DynConst::Const(Const::Number(-23))))
        );
        assert_eq!(
            dynconstant("234223422342"),
            Ok(("", DynConst::Const(Const::Number(234223422342))))
        );

        // Thread-local symbol
        assert_eq!(
            dynconstant("thread $foo"),
            Ok(("", DynConst::Thread(String::from("foo"))))
        );
        assert_eq!(
            dynconstant("thread	$foo"),
            Ok(("", DynConst::Thread(String::from("foo"))))
        );
    }

    #[test]
    fn test_members() {
        assert_eq!(
            members("s 23"),
            Ok((
                "",
                vec![(SubType::ExtType(ExtType::Base(BaseType::Single)), 23)]
            ))
        );

        assert_eq!(
            members("s, s, d, d"),
            Ok((
                "",
                vec![
                    (SubType::ExtType(ExtType::Base(BaseType::Single)), 1),
                    (SubType::ExtType(ExtType::Base(BaseType::Single)), 1),
                    (SubType::ExtType(ExtType::Base(BaseType::Double)), 1),
                    (SubType::ExtType(ExtType::Base(BaseType::Double)), 1),
                ]
            ))
        );
    }

    #[test]
    fn test_aggregate() {
        // TODO: Expand
        assert_eq!(
            typedef("type :fourfloats = { s, s, d, d }"),
            Ok((
                "",
                TypeDef {
                    name: String::from("fourfloats"),
                    defn: AggregateType::Regular(
                        None,
                        vec![
                            (SubType::ExtType(ExtType::Base(BaseType::Single)), 1),
                            (SubType::ExtType(ExtType::Base(BaseType::Single)), 1),
                            (SubType::ExtType(ExtType::Base(BaseType::Double)), 1),
                            (SubType::ExtType(ExtType::Base(BaseType::Double)), 1),
                        ]
                    )
                }
            ))
        );

        assert_eq!(
            typedef("type :opaque = align 16 { 32 }"),
            Ok((
                "",
                TypeDef {
                    name: String::from("opaque"),
                    defn: AggregateType::Opaque(16, 32),
                }
            ))
        );
    }

    #[test]
    fn test_dataitm() {
        // Symbolic with and without offset
        assert_eq!(
            data_item("$foo"),
            Ok(("", DataItem::Symbol(String::from("foo"), None)))
        );
        assert_eq!(
            data_item("$foo +23"),
            Ok(("", DataItem::Symbol(String::from("foo"), Some(23))))
        );
        assert_eq!(
            data_item("$foo   +  23"),
            Ok(("", DataItem::Symbol(String::from("foo"), Some(23))))
        );

        // String
        assert_eq!(
            data_item("\"test\""),
            Ok(("", DataItem::String(String::from("test"))))
        );
        assert_eq!(
            data_item("\" f o o\""),
            Ok(("", DataItem::String(String::from(" f o o"))))
        );

        // Constant
        assert_eq!(
            data_item("23"),
            Ok(("", DataItem::Const(Const::Number(23))))
        );
        assert_eq!(
            data_item("s_42"),
            Ok(("", DataItem::Const(Const::SFP(42.))))
        );
    }

    #[test]
    fn test_dataobj() {
        // TODO: Expand
        assert_eq!(
            data_obj("h 23"),
            Ok((
                "",
                DataObj::DataItem(ExtType::Halfword, vec![DataItem::Const(Const::Number(23))])
            ))
        );
        assert_eq!(data_obj("z   42"), Ok(("", DataObj::ZeroFill(42))));
    }

    #[test]
    fn test_dataobjs() {
        // TODO: Expand
        assert_eq!(data_objs("z 1337"), Ok(("", vec![DataObj::ZeroFill(1337)])));
        assert_eq!(
            data_objs("z 42, z 10"),
            Ok(("", vec![DataObj::ZeroFill(42), DataObj::ZeroFill(10)]))
        );
    }

    #[test]
    fn test_datadef() {
        let input = "data $b = { z 1000 }";
        let items = vec![DataObj::ZeroFill(1000)];
        assert_eq!(
            datadef(input),
            Ok((
                "",
                DataDef {
                    linkage: vec![],
                    name: String::from("b"),
                    align: None,
                    objs: items
                }
            ))
        );

        let input = "data $c = { l -1 2, l $c }";
        let items = vec![
            DataObj::DataItem(
                ExtType::Base(BaseType::Long),
                vec![
                    DataItem::Const(Const::Number(-1)),
                    DataItem::Const(Const::Number(2)),
                ],
            ),
            DataObj::DataItem(
                ExtType::Base(BaseType::Long),
                vec![DataItem::Symbol(String::from("c"), None)],
            ),
        ];
        assert_eq!(
            datadef(input),
            Ok((
                "",
                DataDef {
                    linkage: vec![],
                    name: String::from("c"),
                    align: None,
                    objs: items
                }
            ))
        );
    }

    #[test]
    fn test_block() {
        assert_eq!(
            block("@start\nhlt\n"),
            Ok((
                "",
                Block {
                    label: String::from("start"),
                    phi: vec![],
                    inst: vec![],
                    jump: Some(JumpInstr::Halt),
                }
            ))
        );

        assert_eq!(
            block("@start\n"),
            Ok((
                "",
                Block {
                    label: String::from("start"),
                    phi: vec![],
                    inst: vec![],
                    jump: None,
                }
            ))
        );

        assert_eq!(
            block("@.1\n%.10 =w phi @body.10 0, @logic_right.11 %foo\nret\n"),
            Ok((
                "",
                Block {
                    label: String::from(".1"),
                    phi: vec![Phi {
                        ident: String::from(".10"),
                        base_type: BaseType::Word,
                        labels: HashMap::from([
                            (
                                String::from("body.10"),
                                Value::Const(DynConst::Const(Const::Number(0)))
                            ),
                            (
                                String::from("logic_right.11"),
                                Value::LocalVar(String::from("foo")),
                            )
                        ])
                    }],
                    inst: vec![],
                    jump: Some(JumpInstr::Return(None)),
                }
            ))
        );
    }

    #[test]
    fn test_stat() {
        assert_eq!(
            stat("%c =w add %a, %b"),
            Ok((
                "",
                Statement::Assign(
                    String::from("c"),
                    BaseType::Word,
                    Instr::Add(
                        Value::LocalVar(String::from("a")),
                        Value::LocalVar(String::from("b"))
                    )
                )
            ))
        );

        assert_eq!(
            stat("%r =w ceqw %o1, %o2"),
            Ok((
                "",
                Statement::Assign(
                    String::from("r"),
                    BaseType::Word,
                    Instr::Compare(
                        BaseType::Word,
                        CmpOp::Eq,
                        Value::LocalVar(String::from("o1")),
                        Value::LocalVar(String::from("o2"))
                    )
                )
            ))
        );

        assert_eq!(
            stat("blit %s0, %s1, 8"),
            Ok((
                "",
                Statement::Volatile(VolatileInstr::Blit(
                    Value::LocalVar(String::from("s0")),
                    Value::LocalVar(String::from("s1")),
                    8
                ))
            ))
        );

        assert_eq!(
            stat("%r =s call $myfunc(s %a, l %ap)"),
            Ok((
                "",
                Statement::Call(
                    String::from("r"),
                    Type::Base(BaseType::Single),
                    String::from("myfunc"),
                    vec![
                        FuncParam::Regular(Type::Base(BaseType::Single), String::from("a")),
                        FuncParam::Regular(Type::Base(BaseType::Long), String::from("ap")),
                    ],
                )
            ))
        );
    }

    #[test]
    fn test_funcdef() {
        let input = "export function w $getone(:one %p) {\n@start\nhlt\n}";
        assert_eq!(
            funcdef(input),
            Ok((
                "",
                FuncDef {
                    linkage: vec![Linkage::Export],
                    name: String::from("getone"),
                    abity: Some(Type::Base(BaseType::Word)),
                    params: vec![FuncParam::Regular(
                        Type::UserDef(String::from("one")),
                        String::from("p")
                    )],
                    body: vec![Block {
                        label: String::from("start"),
                        phi: vec![],
                        inst: vec![],
                        jump: Some(JumpInstr::Halt),
                    }]
                }
            ))
        );
    }
}
