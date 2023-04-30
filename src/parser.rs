use crate::types::*;
use crate::util::*;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, none_of},
    combinator::{map_res, opt, recognize},
    multi::{many0, many0_count, many1, separated_list1},
    number::complete::{double, float},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

// STRING := '"' .... '"'
fn parse_string(input: &str) -> IResult<&str, String> {
    // TODO: Does qbe support escaping in strings?
    map_res(
        delimited(char('"'), recognize(many0(none_of("\""))), char('"')),
        |s| -> Result<String, ()> { Ok(String::from(s)) },
    )(input)
}

// See https://c9x.me/git/qbe.git/tree/parse.c?h=v1.1#n302
fn parse_ident(input: &str) -> IResult<&str, String> {
    map_res(
        recognize(pair(
            alt((alpha1, tag("."), tag("_"))),
            many0_count(alt((alphanumeric1, tag("$"), tag("."), tag("_")))),
        )),
        |s| -> Result<String, ()> { Ok(String::from(s)) },
    )(input)
}

// See https://c9x.me/compile/doc/il-v1.1.html#Sigils
fn parse_global(input: &str) -> IResult<&str, String> {
    preceded(char('$'), parse_ident)(input)
}
fn parse_userdef(input: &str) -> IResult<&str, String> {
    preceded(char(':'), parse_ident)(input)
}
fn parse_local(input: &str) -> IResult<&str, String> {
    preceded(char('%'), parse_ident)(input)
}
fn parse_label(input: &str) -> IResult<&str, String> {
    preceded(char('@'), parse_ident)(input)
}

////////////////////////////////////////////////////////////////////////

// BASETY := 'w' | 'l' | 's' | 'd'
pub fn parse_base_type(input: &str) -> IResult<&str, BaseType> {
    alt((
        bind(char('w'), BaseType::Word),
        bind(char('l'), BaseType::Long),
        bind(char('s'), BaseType::Single),
        bind(char('d'), BaseType::Double),
    ))(input)
}

// EXTTY := BASETY | 'b' | 'h'
pub fn parse_ext_type(input: &str) -> IResult<&str, ExtType> {
    alt((
        bind(char('b'), ExtType::Byte),
        bind(char('h'), ExtType::Halfword),
        map_res(parse_base_type, |ty| -> Result<ExtType, ()> {
            Ok(ExtType::Base(ty))
        }),
    ))(input)
}

// CONST :=
//     ['-'] NUMBER  # Decimal integer
//   | 's_' FP       # Single-precision float
//   | 'd_' FP       # Double-precision float
//   | $IDENT        # Global symbol
pub fn parse_const(input: &str) -> IResult<&str, Const> {
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
        map_res(parse_global, |s| -> Result<Const, ()> {
            Ok(Const::Ident(s))
        }),
    ))(input)
}

// DYNCONST :=
//     CONST
//   | 'thread' $IDENT  # Thread-local symbol
pub fn parse_dynconst(input: &str) -> IResult<&str, DynConst> {
    alt((
        map_res(parse_const, |c| -> Result<DynConst, ()> {
            Ok(DynConst::Const(c))
        }),
        map_res(
            preceded(tag("thread"), ws(parse_global)),
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
pub fn _parse_linkage(input: &str) -> IResult<&str, Linkage> {
    alt((
        map_res(tag("export"), |_| -> Result<Linkage, ()> {
            Ok(Linkage::Export)
        }),
        map_res(tag("thread"), |_| -> Result<Linkage, ()> {
            Ok(Linkage::Thread)
        }),
        map_res(
            tuple((tag("section"), ws(parse_string), opt(parse_string))),
            |(_, secname, secflags)| -> Result<Linkage, ()> {
                Ok(Linkage::Section(secname, secflags))
            },
        ),
    ))(input)
}

// Parse linkage terminated with one or more newline characters.
pub fn parse_linkage(input: &str) -> IResult<&str, Linkage> {
    terminated(ws(_parse_linkage), newline0)(input)
}

// TYPEDEF := REGTY | OPAQUETY
pub fn parse_typedef(input: &str) -> IResult<&str, TypeDef> {
    alt((parse_regty, parse_opaque))(input)
}

// REGTY :=
//     'type' :IDENT '=' ['align' NUMBER]
//     '{'
//         ( SUBTY [NUMBER] ),
//     '}'
pub fn parse_regty(input: &str) -> IResult<&str, TypeDef> {
    map_res(
        tuple((
            tag("type"),
            ws(parse_userdef),
            char('='),
            opt(preceded(ws(tag("align")), parse_u64)),
            delimited(ws(char('{')), parse_members, ws(char('}'))),
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
pub fn parse_subty(input: &str) -> IResult<&str, SubType> {
    alt((
        map_res(parse_ext_type, |ty| -> Result<SubType, ()> {
            Ok(SubType::ExtType(ty))
        }),
        map_res(parse_userdef, |id| -> Result<SubType, ()> {
            Ok(SubType::UserDef(id))
        }),
    ))(input)
}

// MEMBERS := ( MEMBER ),
pub fn parse_members(input: &str) -> IResult<&str, Vec<(SubType, u64)>> {
    separated_list1(ws(char(',')), parse_member)(input)
}

// MEMBER := SUBTY [NUMBER]
pub fn parse_member(input: &str) -> IResult<&str, (SubType, u64)> {
    map_res(
        pair(parse_subty, opt(ws(parse_u64))),
        |(ty, n)| -> Result<(SubType, u64), ()> { Ok((ty, n.unwrap_or(1))) },
    )(input)
}

// OPAQUETY := 'type' :IDENT '=' 'align' NUMBER '{' NUMBER '}'
pub fn parse_opaque(input: &str) -> IResult<&str, TypeDef> {
    map_res(
        tuple((
            tag("type"),
            ws(parse_userdef),
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
pub fn parse_data(input: &str) -> IResult<&str, DataDef> {
    map_res(
        tuple((
            many0(ws(parse_linkage)),
            ws(tag("data")),
            parse_global,
            ws(char('=')),
            opt(preceded(tag("align"), parse_u64)),
            delimited(ws(char('{')), parse_data_objs, char('}')),
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
pub fn parse_data_objs(input: &str) -> IResult<&str, Vec<DataObj>> {
    separated_list1(ws(char(',')), parse_data_obj)(input)
}

// DATAOBJ := EXTTY DATAITEM+
//          | 'z'   NUMBER
pub fn parse_data_obj(input: &str) -> IResult<&str, DataObj> {
    alt((
        map_res(
            pair(parse_ext_type, many1(ws(parse_data_item))),
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
pub fn parse_data_item(input: &str) -> IResult<&str, DataItem> {
    alt((
        map_res(
            pair(
                parse_global,
                opt(preceded(opt(ws(char('+'))), ws(parse_u64))),
            ),
            |(ident, off)| -> Result<DataItem, ()> { Ok(DataItem::Symbol(ident, off)) },
        ),
        map_res(parse_string, |s| -> Result<DataItem, ()> {
            Ok(DataItem::String(s))
        }),
        map_res(parse_const, |c| -> Result<DataItem, ()> {
            Ok(DataItem::Const(c))
        }),
    ))(input)
}

// FUNCDEF :=
//     LINKAGE*
//     'function' [ABITY] $IDENT '(' (PARAM), ')' [NL]
//     '{' NL
//         BODY
//     '}'
pub fn parse_function(input: &str) -> IResult<&str, FuncDef> {
    map_res(
        tuple((
            parse_linkage,
            ws(tag("function")),
            opt(parse_abity),
            ws(parse_global),
            delimited(char('('), ws(parse_params), char(')')),
            ws(newline0),
            delimited(ws(char('{')), preceded(newline, parse_body), char('}')),
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
pub fn parse_sub_word(input: &str) -> IResult<&str, SubWordType> {
    alt((
        bind(tag("sb"), SubWordType::SignedByte),
        bind(tag("ub"), SubWordType::UnsignedHalf),
        bind(tag("sh"), SubWordType::SignedHalf),
        bind(tag("uh"), SubWordType::UnsignedHalf),
    ))(input)
}

// ABITY  := BASETY | SUBWTY | :IDENT
pub fn parse_abity(input: &str) -> IResult<&str, Type> {
    alt((
        map_res(parse_base_type, |ty| -> Result<Type, ()> {
            Ok(Type::Base(ty))
        }),
        map_res(parse_sub_word, |ty| -> Result<Type, ()> {
            Ok(Type::SubWordType(ty))
        }),
        map_res(parse_userdef, |str| -> Result<Type, ()> {
            Ok(Type::Ident(str))
        }),
    ))(input)
}

// PARAM :=
//     ABITY %IDENT  # Regular parameter
//   | 'env' %IDENT  # Environment parameter (first)
//   | '...'         # Variadic marker (last)
pub fn parse_param(input: &str) -> IResult<&str, FuncParam> {
    alt((
        map_res(
            pair(parse_abity, ws(parse_local)),
            |(ty, id)| -> Result<FuncParam, ()> { Ok(FuncParam::Regular(ty, id)) },
        ),
        map_res(
            preceded(tag("env"), ws(parse_local)),
            |id| -> Result<FuncParam, ()> { Ok(FuncParam::Env(id)) },
        ),
        map_res(tag("..."), |_| -> Result<FuncParam, ()> {
            Ok(FuncParam::Variadic)
        }),
    ))(input)
}

// PARAMS = (PARAM),
pub fn parse_params(input: &str) -> IResult<&str, Vec<FuncParam>> {
    separated_list1(ws(char(',')), parse_param)(input)
}

// BODY = BLOCK+
pub fn parse_body(input: &str) -> IResult<&str, Vec<Block>> {
    many1(parse_block)(input)
}

// BLOCK :=
//    @IDENT NL     # Block label
//    ( PHI NL )*   # Phi instructions
//    ( INST NL )*  # Regular instructions
//    JUMP NL       # Jump or return
pub fn parse_block(input: &str) -> IResult<&str, Block> {
    map_res(
        tuple((
            terminated(parse_label, ws(newline)),
            terminated(parse_jump, ws(newline)),
        )),
        |(label, jump)| -> Result<Block, ()> {
            Ok(Block {
                label: label,
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
pub fn parse_jump(input: &str) -> IResult<&str, Instr> {
    alt((
        map_res(
            preceded(tag("jmp"), parse_label),
            |l| -> Result<Instr, ()> { Ok(Instr::Jump(l)) },
        ),
        map_res(tag("hlt"), |_| -> Result<Instr, ()> { Ok(Instr::Halt) }),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string() {
        assert_eq!(parse_string("\"foobar\""), Ok(("", String::from("foobar"))));
        assert_eq!(parse_string("\"\""), Ok(("", String::from(""))));
    }

    #[test]
    fn linkage() {
        // Primitive linkage directives
        assert_eq!(parse_linkage("export"), Ok(("", Linkage::Export)));
        assert_eq!(parse_linkage("thread"), Ok(("", Linkage::Thread)));

        // Section linkage directive
        assert_eq!(
            parse_linkage("section \"foobar\"\n"),
            Ok(("", Linkage::Section(String::from("foobar"), None)))
        );
        assert_eq!(
            parse_linkage(" section \"foo\"	\"bar\" \n"),
            Ok((
                "",
                Linkage::Section(String::from("foo"), Some(String::from("bar")))
            ))
        );
    }

    #[test]
    fn types() {
        // Base types
        assert_eq!(parse_base_type("w"), Ok(("", BaseType::Word)));
        assert_eq!(parse_base_type("l"), Ok(("", BaseType::Long)));
        assert_eq!(parse_base_type("s"), Ok(("", BaseType::Single)));
        assert_eq!(parse_base_type("d"), Ok(("", BaseType::Double)));

        // Extented types
        assert_eq!(
            parse_ext_type("s"),
            Ok(("", ExtType::Base(BaseType::Single)))
        );
        assert_eq!(parse_ext_type("h"), Ok(("", ExtType::Halfword)));
    }

    #[test]
    fn ident() {
        assert_eq!(parse_ident("_foo"), Ok(("", String::from("_foo"))));
        assert_eq!(parse_ident("foobar"), Ok(("", String::from("foobar"))));
        assert_eq!(
            parse_ident("foo$_.23__"),
            Ok(("", String::from("foo$_.23__")))
        );
    }

    #[test]
    fn constant() {
        // Number
        assert_eq!(parse_const("23"), Ok(("", Const::Number(23))));
        assert_eq!(parse_const("-5"), Ok(("", Const::Number(-5))));

        // Identifier
        assert_eq!(
            parse_const("$foobar"),
            Ok(("", Const::Ident(String::from("foobar"))))
        );
        assert_eq!(
            parse_const("$_fo$$r"),
            Ok(("", Const::Ident(String::from("_fo$$r"))))
        );

        // Float
        assert_eq!(parse_const("s_23.42"), Ok(("", Const::SFP(23.42))));
        assert_eq!(parse_const("s_23."), Ok(("", Const::SFP(23.))));

        // Double
        assert_eq!(parse_const("d_11e-1"), Ok(("", Const::DFP(1.1))));
    }

    #[test]
    fn dynamic_constant() {
        // Constant
        assert_eq!(
            parse_dynconst("-   23"),
            Ok(("", DynConst::Const(Const::Number(-23))))
        );
        assert_eq!(
            parse_dynconst("234223422342"),
            Ok(("", DynConst::Const(Const::Number(234223422342))))
        );

        // Thread-local symbol
        assert_eq!(
            parse_dynconst("thread $foo"),
            Ok(("", DynConst::Thread(String::from("foo"))))
        );
        assert_eq!(
            parse_dynconst("thread	$foo"),
            Ok(("", DynConst::Thread(String::from("foo"))))
        );
    }

    #[test]
    fn members() {
        assert_eq!(
            parse_members("s 23"),
            Ok((
                "",
                vec![(SubType::ExtType(ExtType::Base(BaseType::Single)), 23)]
            ))
        );

        assert_eq!(
            parse_members("s, s, d, d"),
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
    fn aggregate_types() {
        // TODO: Expand
        assert_eq!(
            parse_typedef("type :fourfloats = { s, s, d, d }"),
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
            parse_typedef("type :opaque = align 16 { 32 }"),
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
    fn data_item() {
        // Symbolic with and without offset
        assert_eq!(
            parse_data_item("$foo"),
            Ok(("", DataItem::Symbol(String::from("foo"), None)))
        );
        assert_eq!(
            parse_data_item("$foo +23"),
            Ok(("", DataItem::Symbol(String::from("foo"), Some(23))))
        );
        assert_eq!(
            parse_data_item("$foo   +  23"),
            Ok(("", DataItem::Symbol(String::from("foo"), Some(23))))
        );

        // String
        assert_eq!(
            parse_data_item("\"test\""),
            Ok(("", DataItem::String(String::from("test"))))
        );
        assert_eq!(
            parse_data_item("\" f o o\""),
            Ok(("", DataItem::String(String::from(" f o o"))))
        );

        // Constant
        assert_eq!(
            parse_data_item("23"),
            Ok(("", DataItem::Const(Const::Number(23))))
        );
        assert_eq!(
            parse_data_item("s_42"),
            Ok(("", DataItem::Const(Const::SFP(42.))))
        );
    }

    #[test]
    fn data_object() {
        // TODO: Expand
        assert_eq!(
            parse_data_obj("h 23"),
            Ok((
                "",
                DataObj::DataItem(ExtType::Halfword, vec![DataItem::Const(Const::Number(23))])
            ))
        );
        assert_eq!(parse_data_obj("z   42"), Ok(("", DataObj::ZeroFill(42))));
    }

    #[test]
    fn data_objects() {
        // TODO: Expand
        assert_eq!(
            parse_data_objs("z 1337"),
            Ok(("", vec![DataObj::ZeroFill(1337)]))
        );
        assert_eq!(
            parse_data_objs("z 42, z 10"),
            Ok(("", vec![DataObj::ZeroFill(42), DataObj::ZeroFill(10)]))
        );
    }

    #[test]
    fn data_definition() {
        let input = "data $b = { z 1000 }";
        let items = vec![DataObj::ZeroFill(1000)];
        assert_eq!(
            parse_data(input),
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
            parse_data(input),
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
    fn block() {
        assert_eq!(
            parse_block("@start\nhlt\n"),
            Ok((
                "",
                Block {
                    label: String::from("start"),
                    jump: Instr::Halt,
                }
            ))
        );
    }

    #[test]
    fn function_definition() {
        let input = "export function w $getone(:one %p) {\n@start\nhlt\n}";
        assert_eq!(
            parse_function(input),
            Ok((
                "",
                FuncDef {
                    linkage: Linkage::Export,
                    name: String::from("getone"),
                    abity: Some(Type::Base(BaseType::Word)),
                    params: vec![FuncParam::Regular(
                        Type::Ident(String::from("one")),
                        String::from("p")
                    )],
                    body: vec![Block {
                        label: String::from("start"),
                        jump: Instr::Halt,
                    }]
                }
            ))
        );
    }
}
