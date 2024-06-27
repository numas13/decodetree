use tests_gen::{
    generated_vs::{self, Opcode},
    generated_vs_opt,
};

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
struct Insn {
    opcode: Option<Opcode>,
}

impl generated_vs::Decode for Insn {
    fn set_opcode(&mut self, opcode: Opcode) {
        self.opcode = Some(opcode);
    }
}

impl generated_vs_opt::Decode for Insn {
    fn set_opcode(&mut self, opcode: Opcode) {
        self.opcode = Some(opcode);
    }
}

#[rustfmt::skip]
const TEST: &[(u32, u32, i32, Option<Opcode>)] = &[
    // a .... 0000
    (0x0000_0000,  0,  -4, None),
    (0x0000_0000,  3,  -4, None),
    (0x0000_0000,  4,  -8, None),
    (0x0000_0000,  7,  -8, None),
    (0x0000_0000,  8,   8, Some(Opcode::A)),
    (0x0000_0000, 16,   8, Some(Opcode::A)),

    // ba .... .... 1111 0000 .... 1000
    (0x0000_00f8, 11, -12, None),
    (0x0000_00f8, 12, -24, None),
    (0x0000_00f8, 23, -24, None),
    (0x0000_00f8, 24,  24, Some(Opcode::BA)),

    // bb 1111 0001 .... 1000
    (0x0000_01f8, 11, -12, None),
    (0x0000_01f8, 12, -16, None),
    (0x0000_01f8, 16,  16, Some(Opcode::BB)),

    // bc 1111 0010 .... 1000
    (0x0000_02f8, 11, -12, None),
    (0x0000_02f8, 12, -16, None),
    (0x0000_02f8, 16,  16, Some(Opcode::BC)),

    // bd .... .... 1111 .... .... 1000
    (0x0000_03f8, 11, -12, None),
    (0x0000_03f8, 12, -24, None),
    (0x0000_03f8, 23, -24, None),
    (0x0000_03f8, 24,  24, Some(Opcode::BD)),

    // c 1111 .... .... .... .... 1001
    (0x00f0_0009,  3,  -4, None),
    (0x00f0_0009,  4, -24, None),
    (0x00f0_0009, 23, -24, None),
    (0x00f0_0009, 24,  24, Some(Opcode::C)),

    // d 1111 .... .... .... .... .... .... 1010
    (0xf000_000a,  3,  -4, None),
    (0xf000_000a,  4, -32, None),
    (0xf000_000a, 31, -32, None),
    (0xf000_000a, 32,  32, Some(Opcode::D)),

    // x 0... 1111
    (0x0000_000f,  3,  -4, None),
    (0x0000_000f,  4,  -8, None),
    (0x0000_000f,  7,  -8, None),
    (0x0000_000f,  8,   8, Some(Opcode::X)),

    // y 1... 1111
    (0x0000_008f,  3, -4, None),
    (0x0000_008f,  4, -8, None),
    (0x0000_008f,  7, -8, None),
    (0x0000_008f,  8,  8, Some(Opcode::Y)),
];

#[test]
fn decode_vs() {
    use generated_vs::Decode;

    for (raw, len, result, expected) in TEST.iter().copied() {
        println!("{raw:08x} {len:3} {result:3} {expected:?}");
        let mut insn = Insn::default();
        assert_eq!(insn.decode(raw, len), result);
        assert_eq!(insn.opcode, expected);
    }
}

#[test]
fn decode_vs_opt() {
    use generated_vs_opt::Decode;

    for (raw, len, result, expected) in TEST.iter().copied() {
        println!("{raw:08x} {len:3} {result:3} {expected:?}");
        let mut insn = Insn::default();
        assert_eq!(insn.decode(raw, len), result);
        assert_eq!(insn.opcode, expected);
    }
}
