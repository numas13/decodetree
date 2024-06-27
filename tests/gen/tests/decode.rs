use tests_gen::{
    generated::{self, Opcode},
    generated_opt,
};

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
struct Insn {
    opcode: Option<Opcode>,
}

impl generated::Decode for Insn {
    fn cond_alias(&self) -> bool {
        true
    }

    fn set_opcode(&mut self, opcode: Opcode) {
        self.opcode = Some(opcode);
    }
}

impl generated_opt::Decode for Insn {
    fn cond_alias(&self) -> bool {
        true
    }

    fn set_opcode(&mut self, opcode: Opcode) {
        self.opcode = Some(opcode);
    }
}

const TEST: &[(u32, Option<Opcode>)] = &[
    (0x00000000, Some(Opcode::A)),
    (0x00000001, Some(Opcode::B)),
    (0x00000002, Some(Opcode::C)),
    (0x00000003, Some(Opcode::DA)),
    (0x01000003, Some(Opcode::DB)),
    (0x02000003, Some(Opcode::DC)),
    (0x03000003, Some(Opcode::DDA)),
    (0x03010003, Some(Opcode::DDB)),
    (0x03020003, Some(Opcode::DDC)),
    (0x00000004, Some(Opcode::E)),
    (0x01000004, Some(Opcode::F)),
    (0xff000004, Some(Opcode::G)),
    (0x000000ff, None),
];

#[test]
fn decode() {
    use generated::Decode;

    for (raw, expected) in TEST.iter().copied() {
        println!("{raw:08x} {expected:?}");
        let mut insn = Insn::default();
        insn.decode(raw);
        assert_eq!(insn.opcode, expected);
    }
}

#[test]
fn decode_opt() {
    use generated_opt::Decode;

    for (raw, expected) in TEST.iter().copied() {
        println!("{raw:08x} {expected:?}");
        let mut insn = Insn::default();
        insn.decode(raw);
        assert_eq!(insn.opcode, expected);
    }
}
