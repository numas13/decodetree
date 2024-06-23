use tests_gen::decode32::{Decode, Opcode};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Insn {
    opcode: Option<Opcode>,
}

impl Decode for Insn {
    fn set_opcode(&mut self, opcode: Opcode) {
        self.opcode = Some(opcode);
    }
}

#[test]
fn decode() {
    fn test(raw: u32, expected: Option<Opcode>) {
        let mut insn = Insn { opcode: None };
        insn.decode(raw);
        assert_eq!(insn.opcode, expected);
    }

    test(0x00000000, Some(Opcode::A));
    test(0x00000001, Some(Opcode::B));
    test(0x00000002, Some(Opcode::C));
    test(0x00000003, Some(Opcode::D));
    test(0x00000004, Some(Opcode::E));
    test(0x01000004, Some(Opcode::F));
    test(0xff000004, Some(Opcode::G));
    test(0x000000ff, None);
}
