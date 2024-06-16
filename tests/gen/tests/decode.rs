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
    test(0x01000000, Some(Opcode::B));
    test(0x02000000, Some(Opcode::C));
    test(0x03000000, Some(Opcode::D));
    test(0x04000000, Some(Opcode::E));
    test(0x04010000, Some(Opcode::F));
    test(0x04ff0000, Some(Opcode::G));
    test(0xff000000, None);
}
