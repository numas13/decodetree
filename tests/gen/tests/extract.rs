use tests_gen::generated_extract::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Insn;

impl Decode for Insn {
    fn set_opcode(&mut self, _: Opcode) {}

    #[rustfmt::skip]
    fn trans_sbyte(&mut self, insn: u32, set: args_b3) -> bool {
        println!("# signed byte");
        println!("  insn: {insn:08x}");
        assert_eq!(set.a, ((insn << 16) as i32) >> 24, "%s0");
        assert_eq!(set.b, ((insn <<  8) as i32) >> 24, "%s1");
        assert_eq!(set.c, ((insn      ) as i32) >> 24, "%s2");
        true
    }

    #[rustfmt::skip]
    fn trans_ubyte(&mut self, insn: u32, set: args_b3) -> bool {
        println!("insn: {insn:08x}");
        assert_eq!(set.a, ((insn >>  8) & 0xff) as i32, "%u0");
        assert_eq!(set.b, ((insn >> 16) & 0xff) as i32, "%u1");
        assert_eq!(set.c, ((insn >> 24) & 0xff) as i32, "%u2");
        true
    }

    fn trans_field_ref(&mut self, insn: u32, set: args_b3) -> bool {
        println!("insn: {insn:08x}");
        assert_eq!(set.a, ((insn << 23) as i32) >> 31, "%rs0");
        assert_eq!(set.b, ((insn << 20) as i32) >> 28, "%rs1");
        assert_eq!(set.c, ((insn << 16) as i32) >> 24, "%rs2");
        true
    }

    fn trans_rev(&mut self, insn: u32, x: i32) -> bool {
        fn nibble(x: u32, i: u32) -> u32 {
            (x >> (i * 4)) & 0xf
        }
        println!("insn: {insn:08x}");
        println!("   x: {x:08x}");
        for i in 0..6 {
            let from = 2 + i;
            let to = 5 - i;
            assert_eq!(nibble(x as u32, to), nibble(insn, from), "{to} <= {from}");
        }
        true
    }

    fn trans_override(&mut self, insn: u32, x: i32) -> bool {
        println!("insn: {insn:08x}");
        println!("   x: {x:08x}");
        assert_eq!(x as u32, (insn >> 16) & 0xff, "expected insn[23:16]");
        true
    }

    fn trans_override_fmt(&mut self, insn: u32, x: i32) -> bool {
        self.trans_override(insn, x)
    }

    fn trans_set_override(&mut self, insn: u32, a: args_a) -> bool {
        println!("insn: {insn:08x}");
        println!(" a.a: {:08x}", a.a);
        assert_eq!(a.a as u32, (insn >> 16) & 0xff, "expected insn[23:16]");
        true
    }

    fn trans_set_override_fmt(&mut self, insn: u32, a: args_a) -> bool {
        self.trans_set_override(insn, a)
    }

    fn trans_set_fill_fmt(&mut self, insn: u32, set: args_b3) -> bool {
        self.trans_ubyte(insn, set)
    }
}

#[test]
fn field_sbyte() {
    let mut insn = Insn;
    insn.decode(0x807f0000);
    insn.decode(0xfff00f00);
}

#[test]
fn field_ubyte() {
    let mut insn = Insn;
    insn.decode(0x807f0001);
    insn.decode(0xfff00f01);
}

#[test]
fn field_ref() {
    let mut insn = Insn;
    insn.decode(0x00000102);
    insn.decode(0x00000802);
    insn.decode(0x00008002);
    insn.decode(0x00000f02);
    insn.decode(0x0000ff02);
    insn.decode(0x0000fe02);
    insn.decode(0x0000f002);
    insn.decode(0x00000002);
}

#[test]
fn field_rev() {
    let mut insn = Insn;
    insn.decode(0x00000103);
}

#[test]
fn field_override() {
    let mut insn = Insn;
    insn.decode(0x33221104);
}

#[test]
fn field_override_fmt() {
    let mut insn = Insn;
    insn.decode(0x33221105);
}

#[test]
fn set_override() {
    let mut insn = Insn;
    insn.decode(0x33221106);
}

#[test]
fn set_override_fmt() {
    let mut insn = Insn;
    insn.decode(0x33221107);
}

#[test]
fn set_fill_fmt() {
    let mut insn = Insn;
    insn.decode(0x807f0008);
    insn.decode(0xfff00f08);
}
