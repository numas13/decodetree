use tests_gen::generated_extract::*;

type Result<T = bool, E = ()> = std::result::Result<T, E>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Insn;

impl Decode for Insn {
    type Error = ();

    fn fail(&self) -> Self::Error {}

    fn set_opcode(&mut self, _: Opcode) {}

    #[rustfmt::skip]
    fn trans_sbyte(&mut self, insn: u32, set: args_b3) -> Result {
        println!("# signed byte");
        println!("  insn: {insn:08x}");
        assert_eq!(set.a, ((insn << 16) as i32) >> 24, "%s0");
        assert_eq!(set.b, ((insn <<  8) as i32) >> 24, "%s1");
        assert_eq!(set.c, ((insn      ) as i32) >> 24, "%s2");
        Ok(true)
    }

    #[rustfmt::skip]
    fn trans_ubyte(&mut self, insn: u32, set: args_b3) -> Result {
        println!("insn: {insn:08x}");
        assert_eq!(set.a, ((insn >>  8) & 0xff) as i32, "%u0");
        assert_eq!(set.b, ((insn >> 16) & 0xff) as i32, "%u1");
        assert_eq!(set.c, ((insn >> 24) & 0xff) as i32, "%u2");
        Ok(true)
    }

    fn trans_field_ref(&mut self, insn: u32, set: args_b3) -> Result {
        println!("insn: {insn:08x}");
        assert_eq!(set.a, ((insn << 23) as i32) >> 31, "%rs0");
        assert_eq!(set.b, ((insn << 20) as i32) >> 28, "%rs1");
        assert_eq!(set.c, ((insn << 16) as i32) >> 24, "%rs2");
        Ok(true)
    }

    fn trans_rev(&mut self, insn: u32, x: i32) -> Result {
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
        Ok(true)
    }

    fn trans_override(&mut self, insn: u32, x: i32) -> Result {
        println!("insn: {insn:08x}");
        println!("   x: {x:08x}");
        assert_eq!(x as u32, (insn >> 16) & 0xff, "expected insn[23:16]");
        Ok(true)
    }

    fn trans_override_fmt(&mut self, insn: u32, x: i32) -> Result {
        self.trans_override(insn, x)
    }

    fn trans_set_override(&mut self, insn: u32, a: args_a) -> Result {
        println!("insn: {insn:08x}");
        println!(" a.a: {:08x}", a.a);
        assert_eq!(a.a as u32, (insn >> 16) & 0xff, "expected insn[23:16]");
        Ok(true)
    }

    fn trans_set_override_fmt(&mut self, insn: u32, a: args_a) -> Result {
        self.trans_set_override(insn, a)
    }

    fn trans_set_fill_fmt(&mut self, insn: u32, set: args_b3) -> Result {
        self.trans_ubyte(insn, set)
    }
}

fn decode(insn: u32) {
    Insn.decode(insn).unwrap();
}

#[test]
fn field_sbyte() {
    decode(0x807f0000);
    decode(0xfff00f00);
}

#[test]
fn field_ubyte() {
    decode(0x807f0001);
    decode(0xfff00f01);
}

#[test]
fn field_ref() {
    decode(0x00000102);
    decode(0x00000802);
    decode(0x00008002);
    decode(0x00000f02);
    decode(0x0000ff02);
    decode(0x0000fe02);
    decode(0x0000f002);
    decode(0x00000002);
}

#[test]
fn field_rev() {
    decode(0x00000103);
}

#[test]
fn field_override() {
    decode(0x33221104);
}

#[test]
fn field_override_fmt() {
    decode(0x33221105);
}

#[test]
fn set_override() {
    decode(0x33221106);
}

#[test]
fn set_override_fmt() {
    decode(0x33221107);
}

#[test]
fn set_fill_fmt() {
    decode(0x807f0008);
    decode(0xfff00f08);
}
