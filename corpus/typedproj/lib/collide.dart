// M12 typed fixture: `length` is an inherited instance member here
// (`StringBuffer.length`) while `shared.dart` declares a top-level variable of
// the same name. Inside the class the inherited member must win; without the
// resolved element fact the cross-file project value was emitted instead.
class Buf extends StringBuffer {
  Buf(String s) {
    write(s);
  }

  int readLength() => length;
}

// M12 follow-up: the same precedence for *writes* — `length` is an inherited
// instance field here, and `shared.dart` declares a top-level value with that
// name. The resolved write element must win.
class BaseBuf {
  int length;

  BaseBuf(this.length);
}

class SubBuf extends BaseBuf {
  SubBuf(int v) : super(v);

  void setLength(int v) {
    length = v;
  }

  int bumpLength() {
    length++;
    return length;
  }

  int readLength() => length;
}

// M12 follow-up: a reserved-word field written through another instance from
// another file (`main.dart`) must use ClojureDart's munged spelling.
class ReservedHolder {
  int base = 0;

  int readBase() => base;
}
