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
