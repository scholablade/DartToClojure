import 'dart:math';

import 'collide.dart';
import 'shared.dart';

// An unaliased `dart:math` top-level function/getter must be resolved as a
// top-level name — routed through the imports/exports shim in batch mode —
// never as an implicit-this member, even inside a class method.
class Calc {
  num maxOf(num a, num b) => max(a, b);

  double circleArea(double r) => pi * r * r;
}

int bufLength(String s) => Buf(s).readLength();

num calcMax(int a, int b) => Calc().maxOf(a, b);
double calcArea(double r) => Calc().circleArea(r);

int sharedValue() => shared;

int topLength() => length;

int bumpedShared() => bumpShared();

// M12 follow-up: inherited-field writes (see `SubBuf`) must win over the
// top-level `length`.
int subLength(int v) {
  final s = SubBuf(v);
  s.setLength(v);
  return s.readLength();
}

int subBumpedLength(int v) {
  final s = SubBuf(v);
  return s.bumpLength();
}

// M12 follow-up: a reserved-word field of another file's class written
// through an instance (`base` is also a top-level value in shared.dart).
int writeReservedBase(int v) {
  final h = ReservedHolder();
  h.base = v;
  return h.readBase();
}
