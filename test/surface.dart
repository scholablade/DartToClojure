// surface.dart — Dart constructs not covered by sample.dart, for astdump coverage.
library surface.lib;

import 'dart:async' deferred as a show Future hide Stream;
import 'x.dart' if (dart.library.io) 'y.dart';
export 'src/x.dart' show A hide B;

typedef OldFn(int x);
typedef NewFn = void Function(int);

mixin M {}

class Base {}

class I {}

class C = Base with M implements I;

const int kOne = 1;

class D extends Base {
  final int x;
  int y = 0;
  D(this.x) : y = x + 1, super();
  D.redir() : this(2);
  int operator [](int i) => i;
  String describe() => super.toString();
}

int? maybe;

void takesCallback(void cb(int x)) {}

void top(int? n, List<int> xs, dynamic d) {
  final s1 = 'a' 'b';
  final l = <int>[1, ?maybe, ...xs];
  final f = ((int a) => a)(2);
  var i = 0;
  for (i in xs) {}
  for (i = 0; i < 2; i++) {}
  for (var (int p, int q) = (1, 2); p < 2; p++) {}
  for (var (int p, int q) in [(1, 2)]) {}
  label:
  ;
  while (true) {
    break label;
  }
  switch (n) {
    case > 1:
      break;
    case int a && > 0:
      break;
    case var b as int:
      break;
    case int c?:
      break;
    case int e!:
      break;
    case (int g):
      break;
    case C(x: 1):
      break;
    default:
      break;
  }
  switch (i) {
    case kOne:
      break;
    default:
      break;
  }
  assert(n != null, 'n');
  print('$i $d $s1 $l $f');
}
