// typed.dart — M11 resolved-type corpus (`astdump --types`).
//
// Every case below is decided from the analyzer-resolved static type of an
// expression: the syntactic passes alone cannot know the type of a call
// result, another object's field, a static field or a top-level getter.
// The committed fixture (`test/fixtures/typed.edn`) carries the
// `:static-type` facts; conversion without `--types` keeps the old
// heuristics (numeric fallback/guards) for the same source.

class Vec {
  final int x;
  const Vec(this.x);
  Vec operator +(Vec o) => Vec(x + o.x);
  Vec operator *(int k) => Vec(x * k);
  String describe() => 'Vec($x)';
}

class Holder {
  final Vec v;
  final String name;
  const Holder(this.v, this.name);
}

const String vecPrefix = 'vec:';
String get vecSuffix => '!';

Vec vecOf(int x) => Vec(x);
String labelOf(int x) => 'v$x';
int intOf(int x) => x + 1;

int typedOps(Holder h) {
  final a = vecOf(1) + vecOf(2);
  final b = vecOf(3) * 2;
  final c = h.v + a;
  final s = labelOf(4) + 'x';
  final t = h.name + vecPrefix;
  final u = vecSuffix + h.name;
  final n = intOf(2) * 3;
  return c.x + s.length + t.length + u.length + n + b.x;
}

String typedStrings(Holder h) =>
    labelOf(4) + h.name + vecPrefix + vecSuffix;
