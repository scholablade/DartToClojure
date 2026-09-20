import 'dart:math' as math;

import 'package:batchproj/barrel.dart';
import 'package:batchproj/models/item.dart';
import 'package:batchproj/widgets/button.dart';

String describe(Item item) => Button(item, 'b').render();

Button makeButton(String label) => Button(Item(label, 3.5), label);

String describeKind() => '${Item.kind}: ${Item.make('box').label}';

String tagOf(Item item) => tag(item);

void resetCounts() {
  itemCount = 0;
  Item.count = 0;
  Item.total = 0;
}

String touch() {
  itemCount++;
  Item.count = Item.count + 2;
  return '$itemCount/${Item.count}/${Item.total}';
}

double hyp(double a, double b) => math.sqrt(a * a + b * b);

String mergeBoxes() =>
    Box(Item('a', 1.0)).merge(Box(Item('b', 2.0)));

// M9: symbols declared in `item.dart`'s parts (`item_tags.dart`,
// `item_names.dart`) are library members: they are indexed under item.dart's
// namespace and callable across files.
String describeTag(Item item) => tagLine(item);

String itemTag(Item item) => item.tag().render();

String itemMeta(Item item) => item.meta();

String makeTag(String value) => Tag(value).render();

String declaredName(String label) => itemDisplayName(label);

// M8: `Badge` (declared in the barrel) applies a mixin from another file;
// `Widget`/`Text` arrive through the barrel's external `export`, so the
// generated file must require the exports shim.
String badgeText(String text) {
  final badge = Badge(text);
  badge.mark();
  return badge.render();
}

Widget makeLabel(String text) => Text(text);

class Holder {
  final Widget child;

  Holder(this.child);
}

// A same-file top-level value: with the exports shim in scope it must stay
// bare (`_motto`), not be routed through the shim.
final String _motto = 'motto';

String motto() => _motto;
