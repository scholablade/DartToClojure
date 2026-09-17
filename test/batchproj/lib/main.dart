import 'dart:math' as math;

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
