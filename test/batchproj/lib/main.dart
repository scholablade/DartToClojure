import 'dart:math' as math;

import 'package:batchproj/models/item.dart';
import 'package:batchproj/widgets/button.dart';

String describe(Item item) => Button(item).render();

Button makeButton(String label) => Button(Item(label, 3.5));

String describeKind() => '${Item.kind}: ${Item.make('box').label}';

double hyp(double a, double b) => math.sqrt(a * a + b * b);
