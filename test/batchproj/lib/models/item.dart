typedef Label = String;

int itemCount = 0;

void bumpCount() {
  itemCount++;
}

String tag(Item item) => '${item.label}:$itemCount';

// M8.18: a method parameter typed with a class declared in the same file must
// not be hinted: the hint would become part of the emitted Dart signature and
// a cross-file caller cannot resolve it (`Can't resolve type Box`).
class Box {
  final Item item;

  Box(this.item);

  String merge(Box other) => '${item.label}+${other.item.label}';
}

class Item {
  static const String kind = 'item';

  static int count = 0;

  final String label;
  final double price;

  Item(this.label, this.price);

  static Item make(String label) => Item(label, 1.0);

  static int get total => count;

  static set total(int v) {
    count = v;
  }

  String get display => '$label ($price)';
}
