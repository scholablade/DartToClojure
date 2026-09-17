typedef Label = String;

int itemCount = 0;

void bumpCount() {
  itemCount++;
}

String tag(Item item) => '${item.label}:$itemCount';

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
