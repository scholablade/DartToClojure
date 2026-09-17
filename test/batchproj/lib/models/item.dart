class Item {
  static const String kind = 'item';

  final String label;
  final double price;

  Item(this.label, this.price);

  static Item make(String label) => Item(label, 1.0);

  String get display => '$label ($price)';
}
