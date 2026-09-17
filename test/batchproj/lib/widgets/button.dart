import '../models/item.dart';

class Button {
  final Item item;
  final Label caption;

  Button(this.item, this.caption);

  String render() => 'Button[$caption]: ${item.display}';
}
