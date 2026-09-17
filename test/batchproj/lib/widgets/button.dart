import '../models/item.dart';

class Button {
  final Item item;

  Button(this.item);

  String render() => 'Button: ${item.display}';
}
