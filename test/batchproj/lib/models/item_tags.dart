// Part of `item.dart` (URI form): declarations are merged into the library's
// namespace by the converter (`part`/`part of`, M9). `_Meta` shares the
// library's private scope.
part of 'item.dart';

String tagLine(Item item) => '${item.label}#${item.price}';

class Tag {
  final String value;

  Tag(this.value);

  String render() => '[$value]';
}

class _Meta {
  final String note;

  _Meta(this.note);

  String get text => note;
}
