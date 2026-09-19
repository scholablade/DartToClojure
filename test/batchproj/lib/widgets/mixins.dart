mixin Labelled on Object {
  final String label = 'L';
  bool _marked = false;

  bool get marked => _marked;

  void mark() {
    _marked = true;
  }

  String describeLabelled(String text) => '$label:$text:$_marked';
}
