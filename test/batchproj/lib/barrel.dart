export 'package:batchproj/models/item.dart';
export 'package:flutter/material.dart';

import 'package:batchproj/widgets/mixins.dart';

class Badge with Labelled {
  final String text;

  Badge(this.text);

  String render() => describeLabelled(text);
}
