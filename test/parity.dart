// parity.dart — expression parity corpus for M2 (parse-only, no resolution).
import 'dart:math' as math;
import 'package:flutter/material.dart' as m;

final p01_add = 1 + 2;
final p02_arith_chain = 1 + 2 + 3 - 4;
final p03_div = 7 / 2;
final p04_intdiv = 7 ~/ 2;
final p05_mod = 7 % 3;
final p06_logic = true && false || !true;
final p07_ifnull = null ?? 3;
final p08_ternary = 1 > 0 ? 'pos' : 'neg';
final p09_eq = 1 == 2;
final p10_neq = 1 != 2;
final p11_is = 1 is int;
final p12_is_not = 1 is! String;
final p13_cast = 1 as num;
final p14_interp = 'Some $name and ${foo()}!';
final p15_interp_only = '$x';
final p16_literal_interp = 'a${1}b';
final p17_raw = r'Some $x ${y}';
final p18_adjacent = 'a' 'b';
final p19_list = [1, 2, 3];
final p20_list_typed = <int>[1, 2];
final p21_list_if = [if (true) 1 else 2];
final p22_list_for = [for (var i in [1, 2]) i];
final p23_list_spread = [1, ...xs];
final p23b_list_spread_if = [1, if (c) ...[2, 3], 4];
final p23c_list_spread_if_else = [if (c) ...[1] else ...[2, 3]];
final p23d_list_for_spread = [for (var i = 0; i < n; i++) ...[i, i]];
final p23e_list_null_aware = [?maybe];
final p24_set = {1, 2, 3};
final p25_map = {'a': 1, 'b': 2};
final p26_map_spread = {'a': 1, ...other};
final p27_index = a[0];
final p28_index_chain = a[0][1];
final p29_prop = obj.field;
final p30_prop_chain = obj.a.b;
final p31_method = obj.doIt(1, 2);
final p32_method_chain = obj.a().b;
final p33_null_chain = obj?.a?.b;
final p34_ctor = Text('hi', style: style);
final p35_ctor_named = Duration(milliseconds: 250);
final p36_static_field = Colors.blue;
final p37_static_index = Colors.red[500];
final p38_static_call = BorderRadius.circular(8);
final p39_static_chain = Colors.grey.shade300;
final p40_lambda = (int a) => a + 1;
final p41_lambda_block = (int a) { return a + 1; };
final p43_throw_expr = x ?? (throw StateError('boom'));
final p44_cascade = Point(1, 2)..x = 3..y = 4;
final p45_cascade_call = list..add(1)..add(2);
final p46_bitwise = (1 & 2) | (3 ^ 4);
final p47_shift = 1 << 2;
final p48_prefix_neg = -x;
final p49_paren = (1 + 2) * 3;
final p50_hex = 0xFF;
final p51_hex_color = 0xFFE3F2FD;
final p52_double_exp = 1.0E3;
final p53_underscore_num = 1_000;
final p54_null_lit = null;
final p55_bool_lit = true;
final p56_uri_parse = Uri.parse('http://x');
final p57_call = foo(1);
final p58_postfix_bang = maybe!;
final p59_math_prefix = math.max(1, 2);
final p60_map_nested = {'a': [1, 2]};
final p61_not_equal_chain = !(a == b);
final p62_and_chain = a && b && c;
final p63_or_chain = a || b || c;
final p64_comparison = a < b;
final p65_string_dq = "double";
final p66_prefixed_ctor = m.Text('x');
final p67_widget_column = Column(children: [
  Question(questions[_questionIndex]['questionText']),
  (questions[_questionIndex]['answers'] as List<String>)
      .map((answer) {
    return Answer(_answerQuestion, answer);
  }).toList(),
  Numb(sorted: (1 < 2 && 2 < 3 && 3 < a) && (a == b)),
]);
final p68_widget_app = MaterialApp(
  title: 'Cljd Demo',
  theme: ThemeData(primarySwatch: Colors.blue),
  home: Scaffold(
      appBar: AppBar(title: Text('Home')),
      body: Center(child: Text('Hi'))),
);
final p69_ternary_lambda =
    TextButton(onPressed: _searchEnabled ? () { pop(); } : null);
final p70_interp_factory = Text('Some $field and ${Factory.create()}');
final p71_get_chain = questions[1]['two'];
final p72_const_list = const SizedBox(height: 8);
