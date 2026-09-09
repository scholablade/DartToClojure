// statements.dart — statement parity corpus for M2 (parse-only).
void s_if(int a) {
  if (a > 0) print('pos');
}

void s_if_else(int a) {
  if (a > 0) print('pos');
  else print('neg');
}

void s_else_if(int a) {
  if (a > 0) print('pos');
  else if (a < 0) print('neg');
  else print('zero');
}

void s_for_in(List<int> xs) {
  for (final x in xs) {
    print(x);
  }
}

void s_switch(int x) {
  switch (x) {
    case 1:
      print('one');
      break;
    case 2:
    case 3:
      print('2or3');
      break;
    default:
      print('other');
  }
}

void s_try() {
  try {
    print(1);
  } on StateError catch (e, st) {
    print(e);
  } catch (e) {
    print(e);
  } finally {
    print(2);
  }
}

int s_return(int a) {
  return a + 1;
}

int s_expr_body(int a) => a * 2;

void s_vars() {
  final a = 1;
  var b = 2;
  print(a + b);
}

void s_assert(int a) {
  assert(a > 0, 'positive');
}

void s_nested_block(int a) {
  {
    print(a);
  }
}

Future<int> s_await(Future<int> f) async {
  return await f;
}
