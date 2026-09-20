// M12 typed fixture: top-level values whose names collide with members of a
// class in another file (see `collide.dart`). Dart scoping gives the class
// member priority inside the class; the resolved element facts must preserve
// that in the generated code.
int shared = 1;

int length = 42;

int base = 100;

int bumpShared() => ++shared;

void resetShared() {
  shared = 1;
}
