// astdump — dump the package:analyzer syntax AST of a Dart file or snippet as EDN.
//
// Determinism contract (schema :ast-version 1):
//  - every node is a map `{:ast :<AnalyzerClassName> <fixed-order type keys> :src {:line L :col C}}`
//  - fixed key order per node type; 1-based line/col from the node's begin token
//  - no timestamps, no random ids, no env-dependent output
//  - unknown node types are dumped generically via childEntities with `:generic true`
//
// Usage:
//   dart run bin/astdump.dart <file.dart>      file mode
//   cat snippet.dart | dart run bin/astdump.dart -   snippet mode (stdin)
//   dart run bin/astdump.dart --version        print the EDN schema version
//   dart run bin/astdump.dart --resolved f.dart  attempt analysis-session resolution
import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'package:analyzer/dart/analysis/analysis_context_collection.dart';
import 'package:analyzer/dart/analysis/features.dart';
import 'package:analyzer/dart/analysis/results.dart';
import 'package:analyzer/dart/analysis/utilities.dart';
import 'package:analyzer/dart/ast/ast.dart';
import 'package:analyzer/dart/ast/token.dart';
import 'package:analyzer/diagnostic/diagnostic.dart';
import 'package:analyzer/source/line_info.dart';

const int astVersion = 1;
final featureSet = FeatureSet.latestLanguageVersion();

/// Set to 1 when a snippet was wrapped in a synthetic function body so that
/// reported positions match the original snippet.
int _lineOffset = 0;

Future<void> main(List<String> args) async {
  var resolved = false;
  String? file;
  for (final a in args) {
    if (a == '--resolved') {
      resolved = true;
    } else if (a == '--version') {
      stdout.writeln(':ast-version $astVersion');
      return;
    } else if (a == '-h' || a == '--help') {
      _usage();
      return;
    } else if (a.startsWith('-') && a != '-') {
      stderr.writeln('astdump: unknown option: $a');
      exit(2);
    } else {
      file = a;
    }
  }
  final bool snippet;
  final String content;
  final String path;
  if (file == null || file == '-') {
    snippet = true;
    content = await stdin.transform(utf8.decoder).join();
    path = '<snippet>';
  } else {
    snippet = false;
    final f = File(file);
    if (!f.existsSync()) {
      stderr.writeln('astdump: no such file: $file');
      exit(2);
    }
    content = f.readAsStringSync();
    path = f.path;
  }

  final result = parseString(
    content: content,
    path: path,
    featureSet: featureSet,
    throwIfDiagnostics: false,
  );
  var unit = result.unit;
  var errors = result.errors;
  // Snippet mode accepts bare expressions/statements (DartClojure parity):
  // retry inside a function body and unwrap it, shifting lines back by one.
  var wrapped = false;
  if (snippet && errors.any((e) => e.severity == Severity.error)) {
    final wresult = parseString(
      content: 'void _snippet_() {\n$content\n;\n}',
      path: path,
      featureSet: featureSet,
      throwIfDiagnostics: false,
    );
    if (!wresult.errors.any((e) => e.severity == Severity.error)) {
      unit = wresult.unit;
      errors = const [];
      wrapped = true;
    }
  }
  final li = unit.lineInfo;
  _lineOffset = wrapped ? 1 : 0;

  final buf = StringBuffer();
  buf.write('{:ast-version $astVersion :mode ${snippet ? ':snippet' : ':file'}');
  if (!snippet) buf.write(' :path "${_escape(path)}"');
  buf.write(' :errors [');
  for (final e in errors) {
    final loc = li.getLocation(e.offset);
    buf.write(
        '{:message "${_escape(e.message)}" :severity :${e.severity.name.toLowerCase()} '
        ':line ${loc.lineNumber - _lineOffset} :col ${loc.columnNumber}} ');
  }
  buf.write('] :resolved ');
  if (!resolved) {
    buf.write('nil');
  } else {
    buf.write(await _resolvedInfo(path));
  }
  buf.write(' :unit ');
  if (wrapped) {
    final fn = unit.declarations.single as FunctionDeclaration;
    final block = (fn.functionExpression.body as BlockFunctionBody).block;
    buf.write('{:ast :CompilationUnit :directives [] :decls '
        '${vec(block.statements.map((st) => dump(st, li)))} '
        ':src {:line 1 :col 1}}');
  } else {
    buf.write(dump(unit, li));
  }
  buf.write('}');
  stdout.writeln(buf.toString());
}

Future<String> _resolvedInfo(String path) async {
  final abs = File(path).absolute.path;
  try {
    final collection = AnalysisContextCollection(includedPaths: [abs]);
    final session = collection.contextFor(abs).currentSession;
    final ru = await session.getResolvedUnit(abs);
    if (ru is ResolvedUnitResult) {
      return 'true';
    }
    return 'false';
  } catch (e) {
    return 'false';
  }
}

void _usage() {
  stderr.writeln(
      'usage: dart run bin/astdump.dart [--resolved] [<file.dart>|-]\n'
      '  no file or "-" reads a Dart snippet from stdin\n'
      '  --version  print the EDN schema version');
}

// ---------------------------------------------------------------------------
// EDN emission
// ---------------------------------------------------------------------------

String _escape(String s) {
  final b = StringBuffer();
  for (final cu in s.runes) {
    switch (cu) {
      case 0x22:
        b.write(r'\"');
      case 0x5C:
        b.write(r'\\');
      case 0x0A:
        b.write(r'\n');
      case 0x0D:
        b.write(r'\r');
      case 0x09:
        b.write(r'\t');
      default:
        if (cu < 0x20) {
          b.write('\\u${cu.toRadixString(16).padLeft(4, '0')}');
        } else {
          b.writeCharCode(cu);
        }
    }
  }
  return b.toString();
}

String s(String v) => '"${_escape(v)}"';
String sQ(String? v) => v == null ? 'nil' : s(v);
String i(int? n) => n?.toString() ?? 'nil';
String b(bool v) => v ? 'true' : 'false';
String opt(String? v) => v ?? 'nil';
String vec(Iterable<String> parts) => '[${parts.join(' ')}]';

String kw(AstNode n) {
  var t = n.runtimeType.toString();
  t = t.replaceAll(RegExp(r'Impl\d*$'), '');
  return ':$t';
}

String src(Token? token, LineInfo li) {
  if (token == null) return 'nil';
  final loc = li.getLocation(token.offset);
  return '{:line ${loc.lineNumber - _lineOffset} :col ${loc.columnNumber}}';
}

String srcOf(AstNode n, LineInfo li) => src(n.beginToken, li);

String _tok(Token? t) => t == null ? 'nil' : s(t.lexeme);

String _typeArgs(TypeArgumentList? tl, LineInfo li) =>
    tl == null ? 'nil' : vec(tl.arguments.map((a) => dump(a, li)));

String dump(AstNode node, LineInfo li) {
  final f = StringBuffer();
  if (node is AnnotatedNode) {
    f.write(':annotations ${vec(node.metadata.map((m) => dump(m, li)))} ');
  }
  switch (node) {
    // ---------------------------------------------------------------------
    // Unit & directives
    // ---------------------------------------------------------------------
    case CompilationUnit u:
      f.write(':directives ${vec(u.directives.map((d) => dump(d, li)))} '
          ':decls ${vec(u.declarations.map((d) => dump(d, li)))} ');
    case LibraryDirective d:
      f.write(':name ${d.name == null ? 'nil' : vec(d.name!.tokens.map((c) => s(c.lexeme)))} ');
    case ImportDirective d:
      f.write(':uri ${dump(d.uri, li)} '
          ':as ${d.prefix == null ? 'nil' : s(d.prefix!.name)} '
          ':deferred ${b(d.deferredKeyword != null)} '
          ':configurations ${vec(d.configurations.map((c) => dump(c, li)))} '
          ':show ${_combinatorNames(d.combinators, 'Show')} '
          ':hide ${_combinatorNames(d.combinators, 'Hide')} ');
    case ExportDirective d:
      f.write(':uri ${dump(d.uri, li)} '
          ':show ${_combinatorNames(d.combinators, 'Show')} '
          ':hide ${_combinatorNames(d.combinators, 'Hide')} ');
    case PartDirective d:
      f.write(':uri ${dump(d.uri, li)} ');
    case PartOfDirective d:
      f.write(':library ${d.libraryName == null ? 'nil' : vec(d.libraryName!.tokens.map((c) => s(c.lexeme)))} '
          ':uri ${d.uri == null ? 'nil' : dump(d.uri!, li)} ');
    case Configuration c:
      f.write(':name ${vec(c.name.tokens.map((c2) => s(c2.lexeme)))} '
          ':uri ${dump(c.uri, li)} ');
    case ShowCombinator c:
      f.write(':names ${vec(c.shownNames.map((n) => s(n.name)))} ');
    case HideCombinator c:
      f.write(':names ${vec(c.hiddenNames.map((n) => s(n.name)))} ');

    // ---------------------------------------------------------------------
    // Declarations
    // ---------------------------------------------------------------------
    case ClassDeclaration c:
      f.write(':abstract ${b(c.abstractKeyword != null)} '
          ':modifier ${_classModifier(c)} '
          ':name ${s(c.namePart.typeName.lexeme)} '
          ':type-params ${c.namePart.typeParameters == null ? 'nil' : vec(c.namePart.typeParameters!.typeParameters.map((t) => dump(t, li)))} '
          ':extends ${c.extendsClause == null ? 'nil' : dump(c.extendsClause!.superclass, li)} '
          ':with ${c.withClause == null ? 'nil' : vec(c.withClause!.mixinTypes.map((m) => dump(m, li)))} '
          ':implements ${c.implementsClause == null ? 'nil' : vec(c.implementsClause!.interfaces.map((i2) => dump(i2, li)))} '
          ':native ${c.nativeClause == null || c.nativeClause!.name == null ? 'nil' : dump(c.nativeClause!.name!, li)} '
          ':members ${vec(c.body.members.map((m) => dump(m, li)))} ');
    case ClassTypeAlias c:
      f.write(':modifier ${_classModifier(c)} '
          ':name ${s(c.name.lexeme)} '
          ':type-params ${c.typeParameters == null ? 'nil' : vec(c.typeParameters!.typeParameters.map((t) => dump(t, li)))} '
          ':superclass ${dump(c.superclass, li)} '
          ':with ${vec(c.withClause.mixinTypes.map((m) => dump(m, li)))} '
          ':implements ${c.implementsClause == null ? 'nil' : vec(c.implementsClause!.interfaces.map((i2) => dump(i2, li)))} ');
    case MixinDeclaration m:
      f.write(':base ${b(m.baseKeyword != null)} '
          ':name ${s(m.name.lexeme)} '
          ':type-params ${m.typeParameters == null ? 'nil' : vec(m.typeParameters!.typeParameters.map((t) => dump(t, li)))} '
          ':on ${m.onClause == null ? 'nil' : vec(m.onClause!.superclassConstraints.map((c) => dump(c, li)))} '
          ':implements ${m.implementsClause == null ? 'nil' : vec(m.implementsClause!.interfaces.map((i2) => dump(i2, li)))} '
          ':members ${vec(m.body.members.map((m2) => dump(m2, li)))} ');
    case EnumDeclaration e:
      f.write(':name ${s(e.namePart.typeName.lexeme)} '
          ':type-params ${e.namePart.typeParameters == null ? 'nil' : vec(e.namePart.typeParameters!.typeParameters.map((t) => dump(t, li)))} '
          ':with ${e.withClause == null ? 'nil' : vec(e.withClause!.mixinTypes.map((m) => dump(m, li)))} '
          ':implements ${e.implementsClause == null ? 'nil' : vec(e.implementsClause!.interfaces.map((i2) => dump(i2, li)))} '
          ':constants ${vec(e.body.constants.map((c) => dump(c, li)))} '
          ':members ${vec(e.body.members.map((m) => dump(m, li)))} ');
    case EnumConstantDeclaration c:
      final args = c.arguments;
      f.write(':name ${s(c.name.lexeme)} '
          ':type-args ${args == null || args.typeArguments == null ? 'nil' : _typeArgs(args.typeArguments, li)} '
          ':ctor ${args == null || args.constructorSelector == null ? 'nil' : s(args.constructorSelector!.name.name)} '
          ':args ${args == null ? 'nil' : dump(args.argumentList, li)} ');
    case ExtensionDeclaration e:
      f.write(':name ${e.name == null ? 'nil' : s(e.name!.lexeme)} '
          ':type-params ${e.typeParameters == null ? 'nil' : vec(e.typeParameters!.typeParameters.map((t) => dump(t, li)))} '
          ':on ${e.onClause == null ? 'nil' : dump(e.onClause!.extendedType, li)} '
          ':members ${vec(e.body.members.map((m) => dump(m, li)))} ');
    case ExtensionTypeDeclaration e:
      final pc = e.primaryConstructor;
      f.write(':name ${s(e.namePart.typeName.lexeme)} '
          ':type-params ${e.namePart.typeParameters == null ? 'nil' : vec(e.namePart.typeParameters!.typeParameters.map((t) => dump(t, li)))} '
          ':const ${b(pc.constKeyword != null)} '
          ':ctor ${pc.constructorName == null ? 'nil' : s(pc.constructorName!.name.lexeme)} '
          ':params ${dump(pc.formalParameters, li)} '
          ':implements ${e.implementsClause == null ? 'nil' : vec(e.implementsClause!.interfaces.map((i2) => dump(i2, li)))} '
          ':members ${vec(e.body.members.map((m) => dump(m, li)))} ');
    case FunctionDeclaration fd:
      final fe = fd.functionExpression;
      f.write(':name ${s(fd.name.lexeme)} '
          ':kind ${fd.isGetter ? ':getter' : fd.isSetter ? ':setter' : ':fn'} '
          ':external ${b(fd.externalKeyword != null)} '
          ':return-type ${fd.returnType == null ? 'nil' : dump(fd.returnType!, li)} '
          ':type-params ${fe.typeParameters == null ? 'nil' : vec(fe.typeParameters!.typeParameters.map((t) => dump(t, li)))} '
          ':params ${fe.parameters == null ? 'nil' : dump(fe.parameters!, li)} '
          ':body ${dump(fe.body, li)} ');
    case TopLevelVariableDeclaration t:
      f.write(':external ${b(t.externalKeyword != null)} '
          ':vars ${dump(t.variables, li)} ');
    case FieldDeclaration fd:
      f.write(':static ${b(fd.isStatic)} '
          ':covariant ${b(fd.covariantKeyword != null)} '
          ':abstract ${b(fd.abstractKeyword != null)} '
          ':external ${b(fd.externalKeyword != null)} '
          ':fields ${dump(fd.fields, li)} ');
    case MethodDeclaration m:
      f.write(':name ${s(m.name.lexeme)} '
          ':kind ${m.isGetter ? ':getter' : m.isSetter ? ':setter' : ':method'} '
          ':static ${b(m.isStatic)} '
          ':abstract ${b(m.body is EmptyFunctionBody && m.externalKeyword == null)} '
          ':external ${b(m.externalKeyword != null)} '
          ':operator ${b(m.isOperator)} '
          ':return-type ${m.returnType == null ? 'nil' : dump(m.returnType!, li)} '
          ':type-params ${m.typeParameters == null ? 'nil' : vec(m.typeParameters!.typeParameters.map((t) => dump(t, li)))} '
          ':params ${m.parameters == null ? 'nil' : dump(m.parameters!, li)} '
          ':body ${dump(m.body, li)} ');
    case ConstructorDeclaration c:
      f.write(':name ${c.name == null ? 'nil' : s(c.name!.lexeme)} '
          ':type ${s(c.typeName!.name)} '
          ':const ${b(c.constKeyword != null)} '
          ':factory ${b(c.factoryKeyword != null)} '
          ':external ${b(c.externalKeyword != null)} '
          ':params ${dump(c.parameters, li)} '
          ':initializers ${vec(c.initializers.map((i2) => dump(i2, li)))} '
          ':redirect ${c.redirectedConstructor == null ? 'nil' : _ctorName(c.redirectedConstructor!, li)} '
          ':body ${dump(c.body, li)} ');
    case GenericTypeAlias g:
      f.write(':name ${s(g.name.lexeme)} '
          ':type-params ${g.typeParameters == null ? 'nil' : vec(g.typeParameters!.typeParameters.map((t) => dump(t, li)))} '
          ':type ${dump(g.functionType ?? g.type, li)} ');
    case FunctionTypeAlias ft:
      f.write(':name ${s(ft.name.lexeme)} '
          ':type-params ${ft.typeParameters == null ? 'nil' : vec(ft.typeParameters!.typeParameters.map((t) => dump(t, li)))} '
          ':return-type ${ft.returnType == null ? 'nil' : dump(ft.returnType!, li)} '
          ':params ${dump(ft.parameters, li)} ');

    // ---------------------------------------------------------------------
    // Class members: initializers
    // ---------------------------------------------------------------------
    case SuperConstructorInvocation s2:
      f.write(':name ${s2.constructorName == null ? 'nil' : s(s2.constructorName!.name)} '
          ':args ${dump(s2.argumentList, li)} ');
    case ConstructorFieldInitializer c:
      f.write(':field ${s(c.fieldName.name)} :expr ${dump(c.expression, li)} ');
    case RedirectingConstructorInvocation r:
      f.write(':name ${r.constructorName == null ? 'nil' : s(r.constructorName!.name)} '
          ':args ${dump(r.argumentList, li)} ');
    case AssertInitializer a:
      f.write(':cond ${dump(a.condition, li)} '
          ':message ${a.message == null ? 'nil' : dump(a.message!, li)} ');

    // ---------------------------------------------------------------------
    // Parameters
    // ---------------------------------------------------------------------
    case FormalParameterList pl:
      f.write(':params ${vec(pl.parameters.map((p) => dump(p, li)))} ');
    case FormalParameter p:
      f.write(':kind ${p.isNamed ? ':named' : p.isOptionalPositional ? ':optional-positional' : ':required-positional'} '
          ':name ${p.name == null ? 'nil' : s(p.name!.lexeme)} '
          ':type ${p.type == null ? 'nil' : dump(p.type!, li)} '
          ':final ${b(p.finalKeyword != null)} '
          ':const ${b(p.constKeyword != null)} '
          ':covariant ${b(p.covariantKeyword != null)} '
          ':default ${p.defaultClause == null ? 'nil' : dump(p.defaultClause!.value, li)} '
          ':required ${b(p.isRequiredNamed)} '
          ':super ${b(p is SuperFormalParameter)} '
          ':this ${b(p is FieldFormalParameter)} '
          ':fn-typed ${p.functionTypedSuffix == null ? 'nil' : '{:type-params ${p.functionTypedSuffix!.typeParameters == null ? 'nil' : vec(p.functionTypedSuffix!.typeParameters!.typeParameters.map((t) => dump(t, li)))} :params ${dump(p.functionTypedSuffix!.formalParameters, li)} :nullable ${b(p.functionTypedSuffix!.question != null)}}'} ');
    case TypeParameter t:
      f.write(':name ${s(t.name.lexeme)} '
          ':bound ${t.bound == null ? 'nil' : dump(t.bound!, li)} ');

    // ---------------------------------------------------------------------
    // Statements
    // ---------------------------------------------------------------------
    case Block bl:
      f.write(':stmts ${vec(bl.statements.map((st) => dump(st, li)))} ');
    case VariableDeclarationList vl:
      f.write(':late ${b(vl.isLate)} '
          ':final ${b(vl.isFinal)} '
          ':const ${b(vl.isConst)} '
          ':type ${vl.type == null ? 'nil' : dump(vl.type!, li)} '
          ':vars ${vec(vl.variables.map((v) => dump(v, li)))} ');
    case VariableDeclaration v:
      f.write(':name ${s(v.name.lexeme)} '
          ':init ${v.initializer == null ? 'nil' : dump(v.initializer!, li)} ');
    case VariableDeclarationStatement v:
      f.write(':vars ${dump(v.variables, li)} ');
    case ExpressionStatement e:
      f.write(':expr ${dump(e.expression, li)} ');
    case ReturnStatement r:
      f.write(':expr ${r.expression == null ? 'nil' : dump(r.expression!, li)} ');
    case IfStatement i2:
      f.write(':cond ${dump(i2.expression, li)} '
          ':case ${i2.caseClause == null ? 'nil' : dump(i2.caseClause!, li)} '
          ':then ${dump(i2.thenStatement, li)} '
          ':else ${i2.elseStatement == null ? 'nil' : dump(i2.elseStatement!, li)} ');
    case CaseClause c:
      f.write(':pattern ${dump(c.guardedPattern, li)} ');
    case ForStatement fs:
      f.write(':loop ${dump(fs.forLoopParts, li)} '
          ':body ${dump(fs.body, li)} ');
    case ForEachPartsWithDeclaration fe:
      f.write(':var ${_declaredIdent(fe.loopVariable, li)} '
          ':iterable ${dump(fe.iterable, li)} ');
    case ForEachPartsWithIdentifier fe:
      f.write(':var ${s(fe.identifier.name)} '
          ':iterable ${dump(fe.iterable, li)} ');
    case ForEachPartsWithPattern fe:
      f.write(':pattern ${dump(fe.pattern, li)} '
          ':final ${b(fe.keyword.lexeme == 'final')} '
          ':iterable ${dump(fe.iterable, li)} ');
    case ForPartsWithDeclarations fp:
      f.write(':init ${dump(fp.variables, li)} '
          ':cond ${fp.condition == null ? 'nil' : dump(fp.condition!, li)} '
          ':updaters ${vec(fp.updaters.map((u) => dump(u, li)))} ');
    case ForPartsWithExpression fp:
      f.write(':init ${fp.initialization == null ? 'nil' : dump(fp.initialization!, li)} '
          ':cond ${fp.condition == null ? 'nil' : dump(fp.condition!, li)} '
          ':updaters ${vec(fp.updaters.map((u) => dump(u, li)))} ');
    case ForPartsWithPattern fp:
      f.write(':pattern ${dump(fp.variables, li)} '
          ':cond ${fp.condition == null ? 'nil' : dump(fp.condition!, li)} '
          ':updaters ${vec(fp.updaters.map((u) => dump(u, li)))} ');
    case DeclaredIdentifier d:
      f.write('${_declaredIdent(d, li)}');
    case WhileStatement w:
      f.write(':cond ${dump(w.condition, li)} :body ${dump(w.body, li)} ');
    case DoStatement d:
      f.write(':body ${dump(d.body, li)} :cond ${dump(d.condition, li)} ');
    case SwitchStatement sw:
      f.write(':expr ${dump(sw.expression, li)} '
          ':cases ${vec(sw.members.map((m) => dump(m, li)))} ');
    case SwitchCase c:
      f.write(':labels ${_labels(c.labels, li)} '
          ':expr ${dump(c.expression, li)} '
          ':stmts ${vec(c.statements.map((st) => dump(st, li)))} ');
    case SwitchPatternCase c:
      f.write(':labels ${_labels(c.labels, li)} '
          ':pattern ${dump(c.guardedPattern, li)} '
          ':stmts ${vec(c.statements.map((st) => dump(st, li)))} ');
    case SwitchDefault d:
      f.write(':labels ${_labels(d.labels, li)} '
          ':stmts ${vec(d.statements.map((st) => dump(st, li)))} ');
    case TryStatement t:
      f.write(':body ${dump(t.body, li)} '
          ':catches ${vec(t.catchClauses.map((c) => dump(c, li)))} '
          ':finally ${t.finallyBlock == null ? 'nil' : dump(t.finallyBlock!, li)} ');
    case CatchClause c:
      f.write(':exception-type ${c.exceptionType == null ? 'nil' : dump(c.exceptionType!, li)} '
          ':exception-var ${c.exceptionParameter == null ? 'nil' : s(c.exceptionParameter!.name.lexeme)} '
          ':stack-var ${c.stackTraceParameter == null ? 'nil' : s(c.stackTraceParameter!.name.lexeme)} '
          ':body ${dump(c.body, li)} ');
    case BreakStatement br:
      f.write(':label ${br.label == null ? 'nil' : s(br.label!.name.lexeme)} ');
    case ContinueStatement c:
      f.write(':label ${c.label == null ? 'nil' : s(c.label!.name.lexeme)} ');
    case LabeledStatement l:
      f.write(':labels ${_labels(l.labels, li)} '
          ':stmt ${dump(l.statement, li)} ');
    case Label l:
      f.write(':id ${s(l.name.lexeme)} ');
    case YieldStatement y:
      f.write(':star ${b(y.star != null)} :expr ${dump(y.expression, li)} ');
    case AssertStatement a:
      f.write(':cond ${dump(a.condition, li)} '
          ':message ${a.message == null ? 'nil' : dump(a.message!, li)} ');
    case EmptyStatement():
      f.write(' ');
    case FunctionDeclarationStatement fd:
      f.write(':fn ${dump(fd.functionDeclaration, li)} ');
    case PatternVariableDeclarationStatement pv:
      f.write(':decl ${dump(pv.declaration, li)} ');
    case PatternVariableDeclaration pd:
      f.write(':final ${b(pd.keyword.lexeme == 'final')} '
          ':pattern ${dump(pd.pattern, li)} '
          ':expr ${dump(pd.expression, li)} ');
    case PatternAssignment pa:
      f.write(':pattern ${dump(pa.pattern, li)} '
          ':expr ${dump(pa.expression, li)} ');

    // ---------------------------------------------------------------------
    // Expressions
    // ---------------------------------------------------------------------
    case SimpleIdentifier si:
      f.write(':id ${s(si.name)} ');
    case PrefixedIdentifier pi:
      f.write(':prefix ${s(pi.prefix.name)} :id ${s(pi.identifier.name)} ');
    case PropertyAccess pa:
      f.write(':target ${pa.target == null ? 'nil' : dump(pa.target!, li)} '
          ':op ${_tok(pa.operator)} '
          ':id ${s(pa.propertyName.name)} '
          ':null-aware ${b(pa.isNullAware)} ');
    case MethodInvocation mi:
      f.write(':target ${mi.target == null ? 'nil' : dump(mi.target!, li)} '
          ':op ${_tok(mi.operator)} '
          ':id ${s(mi.methodName.name)} '
          ':type-args ${_typeArgs(mi.typeArguments, li)} '
          ':args ${dump(mi.argumentList, li)} '
          ':null-aware ${b(mi.isNullAware)} ');
    case InstanceCreationExpression ic:
      f.write(':const ${b(ic.isConst)} '
          ':ctor ${_ctorName(ic.constructorName, li)} '
          ':args ${dump(ic.argumentList, li)} ');
    case ConstructorName cn:
      f.write(':type ${dump(cn.type, li)} :name ${cn.name == null ? 'nil' : s(cn.name!.name)} ');
    case FunctionExpressionInvocation fei:
      f.write(':fn ${dump(fei.function, li)} '
          ':type-args ${_typeArgs(fei.typeArguments, li)} '
          ':args ${dump(fei.argumentList, li)} ');
    case IndexExpression ie:
      f.write(':target ${ie.target == null ? 'nil' : dump(ie.target!, li)} '
          ':index ${dump(ie.index, li)} '
          ':null-aware ${b(ie.isNullAware)} ');
    case CascadeExpression ce:
      f.write(':target ${dump(ce.target, li)} '
          ':null-aware ${b(ce.isNullAware)} '
          ':sections ${vec(ce.cascadeSections.map((c) => dump(c, li)))} ');
    case AssignmentExpression ae:
      f.write(':lhs ${dump(ae.leftHandSide, li)} '
          ':op ${_tok(ae.operator)} '
          ':rhs ${dump(ae.rightHandSide, li)} ');
    case BinaryExpression be:
      f.write(':op ${_tok(be.operator)} '
          ':lhs ${dump(be.leftOperand, li)} '
          ':rhs ${dump(be.rightOperand, li)} ');
    case PrefixExpression pe:
      f.write(':op ${_tok(pe.operator)} :operand ${dump(pe.operand, li)} ');
    case PostfixExpression po:
      f.write(':op ${_tok(po.operator)} :operand ${dump(po.operand, li)} ');
    case ConditionalExpression ce:
      f.write(':cond ${dump(ce.condition, li)} '
          ':then ${dump(ce.thenExpression, li)} '
          ':else ${dump(ce.elseExpression, li)} ');
    case IsExpression isx:
      f.write(':expr ${dump(isx.expression, li)} '
          ':negated ${b(isx.notOperator != null)} '
          ':type ${dump(isx.type, li)} ');
    case AsExpression ax:
      f.write(':expr ${dump(ax.expression, li)} :type ${dump(ax.type, li)} ');
    case AwaitExpression aw:
      f.write(':expr ${dump(aw.expression, li)} ');
    case ThrowExpression te:
      f.write(':expr ${dump(te.expression, li)} ');
    case ParenthesizedExpression pe:
      f.write(':expr ${dump(pe.expression, li)} ');
    case FunctionExpression fe:
      f.write(':type-params ${fe.typeParameters == null ? 'nil' : vec(fe.typeParameters!.typeParameters.map((t) => dump(t, li)))} '
          ':params ${fe.parameters == null ? 'nil' : dump(fe.parameters!, li)} '
          ':body ${dump(fe.body, li)} ');
    case NamedArgument na:
      f.write(':name ${s(na.name.lexeme)} :expr ${dump(na.argumentExpression, li)} ');
    case ArgumentList al:
      f.write(':args ${vec(al.arguments.map((a) => dump(a, li)))} ');
    case SuperExpression():
      f.write(' ');
    case ThisExpression():
      f.write(' ');
    case RethrowExpression():
      f.write(' ');
    case SwitchExpression sx:
      f.write(':expr ${dump(sx.expression, li)} '
          ':cases ${vec(sx.cases.map((c) => dump(c, li)))} ');
    case SwitchExpressionCase sc:
      f.write(':pattern ${dump(sc.guardedPattern, li)} '
          ':expr ${dump(sc.expression, li)} ');

    // ---------------------------------------------------------------------
    // Literals
    // ---------------------------------------------------------------------
    case IntegerLiteral il:
      f.write(':raw ${_tok(il.literal)} :value ${i(il.value)} ');
    case DoubleLiteral dl:
      f.write(':raw ${_tok(dl.literal)} :value ${dl.value.toString()} ');
    case BooleanLiteral bl:
      f.write(':value ${b(bl.value)} ');
    case NullLiteral():
      f.write(' ');
    case SimpleStringLiteral sl:
      f.write(':raw ${_tok(sl.literal)} :value ${sQ(sl.value)} '
          ':raw-string ${b(sl.isRaw)} :multiline ${b(sl.isMultiline)} ');
    case AdjacentStrings ajs:
      f.write(':parts ${vec(ajs.strings.map((st) => dump(st, li)))} ');
    case StringInterpolation sip:
      f.write(':parts ${vec(sip.elements.map((el) => dump(el, li)))} ');
    case InterpolationString istr:
      f.write(':raw ${_tok(istr.contents)} :value ${s(istr.value)} ');
    case InterpolationExpression iexp:
      f.write(':brace ${b(iexp.leftBracket.lexeme == '{')} '
          ':expr ${dump(iexp.expression, li)} ');

    // ---------------------------------------------------------------------
    // Collections
    // ---------------------------------------------------------------------
    case ListLiteral ll:
      f.write(':const ${b(ll.isConst)} '
          ':type-args ${_typeArgs(ll.typeArguments, li)} '
          ':elements ${vec(ll.elements.map((el) => dump(el, li)))} ');
    case SetOrMapLiteral sm:
      // isMap/isSet require resolution; infer the kind syntactically
      final bool hasEntry = sm.elements.any((e) => e is MapLiteralEntry);
      final bool hasValue = sm.elements.any((e) =>
          e is! SpreadElement && e is! NullAwareElement);
      final int nTypeArgs = sm.typeArguments?.arguments.length ?? 0;
      final String kind = hasEntry
          ? ':map'
          : hasValue
              ? ':set'
              : nTypeArgs == 2
                  ? ':map'
                  : nTypeArgs == 1
                      ? ':set'
                      : sm.elements.isEmpty
                          ? ':map' // bare {} is an empty map in Dart
                          : ':unknown';
      f.write(':kind $kind '
          ':const ${b(sm.isConst)} '
          ':type-args ${_typeArgs(sm.typeArguments, li)} '
          ':elements ${vec(sm.elements.map((el) => dump(el, li)))} ');
    case MapLiteralEntry me:
      f.write(':key ${dump(me.key, li)} '
          ':key-null-aware ${b(me.keyQuestion != null)} '
          ':value ${dump(me.value, li)} '
          ':value-null-aware ${b(me.valueQuestion != null)} ');
    case RecordLiteral rl:
      f.write(':const ${b(rl.isConst)} '
          ':fields ${vec(rl.fields.map((fld) => fld is RecordLiteralNamedField
              ? '{:ast :RecordNamedField :name ${s(fld.name.lexeme)} :expr ${dump(fld.fieldExpression, li)} :src ${srcOf(fld, li)}}'
              : dump(fld.fieldExpression, li)))} ');
    case SpreadElement spe:
      f.write(':null-aware ${b(spe.isNullAware)} :expr ${dump(spe.expression, li)} ');
    case NullAwareElement nae:
      f.write(':expr ${dump(nae.value, li)} ');
    case IfElement ie:
      f.write(':cond ${dump(ie.expression, li)} '
          ':case ${ie.caseClause == null ? 'nil' : dump(ie.caseClause!, li)} '
          ':then ${dump(ie.thenElement, li)} '
          ':else ${ie.elseElement == null ? 'nil' : dump(ie.elseElement!, li)} ');
    case ForElement fe:
      f.write(':loop ${dump(fe.forLoopParts, li)} :body ${dump(fe.body, li)} ');

    // ---------------------------------------------------------------------
    // Function bodies
    // ---------------------------------------------------------------------
    case BlockFunctionBody bf:
      f.write(':async ${_tok(bf.keyword)} :block ${dump(bf.block, li)} ');
    case ExpressionFunctionBody ef:
      f.write(':async ${_tok(ef.keyword)} :expr ${dump(ef.expression, li)} ');
    case EmptyFunctionBody():
      f.write(' ');
    case NativeFunctionBody nf:
      f.write(':native ${nf.stringLiteral is SimpleStringLiteral ? s((nf.stringLiteral as SimpleStringLiteral).value) : 'nil'} ');

    // ---------------------------------------------------------------------
    // Type annotations
    // ---------------------------------------------------------------------
    case NamedType nt:
      f.write(':prefix ${nt.importPrefix == null ? 'nil' : s(nt.importPrefix!.name.lexeme)} '
          ':id ${s(nt.name.lexeme)} '
          ':type-args ${_typeArgs(nt.typeArguments, li)} '
          ':nullable ${b(nt.question != null)} ');
    case GenericFunctionType gf:
      f.write(':return-type ${gf.returnType == null ? 'nil' : dump(gf.returnType!, li)} '
          ':type-params ${gf.typeParameters == null ? 'nil' : vec(gf.typeParameters!.typeParameters.map((t) => dump(t, li)))} '
          ':params ${dump(gf.parameters, li)} '
          ':nullable ${b(gf.question != null)} ');
    case RecordTypeAnnotation rt:
      f.write(':positional ${vec(rt.positionalFields.map((pf) => '{:ast :RecordTypePositionalField :name ${pf.name == null ? 'nil' : s(pf.name!.lexeme)} :type ${dump(pf.type, li)} :src ${srcOf(pf, li)}}'))} '
          ':named ${rt.namedFields == null ? 'nil' : vec(rt.namedFields!.fields.map((nf) => '{:ast :RecordTypeNamedField :name ${s(nf.name.lexeme)} :type ${dump(nf.type, li)} :src ${srcOf(nf, li)}}'))} '
          ':nullable ${b(rt.question != null)} ');
    case Annotation an:
      f.write(':name ${dump(an.name, li)} '
          ':ctor ${an.constructorName == null ? 'nil' : s(an.constructorName!.name)} '
          ':type-args ${_typeArgs(an.typeArguments, li)} '
          ':args ${an.arguments == null ? 'nil' : dump(an.arguments!, li)} ');

    // ---------------------------------------------------------------------
    // Patterns (Dart 3)
    // ---------------------------------------------------------------------
    case ConstantPattern cp:
      f.write(':expr ${dump(cp.expression, li)} ');
    case DeclaredVariablePattern vp:
      f.write(':name ${s(vp.name.lexeme)} '
          ':keyword ${_tok(vp.keyword)} '
          ':type ${vp.type == null ? 'nil' : dump(vp.type!, li)} ');
    case AssignedVariablePattern vp:
      f.write(':name ${s(vp.name.lexeme)} ');
    case ListPattern lp:
      f.write(':type-args ${_typeArgs(lp.typeArguments, li)} '
          ':elements ${vec(lp.elements.map((el) => dump(el, li)))} ');
    case MapPattern mp:
      f.write(':type-args ${_typeArgs(mp.typeArguments, li)} '
          ':entries ${vec(mp.elements.map((el) => dump(el, li)))} ');
    case MapPatternEntry mpe:
      f.write(':key ${dump(mpe.key, li)} :value ${dump(mpe.value, li)} ');
    case RecordPattern rp:
      f.write(':fields ${vec(rp.fields.map((fld) => dump(fld, li)))} ');
    case ObjectPattern op:
      f.write(':type ${dump(op.type, li)} '
          ':fields ${vec(op.fields.map((fld) => dump(fld, li)))} ');
    case PatternField pf:
      f.write(':name ${pf.name == null || pf.name!.name == null ? 'nil' : s(pf.name!.name!.lexeme)} '
          ':pattern ${dump(pf.pattern, li)} ');
    case LogicalAndPattern lap:
      f.write(':lhs ${dump(lap.leftOperand, li)} :rhs ${dump(lap.rightOperand, li)} ');
    case LogicalOrPattern lop:
      f.write(':lhs ${dump(lop.leftOperand, li)} :rhs ${dump(lop.rightOperand, li)} ');
    case RelationalPattern rp2:
      f.write(':op ${_tok(rp2.operator)} :operand ${dump(rp2.operand, li)} ');
    case CastPattern cpx:
      f.write(':pattern ${dump(cpx.pattern, li)} :type ${dump(cpx.type, li)} ');
    case NullAssertPattern nap:
      f.write(':pattern ${dump(nap.pattern, li)} ');
    case NullCheckPattern ncp:
      f.write(':pattern ${dump(ncp.pattern, li)} ');
    case ParenthesizedPattern pp:
      f.write(':pattern ${dump(pp.pattern, li)} ');
    case WildcardPattern wp:
      f.write(':type ${wp.type == null ? 'nil' : dump(wp.type!, li)} ');
    case RestPatternElement rpe:
      f.write(':pattern ${rpe.pattern == null ? 'nil' : dump(rpe.pattern!, li)} ');
    case GuardedPattern gp:
      f.write(':pattern ${dump(gp.pattern, li)} '
          ':when ${gp.whenClause == null ? 'nil' : dump(gp.whenClause!.expression, li)} ');
    case WhenClause wc:
      f.write(':expr ${dump(wc.expression, li)} ');

    default:
      return dumpGeneric(node, li);
  }
  return '{:ast ${kw(node)} $f:src ${srcOf(node, li)}}';
}

// ---------------------------------------------------------------------------
// helpers
// ---------------------------------------------------------------------------

String _combinatorNames(NodeList<Combinator> combinators, String kind) {
  final names = <String>[];
  for (final c in combinators) {
    if (kind == 'Show' && c is ShowCombinator) {
      names.addAll(c.shownNames.map((n) => s(n.name)));
    }
    if (kind == 'Hide' && c is HideCombinator) {
      names.addAll(c.hiddenNames.map((n) => s(n.name)));
    }
  }
  return vec(names);
}

String _classModifier(dynamic c) {
  if (c.baseKeyword != null) return ':base';
  if (c.finalKeyword != null) return ':final';
  if (c.interfaceKeyword != null) return ':interface';
  if (c.sealedKeyword != null) return ':sealed';
  if (c.mixinKeyword != null) return ':mixin';
  return 'nil';
}

String _declaredIdent(DeclaredIdentifier d, LineInfo li) {
  return '{:ast :DeclaredIdentifier '
      ':final ${b(d.isFinal)} :const ${b(d.isConst)} '
      ':type ${d.type == null ? 'nil' : dump(d.type!, li)} '
      ':name ${s(d.name.lexeme)} :src ${srcOf(d, li)}}';
}

String _labels(List<Label> labels, LineInfo li) =>
    vec(labels.map((l) => s(l.name.lexeme)));

String _ctorName(ConstructorName cn, LineInfo li) {
  return '{:ast :ConstructorName '
      ':type ${dump(cn.type, li)} '
      ':name ${cn.name == null ? 'nil' : s(cn.name!.name)} '
      ':src ${srcOf(cn, li)}}';
}

String dumpGeneric(AstNode node, LineInfo li) {
  final parts = <String>[];
  for (final e in node.childEntities) {
    if (e is AstNode) {
      parts.add(dump(e, li));
    } else if (e is Token) {
      parts.add('{:ast :Token :lexeme ${s(e.lexeme)} :src ${src(e, li)}}');
    }
  }
  return '{:ast ${kw(node)} :generic true :parts ${vec(parts)} :src ${srcOf(node, li)}}';
}
