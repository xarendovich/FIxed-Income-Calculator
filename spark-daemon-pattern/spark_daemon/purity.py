"""Static purity check of a daemon's code, run before the code is imported.

Rules for daemon.py (contract v1):
- imports only from ALLOWED_IMPORTS (no os, io, pathlib, subprocess, socket, time, ctypes...);
  all I/O goes through the ctx object handed to sense();
- defines sense(ctx) and decide(prev, snapshot); digest(snapshot, recent) is optional; each
  is defined exactly once, undecorated, and never reassigned;
- never names open, exec, eval, compile, __import__, input, print, breakpoint, getattr,
  setattr, delattr, globals, locals, vars, exit or quit - not only as a call, since
  `o = open; o(path)` is the same call under another name;
- no dunder names or attributes, no private attributes (`ctx._policy`), and none of the
  interpreter's introspection attributes (`gen.gi_frame.f_builtins` reaches the real
  builtins without naming them); no dynamic attribute access helpers (attrgetter,
  methodcaller, string.Formatter, typing.get_type_hints, which evaluates strings);
- no format string that reaches a dunder field ("{0.__class__}");
- no global/nonlocal statements, no async code, no star imports;
- top level holds only imports, definitions, constant assignments and a docstring, and any
  call made while importing is one of TOPLEVEL_CALLS (re.compile, frozenset, dataclass...),
  so importing the module has no side effects.

This is a syntactic check that stops honest mistakes, catches the known ways around it,
and makes review easy. It is not a sandbox: Landlock, the audit hook and the systemd unit
are the later layers, and each is tested on its own by the battery.
"""

import ast
import re

ALLOWED_IMPORTS = frozenset({
    "__future__", "bisect", "collections", "dataclasses", "enum", "functools", "hashlib",
    "heapq", "itertools", "json", "math", "operator", "re", "statistics", "string",
    "textwrap", "typing",
})
FORBIDDEN_CALLS = frozenset({
    "open", "exec", "eval", "compile", "__import__", "input", "print", "breakpoint",
    "getattr", "setattr", "delattr", "globals", "locals", "vars", "exit", "quit", "help",
})
# Names that give dynamic attribute access or evaluate strings; refused as names and as
# attributes (operator.attrgetter, string.Formatter().get_field, typing.get_type_hints).
FORBIDDEN_NAMES = frozenset({
    "attrgetter", "methodcaller", "Formatter", "get_field", "get_type_hints", "ForwardRef",
})
# Interpreter introspection attributes: frames, code objects and tracebacks lead back to the
# real builtins and module globals.
FORBIDDEN_ATTRIBUTES = frozenset({
    "gi_frame", "gi_code", "gi_yieldfrom", "gi_running", "cr_frame", "cr_code", "cr_await",
    "cr_origin", "ag_frame", "ag_code", "ag_await", "tb_frame", "tb_next", "tb_lasti",
    "f_globals", "f_builtins", "f_locals", "f_back", "f_code", "f_trace", "f_lasti",
    "co_code", "co_consts", "co_names", "mro", "func_globals", "with_traceback",
})
# Calls allowed while the module is imported (by their final name: re.compile -> compile).
# Anything else at import time is a side effect, or a hang before the watchdog starts.
TOPLEVEL_CALLS = frozenset({
    "compile", "frozenset", "tuple", "dict", "set", "list", "int", "str", "bool", "bytes",
    "range", "sorted", "len", "min", "max", "sum", "zip", "enumerate", "reversed", "chr",
    "ord", "namedtuple", "NamedTuple", "dataclass", "field", "total_ordering", "unique",
    "lru_cache", "cache", "partial", "Enum", "IntEnum", "StrEnum", "Flag", "IntFlag", "auto",
    "Counter", "OrderedDict", "defaultdict", "deque", "TypeVar",
})
REQUIRED = {"sense": 1, "decide": 2}
OPTIONAL = {"digest": 2}
_DUNDER_FORMAT_FIELD = re.compile(r"\{[^{}]*__")


def _is_dunder(name: str) -> bool:
    return name.startswith("__") and name.endswith("__")


def _call_name(func) -> str:
    if isinstance(func, ast.Name):
        return func.id
    if isinstance(func, ast.Attribute):
        return func.attr
    return ""


def _import_time_nodes(tree):
    """Every node evaluated when the module is imported: top-level statements, class bodies,
    decorators, default values and annotations - but not function bodies or lambdas."""
    stack = list(tree.body)
    while stack:
        node = stack.pop()
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            stack.extend(node.decorator_list)
            stack.extend(node.args.defaults)
            stack.extend(d for d in node.args.kw_defaults if d is not None)
            continue
        if isinstance(node, ast.Lambda):
            stack.extend(node.args.defaults)
            continue
        yield node
        stack.extend(ast.iter_child_nodes(node))


def check_source(source: str, filename: str = "daemon.py") -> list:
    problems = []

    def fail(node, message):
        problems.append(f"{filename}:{getattr(node, 'lineno', 0)}: {message}")

    try:
        tree = ast.parse(source, filename=filename)
    except SyntaxError as e:
        return [f"{filename}:{e.lineno}: syntax error"]

    functions = {}
    entry_points = {**REQUIRED, **OPTIONAL}
    for i, node in enumerate(tree.body):
        if isinstance(node, ast.FunctionDef):
            if node.name in functions and node.name in entry_points:
                fail(node, f"{node.name}() is defined more than once")
            functions[node.name] = node
            continue
        if isinstance(node, (ast.Import, ast.ImportFrom)):
            continue
        if isinstance(node, ast.ClassDef):
            for item in node.body:
                if not isinstance(item, (ast.FunctionDef, ast.Assign, ast.AnnAssign, ast.Pass)) and not (
                        isinstance(item, ast.Expr) and isinstance(item.value, ast.Constant)):
                    fail(item, f"{type(item).__name__} in a class body is not allowed (it runs at import)")
            continue
        if isinstance(node, (ast.Assign, ast.AnnAssign)):
            targets = node.targets if isinstance(node, ast.Assign) else [node.target]
            for target in targets:
                for name in ast.walk(target):
                    if isinstance(name, ast.Name) and name.id in entry_points:
                        fail(node, f"{name.id} must be defined with def, not assigned")
            continue
        if i == 0 and isinstance(node, ast.Expr) and isinstance(node.value, ast.Constant) \
                and isinstance(node.value.value, str):
            continue
        fail(node, f"top-level {type(node).__name__} is not allowed (import must have no side effects)")

    for node in _import_time_nodes(tree):
        if isinstance(node, ast.Call) and _call_name(node.func) not in TOPLEVEL_CALLS:
            fail(node, f"call to {_call_name(node.func) or 'an expression'}() at import time is not allowed")
    for node in ast.walk(tree):
        # A bare decorator (@name) is applied - called - at import time too.
        for deco in getattr(node, "decorator_list", ()):
            if not isinstance(deco, ast.Call) and _call_name(deco) not in TOPLEVEL_CALLS:
                fail(deco, f"call to {_call_name(deco) or 'an expression'}() at import time is not allowed")

    for name, arity in entry_points.items():
        fn = functions.get(name)
        if fn is None:
            if name in REQUIRED:
                problems.append(f"{filename}: missing required function {name}()")
            continue
        args = fn.args
        positional = len(args.posonlyargs) + len(args.args)
        if positional != arity or args.vararg or args.kwarg or args.kwonlyargs:
            fail(fn, f"{name}() must take exactly {arity} positional argument(s)")
        if fn.decorator_list:
            fail(fn, f"{name}() must not be decorated")

    call_funcs = {id(n.func) for n in ast.walk(tree) if isinstance(n, ast.Call)}
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for alias in node.names:
                if alias.name.split(".")[0] not in ALLOWED_IMPORTS:
                    fail(node, f"import of {alias.name!r} is not allowed")
        elif isinstance(node, ast.ImportFrom):
            if node.level:
                fail(node, "relative imports are not allowed")
            elif (node.module or "").split(".")[0] not in ALLOWED_IMPORTS:
                fail(node, f"import from {node.module!r} is not allowed")
            for alias in node.names:
                if alias.name == "*":
                    fail(node, "star imports are not allowed")
                elif alias.name in FORBIDDEN_NAMES or alias.name.startswith("_"):
                    fail(node, f"import of {alias.name!r} is not allowed")
        elif isinstance(node, ast.Call):
            func = node.func
            if isinstance(func, ast.Name) and func.id in FORBIDDEN_CALLS:
                fail(node, f"call to {func.id}() is not allowed")
        elif isinstance(node, ast.Attribute):
            attr = node.attr
            if _is_dunder(attr):
                fail(node, f"dunder attribute {attr!r} is not allowed")
            elif attr.startswith("_"):
                fail(node, f"private attribute {attr!r} is not allowed")
            elif attr in FORBIDDEN_ATTRIBUTES:
                fail(node, f"introspection attribute {attr!r} is not allowed")
            elif attr in FORBIDDEN_NAMES:
                fail(node, f"{attr!r} is not allowed (dynamic attribute access or evaluation)")
        elif isinstance(node, ast.Name):
            if _is_dunder(node.id):
                fail(node, f"dunder name {node.id!r} is not allowed")
            elif node.id in FORBIDDEN_NAMES:
                fail(node, f"{node.id!r} is not allowed (dynamic attribute access or evaluation)")
            elif node.id in FORBIDDEN_CALLS and id(node) not in call_funcs:
                fail(node, f"{node.id!r} must not be referenced (aliasing a forbidden builtin)")
        elif isinstance(node, ast.Constant) and isinstance(node.value, str):
            if _DUNDER_FORMAT_FIELD.search(node.value):
                fail(node, "format fields that reach dunder attributes are not allowed")
        elif isinstance(node, (ast.Global, ast.Nonlocal)):
            fail(node, "global and nonlocal state are not allowed; state flows through prev and snapshot")
        elif isinstance(node, (ast.AsyncFunctionDef, ast.Await, ast.AsyncFor, ast.AsyncWith)):
            fail(node, "async code is not allowed")
    return problems


def check_file(path: str) -> list:
    try:
        with open(path, "rb") as fh:
            raw = fh.read(262145)
    except OSError as e:
        return [f"{path}: cannot read ({e.strerror})"]
    if len(raw) > 262144:
        return [f"{path}: larger than 256 KiB"]
    try:
        source = raw.decode("utf-8")
    except UnicodeDecodeError:
        return [f"{path}: not valid UTF-8"]
    return check_source(source, filename=path.rsplit("/", 1)[-1])
