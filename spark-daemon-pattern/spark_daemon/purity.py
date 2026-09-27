"""Static purity check of a daemon's code, run before the code is imported.

Rules for daemon.py (v1):
- imports only from ALLOWED_IMPORTS (no os, io, pathlib, subprocess, socket, time, ctypes...);
  all I/O goes through the ctx object handed to sense();
- defines sense(ctx) and decide(prev, snapshot); digest(snapshot, recent) is optional;
- no calls to open, exec, eval, compile, __import__, input, print, breakpoint, getattr,
  setattr, delattr, globals, locals or vars; no dunder names or attributes;
- no global/nonlocal statements, no async code, no star imports;
- top level holds only imports, definitions, constant assignments and a docstring, so
  importing the module has no side effects.

This is a syntactic check that stops honest mistakes and makes review easy. It is not a
sandbox; the audit hook and the systemd unit are the later layers.
"""

import ast

ALLOWED_IMPORTS = frozenset({
    "__future__", "bisect", "collections", "dataclasses", "enum", "functools", "hashlib",
    "heapq", "itertools", "json", "math", "operator", "re", "statistics", "string",
    "textwrap", "typing",
})
FORBIDDEN_CALLS = frozenset({
    "open", "exec", "eval", "compile", "__import__", "input", "print", "breakpoint",
    "getattr", "setattr", "delattr", "globals", "locals", "vars",
})
REQUIRED = {"sense": 1, "decide": 2}
OPTIONAL = {"digest": 2}


def check_source(source: str, filename: str = "daemon.py") -> list:
    problems = []

    def fail(node, message):
        problems.append(f"{filename}:{getattr(node, 'lineno', 0)}: {message}")

    try:
        tree = ast.parse(source, filename=filename)
    except SyntaxError as e:
        return [f"{filename}:{e.lineno}: syntax error"]

    functions = {}
    for i, node in enumerate(tree.body):
        if isinstance(node, (ast.Import, ast.ImportFrom, ast.FunctionDef, ast.ClassDef)):
            if isinstance(node, ast.FunctionDef):
                functions[node.name] = node
            continue
        if isinstance(node, (ast.Assign, ast.AnnAssign)):
            continue
        if i == 0 and isinstance(node, ast.Expr) and isinstance(node.value, ast.Constant) \
                and isinstance(node.value.value, str):
            continue
        fail(node, f"top-level {type(node).__name__} is not allowed (import must have no side effects)")

    for name, arity in {**REQUIRED, **OPTIONAL}.items():
        fn = functions.get(name)
        if fn is None:
            if name in REQUIRED:
                problems.append(f"{filename}: missing required function {name}()")
            continue
        args = fn.args
        positional = len(args.posonlyargs) + len(args.args)
        if positional != arity or args.vararg or args.kwarg or args.kwonlyargs:
            fail(fn, f"{name}() must take exactly {arity} positional argument(s)")

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
            if any(alias.name == "*" for alias in node.names):
                fail(node, "star imports are not allowed")
        elif isinstance(node, ast.Call):
            func = node.func
            if isinstance(func, ast.Name) and func.id in FORBIDDEN_CALLS:
                fail(node, f"call to {func.id}() is not allowed")
        elif isinstance(node, ast.Attribute):
            if node.attr.startswith("__") and node.attr.endswith("__"):
                fail(node, f"dunder attribute {node.attr!r} is not allowed")
        elif isinstance(node, ast.Name):
            if node.id.startswith("__") and node.id.endswith("__"):
                fail(node, f"dunder name {node.id!r} is not allowed")
        elif isinstance(node, (ast.Global, ast.Nonlocal)):
            fail(node, "global and nonlocal state are not allowed; state flows through prev and snapshot")
        elif isinstance(node, (ast.AsyncFunctionDef, ast.Await, ast.AsyncFor, ast.AsyncWith)):
            fail(node, "async code is not allowed")
        elif isinstance(node, ast.FunctionDef) and node.decorator_list and node.name in {**REQUIRED, **OPTIONAL}:
            fail(node, f"{node.name}() must not be decorated")
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
