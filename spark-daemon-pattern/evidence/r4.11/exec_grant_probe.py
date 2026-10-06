"""R-2b: what Landlock needs so that a process may execute exactly the executables it declares.

In forked children, applies a domain with read (no execute) on the system paths and execute on
nothing, or on one executable only, or on that executable and its ELF interpreter, then tries to
run /usr/bin/git, /usr/bin/true and /usr/bin/sleep, and to import native modules not loaded yet."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
from spark_daemon import landlock  # noqa: E402


def elf_interp(path):
    import struct
    with open(path, "rb") as fh:
        data = fh.read(65536)
    if data[:4] != b"\x7fELF" or data[4] != 2:
        return None
    phoff, = struct.unpack_from("<Q", data, 0x20)
    phentsize, phnum = struct.unpack_from("<HH", data, 0x36)
    for i in range(phnum):
        off = phoff + i * phentsize
        ptype, = struct.unpack_from("<I", data, off)
        if ptype == 3:                                   # PT_INTERP
            p_offset, = struct.unpack_from("<Q", data, off + 8)
            p_filesz, = struct.unpack_from("<Q", data, off + 32)
            return data[p_offset:p_offset + p_filesz].rstrip(b"\0").decode()
    return None


def trial(label, execute_on):
    r, w = os.pipe()
    pid = os.fork()
    if pid == 0:
        os.close(r)
        rules = [(p, landlock.READ_NO_EXEC) for p in landlock.system_read_paths() if os.path.exists(p)]
        rules += [(p, landlock.ACCESS_FS_EXECUTE | landlock.ACCESS_FS_READ_FILE) for p in execute_on]
        if os.path.exists(os.devnull):
            rules.append((os.devnull, landlock.ACCESS_FS_READ_FILE | landlock.ACCESS_FS_WRITE_FILE))
        landlock.restrict_self(rules, scope_signals=True)
        out = []
        for exe in ("/usr/bin/git", "/usr/bin/true", "/usr/bin/sleep"):
            child = os.fork()
            if child == 0:
                devnull = os.open(os.devnull, os.O_WRONLY)
                os.dup2(devnull, 1)
                os.dup2(devnull, 2)
                try:
                    os.execv(exe, [exe, "--version"])
                except OSError as e:
                    os._exit(100 + e.errno % 100)
            code = os.waitpid(child, 0)[1] >> 8
            out.append(f"{os.path.basename(exe)}={'ran' if code < 100 else 'refused(errno %d)' % (code - 100)}")
        try:
            import _decimal, _bz2, _lzma, _sqlite3  # noqa: F401  native modules not loaded before
            out.append("native imports=ok")
        except ImportError as e:
            out.append(f"native imports=FAILED ({e})")
        os.write(w, " ".join(out).encode())
        os._exit(0)
    os.close(w)
    result = os.read(r, 4096).decode()
    os.waitpid(pid, 0)
    print(f"{label:<44} {result}")


git = os.path.realpath("/usr/bin/git")
interp = elf_interp(git)
print(f"Landlock ABI {landlock.abi_version()}; git -> {git}; ELF interpreter {interp}")
trial("execute on nothing", [])
trial("execute on git only", [git])
trial("execute on git and its ELF interpreter", [git, os.path.realpath(interp)])


def loader_trick():
    """With execute on git and the loader only, run the loader directly on another program."""
    r, w = os.pipe()
    pid = os.fork()
    if pid == 0:
        os.close(r)
        rules = [(p, landlock.READ_NO_EXEC) for p in landlock.system_read_paths() if os.path.exists(p)]
        rules += [(p, landlock.ACCESS_FS_EXECUTE | landlock.ACCESS_FS_READ_FILE) for p in (git, os.path.realpath(interp))]
        rules.append((os.devnull, landlock.ACCESS_FS_READ_FILE | landlock.ACCESS_FS_WRITE_FILE))
        landlock.restrict_self(rules, scope_signals=True)
        child = os.fork()
        if child == 0:
            devnull = os.open(os.devnull, os.O_WRONLY)
            os.dup2(devnull, 1)
            try:
                os.execv(os.path.realpath(interp), [interp, "/usr/bin/true"])
            except OSError as e:
                os._exit(100 + e.errno % 100)
        code = os.waitpid(child, 0)[1] >> 8
        os.write(w, (f"loader run directly on /usr/bin/true: exit {code}"
                     + (" (RAN: the loader is a way around an exact grant)" if code == 0 else "")).encode())
        os._exit(0)
    os.close(w)
    print(os.read(r, 4096).decode())
    os.waitpid(pid, 0)


loader_trick()
