//! Spawn Emacs and wait for it, so that macOS keeps attributing Emacs's file
//! accesses to this process (see `nix/darwin.nix`).
//! This must not `exec`: that would replace this binary, and with it the identity
//! that holds the Full Disk Access grant.

const std = @import("std");

// Re-render as a null-terminated literal for the C calls below.
const emacs_path = std.fmt.comptimePrint("{s}", .{@import("build_options").emacs_path});

const Pid = c_int;
const Argv = [*:null]?[*:0]const u8;

extern "c" fn _NSGetArgv() *Argv;
extern "c" fn _NSGetEnviron() *Argv;
extern "c" fn __error() *c_int;
extern "c" fn dprintf(fd: c_int, format: [*:0]const u8, ...) c_int;
extern "c" fn kill(pid: Pid, sig: c_int) c_int;
extern "c" fn posix_spawn(
    pid: *Pid,
    path: [*:0]const u8,
    file_actions: ?*const anyopaque,
    attrp: ?*const anyopaque,
    argv: Argv,
    envp: Argv,
) c_int;
extern "c" fn signal(sig: c_int, handler: *const fn (c_int) callconv(.c) void) ?*const anyopaque;
extern "c" fn waitpid(pid: Pid, status: *c_int, options: c_int) Pid;

const SIGHUP = 1;
const SIGINT = 2;
const SIGTERM = 15;
const EINTR = 4;

var child: Pid = 0;

fn forward(sig: c_int) callconv(.c) void {
    if (child > 0) _ = kill(child, sig);
}

pub fn main() u8 {
    const argv = _NSGetArgv().*;
    const environ = _NSGetEnviron().*;
    argv[0] = emacs_path;

    _ = signal(SIGHUP, forward);
    _ = signal(SIGINT, forward);
    _ = signal(SIGTERM, forward);

    const err = posix_spawn(&child, emacs_path, null, null, argv, environ);
    if (err != 0) {
        _ = dprintf(2, "emacs-launcher: cannot spawn %s: errno %d\n", emacs_path, err);
        return 127;
    }

    var status: c_int = 0;
    while (waitpid(child, &status, 0) < 0) {
        if (__error().* != EINTR) return 127;
    }

    // Mirror the WIFEXITED/WEXITSTATUS/WTERMSIG macros.
    const termsig = status & 0x7f;
    const code: c_int = if (termsig == 0) (status >> 8) & 0xff else 128 + termsig;
    return @intCast(code);
}
