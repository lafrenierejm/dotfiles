const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const emacs_path = b.option(
        []const u8,
        "emacs-path",
        "Absolute path of the Emacs executable to spawn",
    ) orelse @panic("missing required option -Demacs-path");

    const options = b.addOptions();
    options.addOption([]const u8, "emacs_path", emacs_path);

    const exe = b.addExecutable(.{
        .name = "emacs-launcher",
        .root_module = b.createModule(.{
            .root_source_file = b.path("main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    exe.root_module.addOptions("build_options", options);
    b.installArtifact(exe);
}
