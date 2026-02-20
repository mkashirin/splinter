const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    const exe_check = b.addExecutable(.{
        .name = "splinter",
        .root_module = exe_mod,
    });
    const check = b.step("check", "Check if Splinter compiles");

    check.dependOn(&exe_check.step);
    const exe = b.addExecutable(.{
        .name = "splinter",
        .root_module = exe_mod,
    });
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cmd.addArgs(args);
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const test_step = b.step("test", "Run unit tests");
    // Renderer includes all three files, that need to be tested:
    // - Tokenizer.zig,
    // - Parser.zig,
    // - Renderer.zig (itself).
    // Since that, the only file, that needs to be a test root is Renderer.zig.
    const renderer_root_module = b.createModule(.{
        .root_source_file = b.path("src/Renderer.zig"),
        .target = target,
        .optimize = optimize,
    });
    const renderer_unit_tests =
        b.addTest(.{ .root_module = renderer_root_module });
    const run_renderer_unit_testes = b.addRunArtifact(renderer_unit_tests);
    test_step.dependOn(&run_renderer_unit_testes.step);
}
