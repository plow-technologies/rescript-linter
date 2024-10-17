//@ts-check
var child_process = require("child_process");
var assert = require("assert");
var fs = require("fs");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;

console.log(
  child_process.execSync(rescript_exe, { encoding: "utf8", cwd: "./a" })
);

assert(
  fs.existsSync("./node_modules/c/lib/js/tests/test.mjs"),
  "dev files of module 'c' were not built by 'a' even though 'c' is a transitive pinned dependency of 'a' through 'b'"
);
