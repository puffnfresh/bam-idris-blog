with import <nixpkgs> { };
with import <idrispkgs> { };

runCommand "dummy" {
  buildInputs = [
    (idrisWithPackages [ idris-commonmark idris-config idris-lens idris-posix ]) strace less # gdb gcc python
  ];
} ""
