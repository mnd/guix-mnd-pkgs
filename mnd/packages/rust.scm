;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Eric Le Bihan <eric.le.bihan.dev@free.fr>
;;; Copyright © 2016 ng0 <ng0@infotropique.org>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2018 Nikolai Merinov <nikolai.merinov@member.fsf.org>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (mnd packages rust)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26))

(define %cargo-reference-project-file "/dev/null")
(define %cargo-reference-hash
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

(define rust-bootstrap
  (package
    (name "rust-bootstrap")
    (version "1.22.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
 "https://static.rust-lang.org/dist/"
                    "rust-" version "-" %host-type ".tar.gz"))
              (sha256
               (base32
                (match %host-type
                  ("i686-unknown-linux-gnu"
                   "15zqbx86nm13d5vq2gm69b7av4vg479f74b5by64hs3bcwwm08pr")
                  ("x86_64-unknown-linux-gnu"
                   "1yll78x6b3abnvgjf2b66gvp6mmcb9y9jdiqcwhmgc0z0i0fix4c")
                  ("armv7-unknown-linux-gnueabihf"
                   "138a8l528kzp5wyk1mgjaxs304ac5ms8vlpq0ggjaznm6bn2j7a5")
                  ("aarch64-unknown-linux-gnu"
                   "0z6m9m1rx4d96nvybbfmpscq4dv616m615ijy16d5wh2vx0p4na8")
                  ("mips64el-unknown-linux-gnuabi64"
                   "07k4pcv7jvfa48cscdj8752lby7m7xdl88v3a6na1vs675lhgja2")
                  (_ ""))))))
    (build-system gnu-build-system)
    (native-inputs
     `(("patchelf" ,patchelf)))
    (inputs
     `(("gcc" ,(canonical-package gcc))
       ("gcc:lib" ,(canonical-package gcc) "lib")
       ("zlib" ,zlib)))
    (outputs '("out" "cargo"))
    (arguments
     `(#:tests? #f
       #:strip-binaries? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (cargo-out (assoc-ref outputs "cargo"))
                    (gcc:lib (assoc-ref inputs "gcc:lib"))
                    (libc (assoc-ref inputs "libc"))
                    (zlib (assoc-ref inputs "zlib"))
                    (ld-so (string-append libc ,(glibc-dynamic-linker)))
                    (rpath (string-append out "/lib:" zlib "/lib:"
                                          libc "/lib:" gcc:lib "/lib"))
                    (cargo-rpath (string-append cargo-out "/lib:" libc "/lib:"
                                                gcc:lib "/lib"))
                    (rustc (string-append out "/bin/rustc"))
                    (rustdoc (string-append out "/bin/rustdoc"))
                    (cargo (string-append cargo-out "/bin/cargo"))
                    (gcc (assoc-ref inputs "gcc")))
               ;; Install rustc/rustdoc
               (system* "bash" "install.sh"
                        (string-append "--prefix=" out)
                        (string-append "--components=rustc,"
                                       "rust-std-" %host-type))
               ;; Instal cargo
               (system* "bash" "install.sh"
                        (string-append "--prefix=" cargo-out)
                        (string-append "--components=cargo"))
               (for-each (lambda (file)
                           (system* "patchelf" "--set-rpath" rpath file))
                         (cons* rustc rustdoc (find-files out "\\.so$")))
               (system* "patchelf" "--set-rpath" cargo-rpath cargo)
               (for-each (lambda (file)
                           (system* "patchelf" "--set-interpreter" ld-so file))
                         (list rustc rustdoc cargo))
               ;; Rust requires a C toolchain for linking. The prebuilt
               ;; binaries expect a compiler called cc. Thus symlink gcc
               ;; to cc.
               (symlink (string-append gcc "/bin/gcc")
                        (string-append out "/bin/cc"))))))))
    (home-page "https://www.rust-lang.org")
    (synopsis "Prebuilt rust compiler and cargo package manager")
    (description "This package provides a pre-built @command{rustc} compiler
and a pre-built @command{cargo} package manaer, which can
in turn be used to build the final Rust.")
    (license license:asl2.0)))


(define (rust-source version hash)
  (origin
    (method url-fetch)
    (uri (string-append "https://static.rust-lang.org/dist/"
                        "rustc-" version "-src.tar.gz"))
    (sha256 (base32 hash))
    (modules '((guix build utils)))
    (snippet '(begin (delete-file-recursively "src/llvm") #t))))

(define-public rust-1.23
  (package
    (name "rust")
    (version "1.23.0")
    (source (rust-source version "14fb8vhjzsxlbi6yrn1r6fl5dlbdd1m92dn5zj5gmzfwf4w9ar3l"))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison) ; For the tests
       ("cmake" ,cmake)
       ("flex" ,flex) ; For the tests
       ("gdb" ,gdb)   ; For the tests
       ("git" ,git)
       ("procps" ,procps) ; For the tests
       ("python-2" ,python-2)
       ("rustc-bootstrap" ,rust-bootstrap)
       ("cargo-bootstrap" ,rust-bootstrap "cargo")
       ("pkg-config" ,pkg-config) ; For "cargo"
       ("which" ,which)))
    (inputs
     `(("jemalloc" ,jemalloc-4.5.0)
       ("llvm" ,llvm-3.9.1)
       ("openssl" ,openssl)
       ("libcurl" ,curl))) ; For "cargo"
    (outputs '("out" "doc" "cargo"))
    (arguments
     `(#:imported-modules ,%cargo-build-system-modules ;for `generate-checksums'
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           (lambda _
             (setenv "SHELL" (which "sh"))
             (setenv "CONFIG_SHELL" (which "sh"))
             ;; guix llvm-3.9.1 package installs only shared libraries
             (setenv "LLVM_LINK_SHARED" "1")
             #t))
         (add-after 'unpack 'patch-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((bash (assoc-ref inputs "bash")))
               (substitute* "src/libstd/process.rs"
                 ;; The newline is intentional.
                 ;; There's a line length "tidy" check in Rust which would
                 ;; fail otherwise.
                 (("\"/bin/sh\"") (string-append "\n\"" bash "/bin/sh\"")))
               (substitute* "src/libstd/net/tcp.rs"
                 ;; There is no network in build environment
                 (("fn connect_timeout_unroutable")
                  "#[ignore]\nfn connect_timeout_unroutable"))
               ;; <https://lists.gnu.org/archive/html/guix-devel/2017-06/msg00222.html>
               (substitute* "src/libstd/sys/unix/process/process_common.rs"
                 (("fn test_process_mask") "#[ignore]\nfn test_process_mask"))
               ;; Our ld-wrapper cannot process non-UTF8 bytes in LIBRARY_PATH.
               ;; <https://lists.gnu.org/archive/html/guix-devel/2017-06/msg00193.html>
               (delete-file-recursively "src/test/run-make/linker-output-non-utf8")
               #t)))
         (add-after 'patch-tests 'fix-mtime-bug
           (lambda* (#:key #:allow-other-keys)
             (substitute* "src/build_helper/lib.rs"
               ;; Bug in Rust code.
               ;; Current implementation assume that if dst not exist then it's mtime
               ;; is 0, but in same time "src" have 0 mtime in guix build!
               (("let threshold = mtime\\(dst\\);")
                "if !dst.exists() {\nreturn false\n}\n let threshold = mtime(dst);"))))
         (add-after 'patch-source-shebangs 'patch-cargo-checksums
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/Cargo.lock"
               (("(\"checksum .* = )\".*\"" all name)
                (string-append name "\"" ,%cargo-reference-hash "\"")))
             (for-each
              (lambda (filename)
                (use-modules (guix build cargo-build-system))
                (delete-file filename)
                (let* ((dir (dirname filename)))
                  (display (string-append
                            "patch-cargo-checksums: generate-checksums for "
                            dir "\n"))
                  (generate-checksums dir ,%cargo-reference-project-file)))
              (find-files "src/vendor" ".cargo-checksum.json"))
             #t))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc"))
                    (gcc (assoc-ref inputs "gcc"))
                    (gdb (assoc-ref inputs "gdb"))
                    (binutils (assoc-ref inputs "binutils"))
                    (python (assoc-ref inputs "python-2"))
                    (rustc (assoc-ref inputs "rustc-bootstrap"))
                    (cargo (assoc-ref inputs "cargo-bootstrap"))
                    (llvm (assoc-ref inputs "llvm"))
                    (jemalloc (assoc-ref inputs "jemalloc")))
               (call-with-output-file "config.toml"
                 (lambda (port)
                   (display (string-append "
[llvm]
[build]
cargo = \"" cargo "/bin/cargo" "\"
rustc = \"" rustc "/bin/rustc" "\"
docs = true
python = \"" python "/bin/python2" "\"
gdb = \"" gdb "/bin/gdb" "\"
vendor = true
submodules = false
[install]
prefix = \"" out "\"
docdir = \"" doc "/share/doc/rust" "\"
sysconfdir = \"etc\"
localstatedir = \"var/lib\"
[rust]
default-linker = \"" gcc "/bin/gcc" "\"
channel = \"stable\"
rpath = true
# There is 2 failed codegen tests:
# codegen/mainsubprogram.rs and codegen/mainsubprogramstart.rs
# This tests required patched LLVM
codegen-tests = false
[target." %host-type "]
llvm-config = \"" llvm "/bin/llvm-config" "\"
cc = \"" gcc "/bin/gcc" "\"
cxx = \"" gcc "/bin/g++" "\"
ar = \"" binutils "/bin/ar" "\"
jemalloc = \"" jemalloc "/lib/libjemalloc_pic.a" "\"
[dist]
") port)))
               #t)))
         (add-before 'build 'reset-timestamps-after-changes
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (define ref (stat "README.md"))
             (for-each
              (lambda (filename)
                (set-file-time filename ref))
              (find-files "." #:directories? #t))
             #t))
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (and (zero? (system* "./x.py" "build"))
                  (zero? (system* "./x.py" "build" "src/tools/cargo")))))
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (zero? (system* "./x.py" "test"))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (and (zero? (system* "./x.py" "install"))
                  (begin
                   (substitute* "config.toml"
                     ;; replace prefix to specific output
                     (("prefix = \"[^\"]*\"")
                      (string-append "prefix = \"" (assoc-ref outputs "cargo") "\"")))
                   (set-file-time "config.toml" (stat "README.md"))
                   (zero? (system* "./x.py" "install" "cargo"))))))
         (add-after 'install 'wrap-rustc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (libc (assoc-ref inputs "libc"))
                   (ld-wrapper (assoc-ref inputs "ld-wrapper")))
               ;; Let gcc find ld and libc startup files.
               (wrap-program (string-append out "/bin/rustc")
                 `("PATH" ":" prefix (,(string-append ld-wrapper "/bin")))
                 `("LIBRARY_PATH" ":" suffix (,(string-append libc "/lib"))))
               #t))))))
    ;; rustc invokes gcc, so we need to set its search paths accordingly.
    (native-search-paths (package-native-search-paths gcc))
    (synopsis "Compiler for the Rust progamming language")
    (description "Rust is a systems programming language that provides memory
safety and thread safety guarantees.")
    (home-page "https://www.rust-lang.org")
    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define-public rust
  (package
    (inherit rust-1.23)
    (version "1.24.1")
    (source
     (rust-source version
                  "1vv10x2h9kq7fxh2v01damdq8pvlp5acyh1kzcda9sfjx12kv99y"))
    (native-inputs
     (alist-replace "cargo-bootstrap" (list rust-1.23 "cargo")
                    (alist-replace "rustc-bootstrap" (list rust-1.23)
                                   (package-native-inputs rust-1.23))))
    (arguments
     (substitute-keyword-arguments (package-arguments rust-1.23)
       ((#:phases phases) `(modify-phases ,phases
                             (delete 'fix-mtime-bug)))))))
