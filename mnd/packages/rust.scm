;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Eric Le Bihan <eric.le.bihan.dev@free.fr>
;;; Copyright © 2016 ng0 <ng0@infotropique.org>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Nikolai Merinov <nikolai.merinov@member.fsf.org>
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
  #:use-module (guix base16)      ;for generated "cargo" native-inputs
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26))

;; Should be one less than the current released version.
(define %rust-bootstrap-binaries-version "1.23.0")

(define %rust-bootstrap-binaries
  (origin
    (method url-fetch)
    (uri (string-append
          "https://static.rust-lang.org/dist/"
          "rust-" %rust-bootstrap-binaries-version
          "-" %host-type ".tar.gz"))
    (sha256
     (base32
      (match %host-type
        ("i686-unknown-linux-gnu"
         "0gs283lw6larhjlr02zm9g78djq2f6bdhxj6ls66q0z18zpx0nyw")
        ("x86_64-unknown-linux-gnu"
         "0znw3xxh837i5wlwsbbw6bxdqfa58bxyw3716wbckwyph8xb4d4s")
        ("armv7-unknown-linux-gnueabihf"
         "13mh4qx996rb6c3xygflc10j5zkmcxzjr32340ardwb7ja4jfw2q")
        ("aarch64-unknown-linux-gnu"
         "1irbj73ifdm7xvshma7qp61sadm683dnc57jfg5qc8kdjyyrydrq")
        ("mips64el-unknown-linux-gnuabi64"
         "1wksf07ba9idrj1z6x0hdfjsmhpzzi5idawnkfbhy6cj1g9ihnzv")
        (_ "")))))) ; Catch-all for other systems.

(define %cargo-reference-project-file "/dev/null")
(define %cargo-reference-hash
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

(define (increment-rust-version rust-version major patch)
  (match (string-split rust-version #\.)
    (("1" minor _)
     (string-append (number->string major) "."
                    (number->string (+ (string->number minor) 1)) "."
                    (number->string patch)))))

(define* (cargo-version rustc-version #:optional (patch 0))
  ;; Computes the cargo version that matches the rustc version.
  ;; https://github.com/rust-lang/cargo#Releases
  (increment-rust-version rustc-version 0 patch))

(define* (rustc-version bootstrap-version #:optional (patch 0))
  ;; Computes the rustc version that can be compiled from a given
  ;; other rustc version. The patch argument is for selecting
  ;; a stability or security fix. 1.11.0 -> 1.12.1 -> 1.13.0
  (increment-rust-version bootstrap-version 1 patch))

(define rustc-bootstrap
  (package
    (name "rustc-bootstrap")
    (version %rust-bootstrap-binaries-version)
    (source %rust-bootstrap-binaries)
    (build-system gnu-build-system)
    (native-inputs
     `(("patchelf" ,patchelf)))
    (inputs
     `(("gcc:lib" ,(canonical-package gcc) "lib")
       ("zlib" ,zlib)))
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
                    (gcc:lib (assoc-ref inputs "gcc:lib"))
                    (libc (assoc-ref inputs "libc"))
                    (zlib (assoc-ref inputs "zlib"))
                    (ld-so (string-append libc ,(glibc-dynamic-linker)))
                    (rpath (string-append out "/lib:" zlib "/lib:"
                                          libc "/lib:" gcc:lib "/lib"))
                    (rustc (string-append out "/bin/rustc"))
                    (rustdoc (string-append out "/bin/rustdoc")))
               (system* "bash" "install.sh"
                        (string-append "--prefix=" out)
                        (string-append "--components=rustc,"
                                       "rust-std-" %host-type))
               (for-each (lambda (file)
                           (system* "patchelf" "--set-rpath" rpath file))
                         (cons* rustc rustdoc (find-files out "\\.so$")))
               (for-each (lambda (file)
                           (system* "patchelf" "--set-interpreter" ld-so file))
                         (list rustc rustdoc))))))))
    (home-page "https://www.rust-lang.org")
    (synopsis "Prebuilt rust compiler")
    (description "This package provides a pre-built @command{rustc} compiler,
which can in turn be used to build the final Rust compiler.")
    (license license:asl2.0)))

(define cargo-bootstrap
  (package
    (name "cargo-bootstrap")
    (version (cargo-version %rust-bootstrap-binaries-version 0))
    (source %rust-bootstrap-binaries)
    (build-system gnu-build-system)
    (native-inputs
     `(("patchelf" ,patchelf)))
    (inputs
     `(("gcc:lib" ,(canonical-package gcc) "lib")))
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
                    (gcc:lib (assoc-ref inputs "gcc:lib"))
                    (libc (assoc-ref inputs "libc"))
                    (ld-so (string-append libc ,(glibc-dynamic-linker)))
                    (rpath (string-append out "/lib:" libc "/lib:"
                                          gcc:lib "/lib"))
                    (cargo (string-append out "/bin/cargo")))
               (system* "bash" "install.sh"
                        (string-append "--prefix=" out)
                        "--components=cargo")
               (system* "patchelf"
                        "--set-interpreter" ld-so
                        "--set-rpath" rpath
                        cargo)))))))
    (home-page "https://www.rust-lang.org")
    (synopsis "Prebuilt cargo package manager")
    (description "This package provides a pre-built @command{cargo} package
manager, which is required to build itself.")
    (license license:asl2.0)))

(define rust-bootstrap
  (package
    (name "rust-bootstrap")
    (version %rust-bootstrap-binaries-version)
    (source #f)
    (build-system trivial-build-system)
    (propagated-inputs
     `(("rustc-bootstrap" ,rustc-bootstrap)
       ("cargo-bootstrap" ,cargo-bootstrap)
       ("gcc" ,(canonical-package gcc))))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((out (assoc-ref %outputs "out"))
               (gcc (assoc-ref %build-inputs "gcc")))
           (mkdir-p (string-append out "/bin"))
           ;; Rust requires a C toolchain for linking. The prebuilt
           ;; binaries expect a compiler called cc. Thus symlink gcc
           ;; to cc.
           (symlink (string-append gcc "/bin/gcc")
                    (string-append out "/bin/cc"))))))
    (home-page "https://www.rust-lang.org")
    (synopsis "Rust bootstrapping meta package")
    (description "Meta package for a rust environment. Provides pre-compiled
rustc-bootstrap and cargo-bootstrap packages.")
    (license license:asl2.0)))

(define-public rust
  (package
    (name "rust")
    (version (rustc-version %rust-bootstrap-binaries-version 1))
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://static.rust-lang.org/dist/"
                    "rustc-" version "-src.tar.gz"))
              (sha256
               (base32
                "1vv10x2h9kq7fxh2v01damdq8pvlp5acyh1kzcda9sfjx12kv99y"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  (delete-file-recursively "src/llvm")
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison) ; For the tests
       ("cmake" ,cmake)
       ("flex" ,flex) ; For the tests
       ("gdb" ,gdb)   ; For the tests
       ("git" ,git)
       ("procps" ,procps) ; For the tests
       ("python-2" ,python-2)
       ("rust-bootstrap" ,rust-bootstrap)
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
