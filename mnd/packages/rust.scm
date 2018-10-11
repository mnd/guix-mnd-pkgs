(define-module (mnd packages rust)
  #:use-module (gnu packages rust)
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
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26))


(define* (rust-source version hash #:key (patches '()))
  (origin
    (method url-fetch)
    (uri (string-append "https://static.rust-lang.org/dist/"
                        "rustc-" version "-src.tar.gz"))
    (sha256 (base32 hash))
    (modules '((guix build utils)))
    (snippet '(begin (delete-file-recursively "src/llvm") #t))
    (patches (map search-patch patches))))

(define* (rust-bootstrapped-package base-rust version checksum
                                    #:key (patches '()))
  "Bootstrap rust VERSION with source checksum CHECKSUM patched with PATCHES using BASE-RUST."
  (package
    (inherit base-rust)
    (version version)
    (source
     (rust-source version checksum #:patches patches))
    (native-inputs
     (alist-replace "cargo-bootstrap" (list base-rust "cargo")
                    (alist-replace "rustc-bootstrap" (list base-rust)
                                   (package-native-inputs base-rust))))))

(define-public rust-reproducible-1.25
  (let ((base-rust rust-1.25))
    (package
      (inherit base-rust)
      (name "rust-reproducible")
      (inputs
       ;; switch back to 3.9.1
       (alist-replace "llvm" (list llvm-3.9.1)
                      (package-inputs base-rust)))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             ;; Not enabled yet still package moved back to llvm 3.9.1
             (delete 'enable-codegen-tests)
             ;; to increase local test speed
             (delete 'check))))))))

(define-public rust-reproducible-1.27   ;TODO FIXME not reproducible
  (let ((base-rust (rust-bootstrapped-package rust "1.27.2"
                                    "0pg1s37bhx9zqbynxyydq5j6q7kij9vxkcv8maz0m25prm88r0cs"
                                    #:patches
                                    '("rust-coresimd-doctest.patch"
                                      "rust-bootstrap-stage0-test.patch"
                                      "rust-1.25-accept-more-detailed-gdb-lines.patch"
                                      "rust-mdbook-Support-reproducible-builds-by-forcing-window.search.patch"))))
    (package
      (inherit base-rust)
      (name "rust-reproducible")
      (inputs
       `(("libssh2" ,libssh2)           ;for cargo
         ("libgit2" ,libgit2)           ;for cargo
         ;; switch back to 3.9.1
         ,@(alist-replace "llvm" (list llvm-3.9.1)
                          (package-inputs base-rust))))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'set-env 'cargo-use-host-libgit2
               (lambda* _
                 ;; Use system's libgit2.so library
                 (setenv "LIBGIT2_SYS_USE_PKG_CONFIG" "1")
                 #t))
             ;; Not enabled yet still package moved back to llvm 3.9.1
             (delete 'enable-codegen-tests)
             ;; to increase local test speed
             (delete 'check))))))))

(define-public rust-1.28
  (let ((base-rust
         (rust-bootstrapped-package rust "1.28.0"
                                    "11k4rn77bca2rikykkk9fmprrgjswd4x4kaq7fia08vgkir82nhx"
                                    #:patches
                                    '("rust-coresimd-doctest.patch"
                                      "rust-bootstrap-stage0-test.patch"
                                      "rust-1.25-accept-more-detailed-gdb-lines.patch"
                                      "rust-mdbook-Support-reproducible-builds-by-forcing-window.search.patch"))))
    (package
      (inherit base-rust)
      (inputs
       `(("libssh2" ,libssh2)           ;for cargo
         ("libgit2" ,libgit2)           ;for cargo
         ,@(package-inputs base-rust)))
            (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'set-env 'cargo-use-host-libgit2
               (lambda* _
                 ;; Use system's libgit2.so library
                 (setenv "LIBGIT2_SYS_USE_PKG_CONFIG" "1")
                 #t))
             (add-after 'patch-tests 'disable-amd64-avx-test
               ;; This test will fail on x86_64 machines without avx
               (lambda* _
                 (substitute* "src/test/run-pass/issue-44056.rs"
                   (("only-x86_64") "ignore-test"))))
             (add-after 'patch-cargo-tests 'disable-cargo-network-test
               ;; Disable test that try to create server on loopback address
               (lambda* _
                 (substitute* "src/tools/cargo/tests/testsuite/build_auth.rs"
                   (("fn http_auth_offered") "#[ignore]\nfn http_auth_offered")))))))))))
