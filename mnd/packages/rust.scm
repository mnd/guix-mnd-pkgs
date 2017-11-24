;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Eric Le Bihan <eric.le.bihan.dev@free.fr>
;;; Copyright © 2016 ng0 <ng0@libertad.pw>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
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
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build cargo-build-system #:prefix cargo:)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26))

;; Should be one less than the current released version.
(define %rust-bootstrap-binaries-version "1.20.0")

(define %rust-bootstrap-binaries
  (origin
    (method url-fetch)
    (uri (string-append
          "https://static.rust-lang.org/dist/"
          "rust-" %rust-bootstrap-binaries-version
          "-i686-unknown-linux-gnu.tar.gz"))
    (sha256
     (base32
      "0s26n5pgwr0w0fpy4dfszahlkpah414bxpanrbyc5k8ncvh95rdb"))))

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
       #:system "i686-linux"
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
                    (ld-so (string-append libc
                                          ,(glibc-dynamic-linker "i686-linux")))
                    (rpath (string-append out "/lib:" zlib "/lib:"
                                          libc "/lib:" gcc:lib "/lib"))
                    (rustc (string-append out "/bin/rustc"))
                    (rustdoc (string-append out "/bin/rustdoc")))
               (system* "bash" "install.sh"
                        (string-append "--prefix=" out)
                        (string-append "--components=rustc,"
                                       "rust-std-i686-unknown-linux-gnu"))
               (for-each (lambda (file)
                           (system* "patchelf" "--set-rpath" rpath file))
                         (cons* rustc rustdoc (find-files out "\\.so$")))
               (for-each (lambda (file)
                           (system* "patchelf" "--set-interpreter" ld-so file))
                         (list rustc rustdoc))))))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page "https://www.rust-lang.org")
    (synopsis "Prebuilt rust compiler")
    (description "This package provides a pre-built @command{rustc} compiler,
which can in turn be used to build the final Rust compiler.")
    (license license:asl2.0)))

(define cargo-bootstrap
  (package
    (name "cargo-bootstrap")
    (version (cargo-version %rust-bootstrap-binaries-version))
    (source %rust-bootstrap-binaries)
    (build-system gnu-build-system)
    (native-inputs
     `(("patchelf" ,patchelf)))
    (inputs
     `(("gcc:lib" ,(canonical-package gcc) "lib")))
    (arguments
     `(#:tests? #f
       #:strip-binaries? #f
       #:system "i686-linux"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gcc:lib (assoc-ref inputs "gcc:lib"))
                    (libc (assoc-ref inputs "libc"))
                    (ld-so (string-append libc
                                          ,(glibc-dynamic-linker "i686-linux")))
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
    (supported-systems '("i686-linux" "x86_64-linux"))
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

(define-public rustc
  (package
    (name "rustc")
    (version (rustc-version %rust-bootstrap-binaries-version))
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://static.rust-lang.org/dist/"
                    "rustc-" version "-src.tar.gz"))
              (sha256
               (base32
                "1yj8lnxybjrybp00fqhxw8fpr641dh8wcn9mk44xjnsb4i1c21qp"))
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
       ("perl" ,perl) ; For the tests
       ("git" ,git)
       ("procps" ,procps) ; For the tests
       ("python-2" ,python-2)
       ("rust-bootstrap" ,rust-bootstrap)
       ("which" ,which)))
    (inputs
     `(("jemalloc" ,jemalloc)
       ("llvm" ,llvm-3.9.1)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
	 (add-after 'unpack 'set-env
	   (lambda _
	     (setenv "SHELL" (which "sh"))
	     (setenv "CONFIG_SHELL" (which "sh"))
	     #t))
	 (add-after 'patch-source-shebangs 'patch-cargo-checksums
	   (lambda* (#:key inputs #:allow-other-keys)
	     (for-each
	      (lambda (filename)
		(delete-file filename)
		(let* ((dir (dirname filename)))
		  (cargo:generate-checksums dir (assoc-ref inputs "source"))))
	      (find-files "src/vendor" ".cargo-checksum.json"))))
	 (replace 'configure
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (let* ((out (assoc-ref outputs "out"))
		    (gcc (assoc-ref inputs "gcc"))
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
python = \"" python "/bin/python2" "\"
vendor = true
submodules = false
[install]
prefix = \"" out "\"
[rust]
default-linker = \"" gcc "/bin/gcc" "\"
default-ar = \"" binutils "/bin/ar" "\"
channel = \"stable\"
rpath = true
[target.x86_64-unknown-linux-gnu]
llvm-config = \"" llvm "/bin/llvm-config" "\"
cc = \"" gcc "/bin/gcc" "\"
cxx = \"" gcc "/bin/g++" "\"
#jemalloc = \"" jemalloc "/lib/libjemalloc_pic.a" "\"
[dist]") port))))))
	 (replace 'build
	   (lambda* (#:key inputs outputs #:allow-other-keys)
             (zero? (system* "./x.py" "build"))))
	 (replace 'install
	   (lambda* (#:key inputs outputs #:allow-other-keys)
             (zero? (system* "./x.py" "install"))))
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

;; This tries very hard not to get into a cyclic dependency like this:
;;   cargo <- cargo-build-system <- cargo.
(define-public cargo
  (package
    (name "cargo")
    (version (cargo-version (rustc-version %rust-bootstrap-binaries-version) 0))
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rust-lang/cargo/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1d78jq7mc34n265by68amr9r4nzbiqrilfbwh7gx56ydn4gb6rpr"))))
    (build-system cargo-build-system)
    (propagated-inputs
     `(("cmake" ,cmake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("curl" ,curl)
       ("libgit2" ,libgit2)
       ("libssh2" ,libssh2)
       ("openssl" ,openssl)
       ("python-2" ,python-2)
       ("zlib" ,zlib)))
    (native-inputs
     `(("rust-advapi32-sys-0.2.0"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "advapi32-sys" "0.2.0"))
           (sha256
            (base16
             "e06588080cb19d0acb6739808aafa5f26bfb2ca015b2b6370028b44cf7cb8a9a"))))
       ("rust-aho-corasick-0.5.3"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "aho-corasick" "0.5.3"))
           (sha256
            (base16
             "ca972c2ea5f742bfce5687b9aef75506a764f61d37f8f649047846a9686ddb66"))))
       ("rust-aho-corasick-0.6.3"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "aho-corasick" "0.6.3"))
           (sha256
            (base16
             "500909c4f87a9e52355b26626d890833e9e1d53ac566db76c36faa984b889699"))))
       ("rust-atty-0.2.2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "atty" "0.2.2"))
           (sha256
            (base16
             "d912da0db7fa85514874458ca3651fe2cddace8d0b0505571dbdcd41ab490159"))))
       ("rust-backtrace-0.3.2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "backtrace" "0.3.2"))
           (sha256
            (base16
             "72f9b4182546f4b04ebc4ab7f84948953a118bd6021a1b6a6c909e3e94f6be76"))))
       ("rust-backtrace-sys-0.1.11"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "backtrace-sys" "0.1.11"))
           (sha256
            (base16
             "3a0d842ea781ce92be2bf78a9b38883948542749640b8378b3b2f03d1fd9f1ff"))))
       ("rust-bitflags-0.9.1"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "bitflags" "0.9.1"))
           (sha256
            (base16
             "4efd02e230a02e18f92fc2735f44597385ed02ad8f831e7c1c1156ee5e1ab3a5"))))
       ("rust-bufstream-0.1.3"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "bufstream" "0.1.3"))
           (sha256
            (base16
             "f2f382711e76b9de6c744cc00d0497baba02fb00a787f088c879f01d09468e32"))))
       ("rust-cfg-if-0.1.2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "cfg-if" "0.1.2"))
           (sha256
            (base16
             "d4c819a1287eb618df47cc647173c5c4c66ba19d888a6e50d605672aed3140de"))))
       ("rust-cmake-0.1.24"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "cmake" "0.1.24"))
           (sha256
            (base16
             "b8ebbb35d3dc9cd09497168f33de1acb79b265d350ab0ac34133b98f8509af1f"))))
       ("rust-crossbeam-0.2.10"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "crossbeam" "0.2.10"))
           (sha256
            (base16
             "0c5ea215664ca264da8a9d9c3be80d2eaf30923c259d03e870388eb927508f97"))))
       ("rust-curl-0.4.7"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "curl" "0.4.7"))
           (sha256
            (base16
             "6689276ab61f97c660669a5ecc117c36875dfc1ba301c986b16c653415bdf9d7"))))
       ("rust-curl-sys-0.3.14"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "curl-sys" "0.3.14"))
           (sha256
            (base16
             "d5481162dc4f424d088581db2f979fa7d4c238fe9794595de61d8d7522e277de"))))
       ("rust-dbghelp-sys-0.2.0"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "dbghelp-sys" "0.2.0"))
           (sha256
            (base16
             "97590ba53bcb8ac28279161ca943a924d1fd4a8fb3fa63302591647c4fc5b850"))))
       ("rust-docopt-0.8.1"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "docopt" "0.8.1"))
           (sha256
            (base16
             "3b5b93718f8b3e5544fcc914c43de828ca6c6ace23e0332c6080a2977b49787a"))))
       ("rust-dtoa-0.4.1"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "dtoa" "0.4.1"))
           (sha256
            (base16
             "80c8b71fd71146990a9742fc06dcbbde19161a267e0ad4e572c35162f4578c90"))))
       ("rust-env_logger-0.4.3"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "env_logger" "0.4.3"))
           (sha256
            (base16
             "3ddf21e73e016298f5cb37d6ef8e8da8e39f91f9ec8b0df44b7deb16a9f8cd5b"))))
       ("rust-error-chain-0.11.0"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "error-chain" "0.11.0"))
           (sha256
            (base16
             "ff511d5dc435d703f4971bc399647c9bc38e20cb41452e3b9feb4765419ed3f3"))))
       ("rust-filetime-0.1.10"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "filetime" "0.1.10"))
           (sha256
            (base16
             "5363ab8e4139b8568a6237db5248646e5a8a2f89bd5ccb02092182b11fd3e922"))))
       ("rust-flate2-0.2.19"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "flate2" "0.2.19"))
           (sha256
            (base16
             "36df0166e856739905cd3d7e0b210fe818592211a008862599845e012d8d304c"))))
       ("rust-foreign-types-0.2.0"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "foreign-types" "0.2.0"))
           (sha256
            (base16
             "3e4056b9bd47f8ac5ba12be771f77a0dae796d1bbaaf5fd0b9c2d38b69b8a29d"))))
       ("rust-fs2-0.4.2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "fs2" "0.4.2"))
           (sha256
            (base16
             "9ab76cfd2aaa59b7bf6688ad9ba15bbae64bff97f04ea02144cfd3443e5c2866"))))
       ("rust-gcc-0.3.51"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "gcc" "0.3.51"))
           (sha256
            (base16
             "120d07f202dcc3f72859422563522b66fe6463a4c513df062874daad05f85f0a"))))
       ("rust-git2-0.6.6"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "git2" "0.6.6"))
           (sha256
            (base16
             "aa01936ac96555c083c0e8553f672616274408d9d3fc5b8696603fbf63ff43ee"))))
       ("rust-git2-curl-0.7.0"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "git2-curl" "0.7.0"))
           (sha256
            (base16
             "68676bc784bf0bef83278898929bf64a251e87c0340723d0b93fa096c9c5bf8e"))))
       ("rust-glob-0.2.11"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "glob" "0.2.11"))
           (sha256
            (base16
             "8be18de09a56b60ed0edf84bc9df007e30040691af7acd1c41874faac5895bfb"))))
       ("rust-hamcrest-0.1.1"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "hamcrest" "0.1.1"))
           (sha256
            (base16
             "bf088f042a467089e9baa4972f57f9247e42a0cc549ba264c7a04fbb8ecb89d4"))))
       ("rust-hex-0.2.0"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "hex" "0.2.0"))
           (sha256
            (base16
             "d6a22814455d41612f41161581c2883c0c6a1c41852729b17d5ed88f01e153aa"))))
       ("rust-idna-0.1.2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "idna" "0.1.2"))
           (sha256
            (base16
             "2233d4940b1f19f0418c158509cd7396b8d70a5db5705ce410914dc8fa603b37"))))
       ("rust-itoa-0.3.1"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "itoa" "0.3.1"))
           (sha256
            (base16
             "eb2f404fbc66fd9aac13e998248505e7ecb2ad8e44ab6388684c5fb11c6c251c"))))
       ("rust-jobserver-0.1.6"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "jobserver" "0.1.6"))
           (sha256
            (base16
             "443ae8bc0af6c106e6e8b77e04684faecc1a5ce94e058f4c2b0a037b0ea1b133"))))
       ("rust-kernel32-sys-0.2.2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "kernel32-sys" "0.2.2"))
           (sha256
            (base16
             "7507624b29483431c0ba2d82aece8ca6cdba9382bff4ddd0f7490560c056098d"))))
       ("rust-lazy_static-0.2.8"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "lazy_static" "0.2.8"))
           (sha256
            (base16
             "3b37545ab726dd833ec6420aaba8231c5b320814b9029ad585555d2a03e94fbf"))))
       ("rust-libc-0.2.25"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "libc" "0.2.25"))
           (sha256
            (base16
             "b807d3f9f61fec68ffa8b10389fffb9235aa0ffa32935be864b2329de5846b74"))))
       ("rust-libgit2-sys-0.6.12"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "libgit2-sys" "0.6.12"))
           (sha256
            (base16
             "df18a822100352d9863b302faf6f8f25c0e77f0e60feb40e5dbe1238b7f13b1d"))))
       ("rust-libssh2-sys-0.2.6"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "libssh2-sys" "0.2.6"))
           (sha256
            (base16
             "0db4ec23611747ef772db1c4d650f8bd762f07b461727ec998f953c614024b75"))))
       ("rust-libz-sys-1.0.16"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "libz-sys" "1.0.16"))
           (sha256
            (base16
             "3fdd64ef8ee652185674455c1d450b83cbc8ad895625d543b5324d923f82e4d8"))))
       ("rust-log-0.3.8"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "log" "0.3.8"))
           (sha256
            (base16
             "880f77541efa6e5cc74e76910c9884d9859683118839d6a1dc3b11e63512565b"))))
       ("rust-matches-0.1.6"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "matches" "0.1.6"))
           (sha256
            (base16
             "100aabe6b8ff4e4a7e32c1c13523379802df0772b82466207ac25b013f193376"))))
       ("rust-memchr-0.1.11"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "memchr" "0.1.11"))
           (sha256
            (base16
             "d8b629fb514376c675b98c1421e80b151d3817ac42d7c667717d282761418d20"))))
       ("rust-memchr-1.0.1"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "memchr" "1.0.1"))
           (sha256
            (base16
             "1dbccc0e46f1ea47b9f17e6d67c5a96bd27030519c519c9c91327e31275a47b4"))))
       ("rust-miniz-sys-0.1.9"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "miniz-sys" "0.1.9"))
           (sha256
            (base16
             "28eaee17666671fa872e567547e8428e83308ebe5808cdf6a0e28397dbe2c726"))))
       ("rust-miow-0.2.1"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "miow" "0.2.1"))
           (sha256
            (base16
             "8c1f2f3b1cf331de6896aabf6e9d55dca90356cc9960cca7eaaf408a355ae919"))))
       ("rust-net2-0.2.29"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "net2" "0.2.29"))
           (sha256
            (base16
             "bc01404e7568680f1259aa5729539f221cb1e6d047a0d9053cab4be8a73b5d67"))))
       ("rust-num-0.1.39"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num" "0.1.39"))
           (sha256
            (base16
             "2c3a3dc9f30bf824141521b30c908a859ab190b76e20435fcd89f35eb6583887"))))
       ("rust-num-bigint-0.1.39"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-bigint" "0.1.39"))
           (sha256
            (base16
             "6361748d02e5291c72a422dc8ed4d8464a80cb1e618971f6fffe6d52d97e3286"))))
       ("rust-num-complex-0.1.38"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-complex" "0.1.38"))
           (sha256
            (base16
             "412dfc143c56579aa6a22c574e38ddbf724522f1280ae2b257498cccff3fb6af"))))
       ("rust-num-integer-0.1.34"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-integer" "0.1.34"))
           (sha256
            (base16
             "ef1a4bf6f9174aa5783a9b4cc892cacd11aebad6c69ad027a0b65c6ca5f8aa37"))))
       ("rust-num-iter-0.1.33"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-iter" "0.1.33"))
           (sha256
            (base16
             "f7d1891bd7b936f12349b7d1403761c8a0b85a18b148e9da4429d5d102c1a41e"))))
       ("rust-num-rational-0.1.38"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-rational" "0.1.38"))
           (sha256
            (base16
             "33c881e104a26e1accc09449374c095ff2312c8e0c27fab7bbefe16eac7c776d"))))
       ("rust-num-traits-0.1.39"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-traits" "0.1.39"))
           (sha256
            (base16
             "1708c0628602a98b52fad936cf3edb9a107af06e52e49fdf0707e884456a6af6"))))
       ("rust-num_cpus-1.6.2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num_cpus" "1.6.2"))
           (sha256
            (base16
             "aec53c34f2d0247c5ca5d32cca1478762f301740468ee9ee6dcb7a0dd7a0c584"))))
       ("rust-openssl-0.9.14"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "openssl" "0.9.14"))
           (sha256
            (base16
             "11ba043cb65fc9af71a431b8a36ffe8686cd4751cdf70a473ec1d01066ac7e41"))))
       ("rust-openssl-probe-0.1.1"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "openssl-probe" "0.1.1"))
           (sha256
            (base16
             "d98df0270d404ccd3c050a41d579c52d1db15375168bb3471e04ec0f5f378daf"))))
       ("rust-openssl-sys-0.9.14"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "openssl-sys" "0.9.14"))
           (sha256
            (base16
             "236c718c2e2c2b58a546d86ffea5194400bb15dbe01ca85325ffd357b03cf66c"))))
       ("rust-percent-encoding-1.0.0"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "percent-encoding" "1.0.0"))
           (sha256
            (base16
             "de154f638187706bde41d9b4738748933d64e6b37bdbffc0b47a97d16a6ae356"))))
       ("rust-pkg-config-0.3.9"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "pkg-config" "0.3.9"))
           (sha256
            (base16
             "3a8b4c6b8165cd1a1cd4b9b120978131389f64bdaf456435caa41e630edba903"))))
       ("rust-psapi-sys-0.1.0"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "psapi-sys" "0.1.0"))
           (sha256
            (base16
             "abcd5d1a07d360e29727f757a9decb3ce8bc6e0efa8969cfaad669a8317a2478"))))
       ("rust-quote-0.3.15"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "quote" "0.3.15"))
           (sha256
            (base16
             "7a6e920b65c65f10b2ae65c831a81a073a89edd28c7cce89475bff467ab4167a"))))
       ("rust-rand-0.3.15"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "rand" "0.3.15"))
           (sha256
            (base16
             "022e0636ec2519ddae48154b028864bdce4eaf7d35226ab8e65c611be97b189d"))))
       ("rust-regex-0.1.80"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "regex" "0.1.80"))
           (sha256
            (base16
             "4fd4ace6a8cf7860714a2c2280d6c1f7e6a413486c13298bbc86fd3da019402f"))))
       ("rust-regex-0.2.2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "regex" "0.2.2"))
           (sha256
            (base16
             "1731164734096285ec2a5ec7fea5248ae2f5485b3feeb0115af4fda2183b2d1b"))))
       ("rust-regex-syntax-0.3.9"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "regex-syntax" "0.3.9"))
           (sha256
            (base16
             "f9ec002c35e86791825ed294b50008eea9ddfc8def4420124fbc6b08db834957"))))
       ("rust-regex-syntax-0.4.1"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "regex-syntax" "0.4.1"))
           (sha256
            (base16
             "ad890a5eef7953f55427c50575c680c42841653abd2b028b68cd223d157f62db"))))
       ("rust-rustc-demangle-0.1.4"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "rustc-demangle" "0.1.4"))
           (sha256
            (base16
             "3058a43ada2c2d0b92b3ae38007a2d0fa5e9db971be260e0171408a4ff471c95"))))
       ("rust-rustc-serialize-0.3.24"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "rustc-serialize" "0.3.24"))
           (sha256
            (base16
             "dcf128d1287d2ea9d80910b5f1120d0b8eede3fbf1abe91c40d39ea7d51e6fda"))))
       ("rust-scoped-tls-0.1.0"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "scoped-tls" "0.1.0"))
           (sha256
            (base16
             "f417c22df063e9450888a7561788e9bd46d3bb3c1466435b4eccb903807f147d"))))
       ("rust-semver-0.7.0"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "semver" "0.7.0"))
           (sha256
            (base16
             "3fdd61b85a0fa777f7fb7c454b9189b2941b110d1385ce84d7f76efdf1606a85"))))
       ("rust-semver-parser-0.7.0"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "semver-parser" "0.7.0"))
           (sha256
            (base16
             "388a1df253eca08550bef6c72392cfe7c30914bf41df5269b68cbd6ff8f570a3"))))
       ("rust-serde-1.0.9"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "serde" "1.0.9"))
           (sha256
            (base16
             "6a7c6b751a2e8d5df57a5ff71b5b4fc8aaee9ee28ff1341d640dd130bb5f4f7a"))))
       ("rust-serde_derive-1.0.9"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "serde_derive" "1.0.9"))
           (sha256
            (base16
             "2f6ca58905ebd3c3b285a8a6d4f3ac92b92c0d7951d5649b1bdd212549c06639"))))
       ("rust-serde_derive_internals-0.15.1"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "serde_derive_internals" "0.15.1"))
           (sha256
            (base16
             "37aee4e0da52d801acfbc0cc219eb1eda7142112339726e427926a6f6ee65d3a"))))
       ("rust-serde_ignored-0.0.3"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "serde_ignored" "0.0.3"))
           (sha256
            (base16
             "c10e798e4405d7dcec3658989e35ee6706f730a9ed7c1184d5ebd84317e82f46"))))
       ("rust-serde_json-1.0.2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "serde_json" "1.0.2"))
           (sha256
            (base16
             "48b04779552e92037212c3615370f6bd57a40ebba7f20e554ff9f55e41a69a7b"))))
       ("rust-shell-escape-0.1.3"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "shell-escape" "0.1.3"))
           (sha256
            (base16
             "dd5cc96481d54583947bfe88bf30c23d53f883c6cd0145368b69989d97b84ef8"))))
       ("rust-socket2-0.2.1"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "socket2" "0.2.1"))
           (sha256
            (base16
             "12cdbddbaa27bf94cc194b8e37f5811db6fe83cea96cf99cf1f8e92b65a41371"))))
       ("rust-strsim-0.6.0"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "strsim" "0.6.0"))
           (sha256
            (base16
             "b4d15c810519a91cf877e7e36e63fe068815c678181439f2f29e2562147c3694"))))
       ("rust-syn-0.11.11"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "syn" "0.11.11"))
           (sha256
            (base16
             "d3b891b9015c88c576343b9b3e41c2c11a51c219ef067b264bd9c8aa9b441dad"))))
       ("rust-synom-0.11.3"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "synom" "0.11.3"))
           (sha256
            (base16
             "a393066ed9010ebaed60b9eafa373d4b1baac186dd7e008555b0f702b51945b6"))))
       ("rust-tar-0.4.13"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "tar" "0.4.13"))
           (sha256
            (base16
             "281285b717926caa919ad905ef89c63d75805c7d89437fb873100925a53f2b1b"))))
       ("rust-tempdir-0.3.5"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "tempdir" "0.3.5"))
           (sha256
            (base16
             "87974a6f5c1dfb344d733055601650059a3363de2a6104819293baff662132d6"))))
       ("rust-termcolor-0.3.2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "termcolor" "0.3.2"))
           (sha256
            (base16
             "9a5193a56b8d82014662c4b933dea6bec851daf018a2b01722e007daaf5f9dca"))))
       ("rust-thread-id-2.0.0"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "thread-id" "2.0.0"))
           (sha256
            (base16
             "a9539db560102d1cef46b8b78ce737ff0bb64e7e18d35b2a5688f7d097d0ff03"))))
       ("rust-thread_local-0.2.7"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "thread_local" "0.2.7"))
           (sha256
            (base16
             "8576dbbfcaef9641452d5cf0df9b0e7eeab7694956dd33bb61515fb8f18cfdd5"))))
       ("rust-thread_local-0.3.4"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "thread_local" "0.3.4"))
           (sha256
            (base16
             "1697c4b57aeeb7a536b647165a2825faddffb1d3bad386d507709bd51a90bb14"))))
       ("rust-toml-0.4.2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "toml" "0.4.2"))
           (sha256
            (base16
             "b0601da6c97135c8d330c7a13a013ca6cd4143221b01de2f8d4edc50a9e551c7"))))
       ("rust-unicode-bidi-0.3.4"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "unicode-bidi" "0.3.4"))
           (sha256
            (base16
             "49f2bd0c6468a8230e1db229cff8029217cf623c767ea5d60bfbd42729ea54d5"))))
       ("rust-unicode-normalization-0.1.5"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "unicode-normalization" "0.1.5"))
           (sha256
            (base16
             "51ccda9ef9efa3f7ef5d91e8f9b83bbe6955f9bf86aec89d5cce2c874625920f"))))
       ("rust-unicode-xid-0.0.4"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "unicode-xid" "0.0.4"))
           (sha256
            (base16
             "8c1f860d7d29cf02cb2f3f359fd35991af3d30bac52c57d265a3c461074cb4dc"))))
       ("rust-unreachable-1.0.0"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "unreachable" "1.0.0"))
           (sha256
            (base16
             "382810877fe448991dfc7f0dd6e3ae5d58088fd0ea5e35189655f84e6814fa56"))))
       ("rust-url-1.5.1"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "url" "1.5.1"))
           (sha256
            (base16
             "eeb819346883532a271eb626deb43c4a1bb4c4dd47c519bd78137c3e72a4fe27"))))
       ("rust-utf8-ranges-0.1.3"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "utf8-ranges" "0.1.3"))
           (sha256
            (base16
             "a1ca13c08c41c9c3e04224ed9ff80461d97e121589ff27c753a16cb10830ae0f"))))
       ("rust-utf8-ranges-1.0.0"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "utf8-ranges" "1.0.0"))
           (sha256
            (base16
             "662fab6525a98beff2921d7f61a39e7d59e0b425ebc7d0d9e66d316e55124122"))))
       ("rust-vcpkg-0.2.2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "vcpkg" "0.2.2"))
           (sha256
            (base16
             "9e0a7d8bed3178a8fb112199d466eeca9ed09a14ba8ad67718179b4fd5487d0b"))))
       ("rust-void-1.0.2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "void" "1.0.2"))
           (sha256
            (base16
             "6a02e4885ed3bc0f2de90ea6dd45ebcbb66dacffe03547fadbb0eeae2770887d"))))
       ("rust-winapi-0.2.8"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "winapi" "0.2.8"))
           (sha256
            (base16
             "167dc9d6949a9b857f3451275e911c3f44255842c1f7a76f33c55103a909087a"))))
       ("rust-winapi-build-0.1.1"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "winapi-build" "0.1.1"))
           (sha256
            (base16
             "2d315eee3b34aca4797b2da6b13ed88266e6d612562a0c46390af8299fc699bc"))))
       ("rust-wincolor-0.1.4"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "wincolor" "0.1.4"))
           (sha256
            (base16
             "a39ee4464208f6430992ff20154216ab2357772ac871d994c51628d60e58b8b0"))))
       ("rust-ws2_32-sys-0.2.1"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "ws2_32-sys" "0.2.1"))
           (sha256
            (base16
             "d59cefebd0c892fa2dd6de581e937301d8552cb44489cdff035c6187cb63fa5e"))))))
    (arguments
     `(#:cargo ,cargo-bootstrap
       #:tests? #f ; FIXME
       #:modules
       ((ice-9 match)
        (srfi srfi-1) ; 'every
        (guix build utils)
        (guix build cargo-build-system))
       #:phases
       (modify-phases %standard-phases
         ;; Avoid cargo complaining about missmatched checksums.
         (delete 'patch-source-shebangs)
         (delete 'patch-generated-file-shebangs)
         (delete 'patch-usr-bin-file)
         (add-after 'unpack 'unpack-submodule-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (define (unpack source target)
               (mkdir-p target)
               (with-directory-excursion target
                 (zero? (system* "tar" "xf"
                                 source
                                 "--strip-components=1"))))
             (define (touch file-name)
               (call-with-output-file file-name (const #t)))
             (define (install-rust-library entry)
               (match entry
                 ((name . src)
                  (if (string-prefix? "rust-" name)
                    (let* ((rust-length (string-length "rust-"))
                           (rust-name (string-drop name
                                                   rust-length))
                           (rsrc (string-append "vendor/"
                                                rust-name))
                           (unpack-status (unpack src rsrc)))
                      (touch (string-append rsrc "/.cargo-ok"))
                      (generate-checksums rsrc src)
                      unpack-status)))
                 (_ #t)))
               (mkdir "vendor")
               (every install-rust-library inputs)))
         (add-after 'unpack 'set-environment-up
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((gcc (assoc-ref inputs "gcc"))
                    (cc (string-append gcc "/bin/gcc")))
               (mkdir ".cargo")
               (call-with-output-file ".cargo/config"
                 (lambda (p)
                   (format p "
[source.crates-io]
registry = 'https://github.com/rust-lang/crates.io-index'
replace-with = 'vendored-sources'

[source.vendored-sources]
directory = 'vendor'
")))
               (setenv "CMAKE_C_COMPILER" cc)
               (setenv "CC" cc))
             #t))
         (delete 'configure))))
    (home-page "https://github.com/rust-lang/cargo")
    (synopsis "Build tool and package manager for Rust")
    (description "Cargo is a tool that allows Rust projects to declare their
dependencies and ensures a reproducible build.")
    ;; Cargo is dual licensed Apache and MIT. Also contains
    ;; code from openssl which is GPL2 with linking exception.
    (license (list license:asl2.0 license:expat license:gpl2))))
