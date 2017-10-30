;;; Copyright (C) 2017 Nikolay Merinov <nikolay.merinov@member.fsf.org>
;;;
;;; This file is part of mnd repository for GNU Guix.
;;;
;;; This script is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This script is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with mnd repository.  If not, see <http://www.gnu.org/licenses/>.

(define-module (mnd packages yocto)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages less)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages file)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (mnd packages chrpath)
  #:use-module (mnd packages coreutils))

;; USAGE:
;;    # pwd by full path required by kernel, bash required by configure scripts, ln required by perl
;;    guix environment -L ~/Workspace/guix-mnd-pkgs --ad-hoc poky --container --network --root=${PWD}/poky.env --expose=${PWD}/poky.env=/usr  --expose=/bin/sh=/bin/bash --expose=${PWD}/poky.env/bin/pwd=/bin/pwd --expose=${PWD}/poky.env/bin/ln=/bin/ln
;;    export LC_ALL=en_US.UTF-8  # Use locale suitable for python3
;;
;;    # If you want to use patched version of poky call
;;    source /usr/share/yocto/oe-init-build-env
;;    # NOTE: For qemu build it's possible that you must add next line to conf/local.conf
;;    # http://yocto.yoctoproject.narkive.com/MD5B7Gqd/linux-yocto-rt-4-4-11
;;    ARCH_pn-linux-yocto="i386"
;;
;;    # If you use your own local copy of poky then apply patches from this pacakge and call
;;    source ./oe-init-build-env # Create build environment
;;    export BB_ENV_EXTRAWHITE="$BB_ENV_EXTRAWHITE GUIX_LOCPATH LIBRARY_PATH"
;; Configuration:
;;    # Then add next lines to conf/local.conf file:
;;    export LIBRARY_PATH
;;    export GUIX_LOCPATH
;;
;;    BUILD_CPPFLAGS_append = " -isystem/usr/include"
;;    # gmp-native check C preprocessor and CXX preprocessor without CPPFLAGS
;;    BUILD_CPP_append = " -isystem/usr/include"
;;    BUILD_CXX_append = " -isystem/usr/include"
;;    # gmp-native build part of sources with CC_FOR_BUILD without flags
;;    BUILD_CC_append = " -isystem/usr/include"
;;

(define-public poky
  (package
   (name "poky")
   (version "2.4-dev")
   (source (origin
	     (method git-fetch)
	     (uri (git-reference
		   (url "git://git.yoctoproject.org/poky")
		   (commit "65d23bd7986615fdfb0f1717b615534a2a14ab80")))
	     (file-name (string-append name "-" version "-checkout"))
	     (sha256
	      (base32
	       "1bi0llsz91cqmyyffyp0cfiwc5yvc4n4dqdy23k2c2rqvssf15kf"))
	     (patches
	      (list (search-patch "poky-perl-native_5.24.1.bb-Provide-custom-lddlflags.patch")
		    ;; FIXME: Last "uninative" crash on my machine
		    (search-patch "poky-downgrade-uninative.patch")))))
   (build-system trivial-build-system)
   (arguments `(#:modules ((guix build utils))
		#:builder
		(begin
		  (use-modules (guix build utils)
			       (srfi srfi-26))
		  
		  (let* ((out (assoc-ref %outputs "out"))
			 (source (assoc-ref %build-inputs "source"))
			 (share (string-append out "/share"))
			 (yocto (string-append share "/yocto"))
			 (bin (string-append out "/bin"))
			 (tar-dir (string-append (assoc-ref %build-inputs "tar") "/bin"))
			 (xz-dir (string-append (assoc-ref %build-inputs "xz") "/bin"))
			 (python-3-dir (string-append (assoc-ref %build-inputs "python-3") "/bin"))
			 (python-2-dir (string-append (assoc-ref %build-inputs "python-2") "/bin"))
			 (sh-dir (string-append (assoc-ref %build-inputs "bash") "/bin"))
			 (coreutils-dir (string-append (assoc-ref %build-inputs "coreutils") "/bin"))
			 (shebang-dirs (list python-2-dir python-3-dir sh-dir coreutils-dir))
			 (env-util (string-append coreutils-dir "/env")))
		    (mkdir-p share)
		    ; FIXME: Without patches we have fetched sources, but with patches source is tarball
		    #;(copy-recursively (string-append source "/.") yocto)
		    (chdir share)
		    (setenv "PATH" (string-append tar-dir ":" xz-dir))
		    (system* "tar" "xvf" source)
		    (rename-file ,(string-append name "-" version "-checkout") yocto)
		    (with-directory-excursion yocto
		      (substitute* "bitbake/lib/bb/utils.py"
			(("'BB_TASKHASH',")
			 ; GUIX_LOCPATH required for python locale
			 ; LIBRARY_PATH used only for host compiler, required to found crti.o
			 "'BB_TASKHASH', 'GUIX_LOCPATH', 'LIBRARY_PATH',"))
		      (substitute* "meta/classes/sanity.bbclass"
			(("if 0 == os.getuid\\(\\):")
			 ; In container my user has ID 0, so remove test on root
			 "if False:"))
		      (substitute* "meta/conf/bitbake.conf"
			(("BUILD_(CC|CPP|CXX|CPPFLAGS) = \"[^ \"]+" all)
			 ; For most cases it's enough to add key to CPPFLAGS, but many packages
			 ; ignored this variable, so add in every place where we can.
			 (string-append all " -isystem/usr/include ")))
		      (substitute* "bitbake/lib/bb/fetch2/__init__.py"
			(("cp -fpPRH")
			 ; FIXME: In container "nobody" must own files in /gnu/store,
			 ; but in fact there is gid/uid = 65534 and there is no such user in /etc/passwd
			 "cp -fpPRH --no-preserve=ownership")))))))
   (propagated-inputs `(("bash" ,bash)
			("coreutils" ,coreutils-with-xattr)
			("python-3" ,python-3)
			("python-2" ,python-2.7)
			("glibc" ,glibc) ;ldd geconf rpcgen
			("glibc-locales" ,glibc-locales) ;locales for python
			("texinfo" ,texinfo) ;makeinfo
			("perl" ,perl)
			("chrpath" ,chrpath)
			("cpio" ,cpio)
			("file" ,file)
			("gcc" ,gcc)
			("binutils" ,binutils)
			("elfutils" ,elfutils) ;readelf
			("diffstat" ,diffstat)
			("git" ,git)
			("gawk" ,gawk)
			("grep" ,grep)
			("sed" ,sed)
			("make" ,gnu-make)
			("tar" ,tar)
			("xz" ,xz)
			("bzip2" ,bzip2)
			("gzip" ,gzip)
			("diffutils" ,diffutils)
			("findutils" ,findutils)
			("util-linux" ,util-linux)
			("inetutils" ,inetutils)
			("which" ,which)
			("iproute2" ,iproute)
			("procps" ,procps)
			("openssh" ,openssh)
			("wget" ,wget)
			("patch" ,patch)
			("socat" ,socat)
			("screen" ,screen))) ; FIXME: for "bitbake -c devshell"
   (synopsis "Yocto reference distribution")
   (description "Poky is a reference distribution of the Yocto Project. It contains the OpenEmbedded Build System (BitBake and OpenEmbedded Core) as well as a set of metadata to get you started building your own distro.")
   (home-page "https://www.yoctoproject.org/tools-resources/projects/poky")
   (license gpl2)))			;And MIT for part of packages
