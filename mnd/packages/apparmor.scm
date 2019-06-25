;;; Copyright (C) 2019 Nikolai Merinov <nikolai.merinov@member.fsf.org>
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

(define-module (mnd packages apparmor)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix licenses)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages perl))

(define-public perl-gettext
  (package
    (name "perl-gettext")
    (version "1.07")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/P/PV/PVANDRY/gettext-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "05cwqjxxary11di03gg3fm6j9lbvg1dr2wpr311c1rwp8salg7ch"))))
    (build-system perl-build-system)
    (home-page
     "https://metacpan.org/release/gettext")
    (synopsis
     "Perl bindings for POSIX i18n gettext functions")
    (description "TODO")
    (license perl-license)))

(define %apparmor-utils-directories
  '("libraries/libapparmor" "binutils" "parser" "utils"
    #;"changehat/mod_apparmor" ; Apache module
    #;"changehat/pam_apparmor" ; PAM module
    "profiles"))

(define-public apparmor-utils
  (package
    (name "apparmor-utils")
    (version "2.13.2")
    (source (origin
	      (method url-fetch)
	      (uri (string-append "https://launchpad.net/apparmor/" (string-take version 4)
                                  "/" version "/+download/apparmor-" version ".tar.gz"))
	      (sha256
	       (base32
		"0c0sn50bpvc2f9p8ni86jwzmg07w9bj0d3a2b1w5rnnz4scyykc4"))
              (modules '((guix build utils)))
              (snippet '(begin
                          ;; Remove hard-coded path to capability.h
                          (substitute* '("parser/Makefile" "common/Make.rules" "utils/Makefile")
                            ;; TODO Maybe add "linux-libre-headers" as dependency instead?
                            (("/usr/include/linux/capability.h") ""))
                          ;; Pass LDFLAGS to python wrapper library
                          (substitute* "libraries/libapparmor/swig/python/setup.py.in"
                            (("-lapparmor") "-lapparmor @LDFLAGS@"))
                          ;; remove hard-coded /usr prefix
                          (substitute* "utils/python-tools-setup.py"
                            (("/usr") ""))
                          (substitute* "utils/Makefile"
                            (("--prefix=\\$\\{PYPREFIX\\} --root=\\$\\{DESTDIR\\}") "--prefix=${DESTDIR}"))
                          #t))))
    (inputs
     `(("python-3" ,python-3)
       ("perl" ,perl)
       ("gettext" ,gettext-minimal)))
    (native-inputs
     `(("gawk" ,gawk)
       ("bison" ,bison)
       ("flex" ,flex)
       ("dejagnu" ,dejagnu)
       ("which" ,which)
       ("perl-gettext" ,perl-gettext)
       ("python-pyflakes" ,python-pyflakes)
       ("swig" ,swig)))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((srfi srfi-1)
                  ,@%gnu-build-system-modules)
       #:configure-flags (list "--with-perl" "--with-python"
                               (string-append "LDFLAGS=-Wl,-rpath=" (assoc-ref %outputs "out") "/lib"))
       #:make-flags (list "CC=gcc" "POD2MAN=pod2man" "POD2HTML=pod2html" "PROVE=prove")
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'set-configuration-directory
           (lambda* _
             (chdir "libraries/libapparmor")
             #t))
         (add-before 'set-configuration-directory 'setenv
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "PYTHONPATH" (string-append (getcwd) "/libraries/libapparmor/swig/python"))
             (setenv "PYTHON" (string-append (assoc-ref inputs "python-3") "/bin/python3"))
             (setenv "PYTHON_VERSION" "3")
             (setenv "PYTHON_VERSIONS" "python3")
             #t))
         (add-after 'configure 'set-build-directory
           (lambda* _
             (chdir "../..")
             #t))
         (replace 'build
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (for-each
              (lambda (dir)
                (with-directory-excursion dir
                  (apply invoke "make" make-flags)))
              ',%apparmor-utils-directories)
             #t))
         (replace 'check
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (for-each
              (lambda (dir)
                (with-directory-excursion dir
                 (apply invoke "make" "check" 
                        ;; Disable check-logprof tests in "profiles" directory
                        ;; TODO FIXME
                        (cons* "LOGPROF=true"
                               make-flags))))
              (remove (lambda (dir) (or ;; no tests for this packages
                                        (string=? "changehat/pam_apparmor" dir)
                                        (string=? "changehat/mod_apparmor" dir)))
                      ',%apparmor-utils-directories))
             #t))
         (replace 'install
           (lambda* (#:key (make-flags '()) outputs #:allow-other-keys)
             (for-each
              (lambda (dir)
                (with-directory-excursion dir
                  (apply
                   invoke "make" "install" 
                   (cons*
                    (if (string=? "libraries/libapparmor" dir)
                        "DESTDIR=" ; Install path already configured in autotools package
                        (string-append "DESTDIR=" (assoc-ref outputs "out")))
                    (string-append "LOCALEDIR=/share/locale")
                    (string-append "MANDIR=/share/man")
                    (string-append "DOCDIR=" (assoc-ref outputs "out") "/share/doc/${NAME}-${VERSION}")
                    (string-append "BINDIR=" (assoc-ref outputs "out") "/bin")
                    (string-append "CONFDIR=" (assoc-ref outputs "out") "/etc/apparmor")
                    (string-append "INSTALL_CONFDIR=" (assoc-ref outputs "out") "/etc/apparmor")
                    (string-append "VIM_INSTALL_PATH=" (assoc-ref outputs "out") "/share/apparmor")
                    (string-append "EXTRAS_DEST=" (assoc-ref outputs "out") "/share/apparmor/extra-profiles/")
                    (string-append "APPARMOR_BIN_PREFIX=" (assoc-ref outputs "out") "/lib/apparmor")
                    (string-append "APPARMOR_BIN_PREFIX=" (assoc-ref outputs "out") "/lib/systemd/system")
                    make-flags))))
              ',%apparmor-utils-directories)
             #t)))))
    (synopsis "AppArmor userspace utilities")
    (description "TODO")
    (home-page "https://launchpad.net/apparmor")
    (license gpl2)))
