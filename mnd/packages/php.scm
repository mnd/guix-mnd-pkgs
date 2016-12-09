;;; Copyright (C) 2015-2016 Nikolay Merinov <nikolay.merinov@member.fsf.org>
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

(define-module (mnd packages php)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages web))

(define-public php
  (package
    (name "php")
    (version "5.6.17")
    (source (origin
	      (method url-fetch)
	      (uri (string-append "http://www.php.net/distributions/php-" version
				  ".tar.xz"))
	      (sha256
	       (base32
		"0b7vn2kv0lg56kyfl42h9c9cpmxcvrk68q9iw5qp2z0c714mg7ga"))
	      (patches (list (search-patch "php5-httpd2-support.patch")
			     (search-patch "php5-deterministic-build.patch")))))
    (build-system gnu-build-system)
    ;; REPORT_EXIT_STATUS=1 will fail if tests failed. For now 30/9000 tests fails
    (arguments `(#:make-flags (list "NO_INTERACTION=1" "REPORT_EXIT_STATUS=1" "SKIP_ONLINE_TESTS=1")
		 #:test-target "test"
		 #:configure-flags
		 (list (string-append "--with-libxml-dir="
				      (assoc-ref %build-inputs "libxml2"))
		       ;; WARNING: Probably correct will be "--with-pdo-pgsql=shared,"
		       (string-append "--with-pdo-pgsql="
				      (assoc-ref %build-inputs "postgresql"))
		       "--enable-fpm")
                 #:phases
                 (modify-phases %standard-phases
		   (add-after 'unpack 'fix-hardcoded-path-and-loclaes
		     (lambda _
		       ;; fix path to sh
		       (substitute* '("run-tests.php" "ext/standard/proc_open.c")
			 (("/bin/sh") (which "sh")))
		       ;; Required for $path/include/iconv.h file from eglibc
		       (substitute* "configure"
			 (("/usr/local /usr")
			  (string-append (assoc-ref %build-inputs
						    ,(if (%current-target-system)
							 "cross-libc" "libc"))
					 " /usr/local /usr")))
		       ;; WARNING: "locale -a" call broken on GUIX SD.
		       (substitute* '("ext/standard/tests/strings/setlocale_basic1.phpt"
				      "ext/standard/tests/strings/setlocale_basic2.phpt"
				      "ext/standard/tests/strings/setlocale_basic3.phpt"
				      "ext/standard/tests/strings/setlocale_variation1.phpt"
				      "ext/standard/tests/strings/setlocale_variation2.phpt")
			 (("locale -a") "echo C ; echo POSIX ; echo en_US.utf8"))))
		   (add-after 'install 'install-configs-remove-timestamps
		     (lambda* (#:key outputs #:allow-other-keys)
		       (let ((out (assoc-ref outputs "out")))
			 ;; Preform step 4 from
			 ;; https://secure.php.net/manual/en/install.unix.nginx.php
			 (copy-file "php.ini-development"
				    (string-append out "/php/php.ini"))
			 (rename-file (string-append out "/etc/php-fpm.conf.default")
				      (string-append out "/etc/php-fpm.conf"))
			 ;; WARNING: Is it required?
			 (delete-file-recursively (string-append out "/lib/php/test"))
			 ;; WARNING: We can't patch "install-pear-nozlib.phar", so fix
			 ;; after installation: replace date in last field of file
			 (substitute*
			     (find-files (string-append out "/lib/php/.channels") ".*\\.reg")
			   (("\"[^\"]+\";[}]$") "\"Thu,  1 Jan 1970 00:00:01 +0000\";}"))
			 (substitute*
			     (find-files (string-append out "/lib/php/.registry") ".*\\.reg")
			   (("_lastmodified\";i:[0-9]+;[}]$") "_lastmodified\";i:1;}"))))))))
    (inputs `(("gawk" ,gawk)
	      ("libxml2" ,libxml2)
	      ("postgresql" ,postgresql)))
    ;; Required for generation of files and for tests
    (native-inputs `(("expect" ,expect)
		     ("perl" ,perl)
		     ("procps" ,procps)))
    (synopsis "PHP programming language")
    (description "PHP programming language")
    (home-page "http://www.php.net/")
    (license bsd-3)))
