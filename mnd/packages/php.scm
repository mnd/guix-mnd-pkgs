;;; Copyright (C) 2015 Nikolay Merinov <nikolay.merinov@member.fsf.org>
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
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web))

(define-public php
  (package
    (name "php")
    (version "5.6.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.php.net/distributions/php-" version
                                  ".tar.xz"))
              (sha256
	        (base32
                  "1afcxfrka54iij6b1j8xlji1y6akcb04pyk3jqk9f2g647kf4lng"))
	      (patches (list (search-patch "php5-httpd2-support.patch")))))
    (build-system gnu-build-system)
    (arguments `(#:test-target "test"
		 #:configure-flags (list (string-append "--with-libxml-dir=" (assoc-ref %build-inputs "libxml2")) 
	                                 (string-append "--with-apxs2=" (assoc-ref %build-inputs "httpd") "/bin/apxs")
	                                 "--with-mysql")))
    (inputs `(("gawk" ,gawk)
	      ("libxml2" ,libxml2)))
    (propagated-inputs `(("httpd" ,httpd)))
    (synopsis "PHP programming language")
    (description "PHP programming language")
    (home-page "http://www.php.net/")
    (license bsd-3)))
