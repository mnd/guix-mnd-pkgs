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

(define-module (mnd packages chrpath)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses))

(define-public chrpath
  (package
    (name "chrpath")
    (version "0.16")
    (source (origin
	      (method url-fetch)
	      (uri (string-append "https://alioth.debian.org/frs/download.php/latestfile/813/chrpath-" version
				  ".tar.gz"))
	      (sha256
	       (base32
		"0yvfq891mcdkf8g18gjjkn2m5rvs8z4z4cl1vwdhx6f2p9a4q3dv"))))
    (build-system gnu-build-system)
    (synopsis "Command line tool to adjust the RPATH or RUNPATH of ELF binaries")
    (description "Command line tool to adjust the RPATH or RUNPATH of ELF binaries")
    (home-page "https://alioth.debian.org/projects/chrpath/")
    (license gpl2)))
