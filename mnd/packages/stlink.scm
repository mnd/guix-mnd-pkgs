;;; Copyright (C) 2016 Nikolay Merinov <nikolay.merinov@member.fsf.org>
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

(define-module (mnd packages stlink)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pkg-config))

(define-public stm8flash
  (let
      ((commit "c2dc8a5ff87f000b9b6f8eca372bb29788404ea3")
       (revision "1"))
    (package
      (name "stm8flash")
      (version (string-append "0.0-" revision "-" (string-take commit 9)))
      (source (origin
		(method git-fetch)
		(uri (git-reference 
		      (url "https://github.com/vdudouyt/stm8flash.git")
		      (commit commit)))
		(sha256
		 (base32
		  "0v0sg1njmy2w6ajhmn37d4kr0vm503n711pvq40pyzfzx3bhiyxl"))))
      (build-system gnu-build-system)
      (arguments `(#:make-flags (list "CC=gcc")
		   #:phases (modify-phases %standard-phases
			      (delete 'configure)
			      (delete 'check)
			      (replace 'install
				(lambda* (#:key outputs #:allow-other-keys)
				  (let* ((out (assoc-ref outputs "out"))
					 (bin (string-append out "/bin")))
				    (mkdir-p bin)
				    (copy-file "stm8flash" (string-append bin "/stm8flash"))))))))
      (inputs `(("libusb" ,libusb)))
      (native-inputs `(("pkg-config" ,pkg-config)))
      (synopsis "STM8 ST-LINK flash")
      (description "Utility to flash STM8 microcontrollers through ST-LINK v1/v2 interface")
      (home-page "https://github.com/vdudouyt/stm8flash")
      (license gpl2))))
