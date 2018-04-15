;;; Copyright © 2018 Nikolai Merinov <nikolai.merinov@member.fsf.org>
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

(define-module (mnd packages rust)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix utils))

(define-public rust-1.25
  (let ((base-rust ((@@ (guix profiles) find-package) "rust" "1.24.1")))
    (package
      (inherit base-rust)
      (version "1.25.0")
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://static.rust-lang.org/dist/"
                             "rustc-" version "-src.tar.gz"))
         (sha256 (base32 "0baxjr99311lvwdq0s38bipbnj72pn6fgbk6lcq7j555xq53mxpf"))
         (modules '((guix build utils)))
         (snippet '(begin (delete-file-recursively "src/llvm") #t))))
      (native-inputs
       (alist-replace "cargo-bootstrap" (list base-rust "cargo")
                      (alist-replace "rustc-bootstrap" (list base-rust)
                                     (package-native-inputs base-rust)))))))
