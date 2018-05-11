;;; Copyright (C) 2017 Nikolai Merinov <nikolai.merinov@member.fsf.org>
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

(define-module (mnd packages coreutils)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages attr))

(define-public coreutils-with-xattr
  (package
    (inherit coreutils)
    (name "coreutils-with-xattr")
    (inputs `(("attr"  ,attr)
	      ,@(package-inputs coreutils)))))
