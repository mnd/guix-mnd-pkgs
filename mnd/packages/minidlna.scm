;;; Copyright (C) 2018 Nikolai Merinov <nikolai.merinov@member.fsf.org>
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

(define-module (mnd packages minidlna)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages video)
  #:use-module (gnu packages image)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages gettext))

(define-public minidlna
  (package
   (name "minidlna")
   (version "1.2.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/minidlna/minidlna/"
                                version "/minidlna-" version ".tar.gz"))
            (sha256
             (base32
              "1v1ffhmaqxpvf2vv4yyvjsks4skr9y088853awsh7ixh7ai8nf37"))))
   (build-system gnu-build-system)
   (inputs `(("ffmpeg" ,ffmpeg)
             ("libjpeg" ,libjpeg-turbo)
             ("sqlite" ,sqlite)
             ("libexif" ,libexif)
             ("libid3tag" ,libid3tag)
             ("libogg" ,libogg)
             ("libvorbis" ,libvorbis)
             ("flac" ,flac)))
   (native-inputs `(("gettext" ,gettext-minimal)))
   (synopsis "Simple media server compliant with DLNA/UPnP-AV clients")
   (description "ReadyMedia (formerly known as MiniDLNA) is a simple media server software, with the aim of being fully compliant with DLNA/UPnP-AV clients.")
   (home-page "https://sourceforge.net/projects/minidlna/")
   (license gpl2)))
