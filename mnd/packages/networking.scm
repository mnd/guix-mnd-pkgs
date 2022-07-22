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

(define-module (mnd packages networking)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages vpn))


(define-public strongswan-configurable
  (package/inherit strongswan
    (name "strongswan-configurable")
    (inputs `(("libsoup-2" ,libsoup-minimal-2)
              ,@(package-inputs strongswan)))
    (arguments
     (substitute-keyword-arguments (package-arguments strongswan)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'patch-script-paths
             (lambda _
               (substitute* "src/ipsec/_ipsec.in"
                 (("PATH=.*$") "PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin\n"))
               #t))
           (add-after 'unpack 'read-/etc/ipsec.secrets
             (lambda _
               (with-output-to-file "src/starter/ipsec.secrets"
                 (lambda _
                   (display "include /etc/ipsec.secrets")))
               #t))))))))

(define-public network-manager-l2tp
  (package
    (name "network-manager-l2tp")
    (version "1.2.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nm-l2tp/NetworkManager-l2tp.git")
                    (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0h0v8gpi2r7ldjg1n64qgv44bxz8jphdcb5l3pbq966bdpapivzh"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; Use abolute path to the plugin files
       (list "--enable-absolute-paths=yes" "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-configuration
           (lambda _
             ;; configure should be started after `patch-generated-file-shebangs'
             (substitute* "autogen.sh"
               (("\\./configure .*") "true"))
             #t))
         (add-after 'unpack 'patch-utilities
           (lambda _
             (substitute* "shared/utils.c"
               (("/sbin/ipsec") (which "ipsec"))
               (("/sbin/xl2tpd") (which "xl2tpd")))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("glib:bin" ,glib "bin")))
    (inputs
     `(("gtk+" ,gtk+)
       ("network-manager" ,network-manager)
       ("network-manager-applet" ,network-manager-applet)
       ("libsecret" ,libsecret)
       ("ppp" ,ppp)
       ("strongswan" ,strongswan-configurable)
       ("xl2tpd" ,xl2tpd)))
    (home-page "https://github.com/nm-l2tp/NetworkManager-l2tp/")
    (synopsis "l2tp plugin for NetworkManager")
    (description "This extension of NetworkManager allows it to take case of
connections to l2tp virtual private networks")
    (license license:gpl2+)
    (properties `((upstream-name . "NetworkMananger-l2tp")))))
