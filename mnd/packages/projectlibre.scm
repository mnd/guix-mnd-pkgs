;;; Copyright (C) 2020 Nikolai Merinov <nikolai.merinov@member.fsf.org>
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

(define-module (mnd packages projectlibre)
  #:use-module (guix packages)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ant)
  #:use-module (guix licenses))

(define-public wmname
  (package
    (name "wmname")
    (version "0.1")
    (source (origin
	      (method url-fetch)
	      (uri (string-append "https://dl.suckless.org/tools/wmname-" version
				  ".tar.gz"))
	      (sha256
	       (base32
		"1i82ilhbk36hspc2j0fbpg27wjj7xnvzpv1ppgf6fccina4d36jm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases (delete 'configure))
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "CC=gcc")
       #:tests? #f))                    ; no check target
    (inputs
     `(("libX11" ,libx11)))
    (synopsis "wmname prints/sets the window manager name property of the root window similar to how hostname(1) behaves.")
    (description "wmname prints/sets the window manager name property of the root window similar to how hostname(1) behaves.

wmname is a nice utility to fix problems with JDK versions and other broken programs assuming a reparenting window manager for instance.")
    (home-page "https://tools.suckless.org/x/wmname/")
    (license x11-style)))

(define-public projectlibre
  (let ((commit "a4f188f47459206c8c30219727f13f8ca2735cc7")
        (revision "0"))
    (package
      (name "projectlibre")
      (version (git-version "1.9.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://git.code.sf.net/p/projectlibre/code")
                       (commit commit)))
                (sha256
                 (base32
                  "0wx882r3c5ra5f5j2r1ymhw0aqdw76gvq0z8ndvlmy5qw1yx4m68"))))
      (build-system ant-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'build 'chdir-to-build.xml
             (lambda* _
               (chdir "projectlibre_build")
               #t))
	   (replace 'install
             ;; jar files was already installed on the 'build phase, install wrapper script to run
	     (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (projectlibre-root (string-append out "/share/projectlibre"))
                      (bin (string-append out "/bin")))
               (mkdir-p projectlibre-root)
               (install-file "packages/projectlibre-1.9.1/projectlibre.jar" projectlibre-root)
               (install-file "packages/projectlibre-1.9.1/readme.html" projectlibre-root)
               (copy-recursively "packages/projectlibre-1.9.1/lib" (string-append projectlibre-root "/lib"))
               (copy-recursively "packages/projectlibre-1.9.1/license" (string-append projectlibre-root "/license"))
               (substitute* "packages/projectlibre-1.9.1/projectlibre.sh"
	         (("PROJECTLIBRE_HOME=.*$") (string-append "PROJECTLIBRE_HOME=" projectlibre-root "\n"))
                 (("JAVA_INSTALL_DIR=.*$") (string-append "JAVA_INSTALL_DIR=" (assoc-ref inputs "jdk") "\n"))
	         (("JAVA_EXE=.*$") (string-append "JAVA_EXE=" (assoc-ref inputs "jdk") "/bin/java\n")))
               (chmod "packages/projectlibre-1.9.1/projectlibre.sh" #o755)
               (install-file "packages/projectlibre-1.9.1/projectlibre.sh" bin)
               #t))))
         #:build-target "dir"
	 #:tests? #f))
      (synopsis "ProjectLibre is project management software, the leading alternative to Microsoft Project.")
      (description "ProjectLibre is a project management software company with both a free open source desktop and upcoming Cloud version.
ProjectLibre desktop is a free and open-source project management software system intended ultimately as a standalone replacement for Microsoft Project.
ProjectLibre is written in the Java programming language, and will thus theoretically run on any machine for which a fully functioning Java Virtual Machine exists.

If ProjectLibre draws blank screen with your WM try to configre 'wmname LG3D' as WA.")
      (home-page "https://sourceforge.net/projects/projectlibre/")
      (license ((@@ (guix licenses) license) "CPAL 1.0"
                 "http://directory.fsf.org/wiki/License:CPALv1.0"
                 "https://www.socialtext.net/open/cpal_license_in_wikitext")))))
