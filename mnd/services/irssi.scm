;;; Copyright (C) 2016 Nikolai Merinov <nikolai.merinov@member.fsf.org>
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

(define-module (mnd services irssi)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages irc) ; irssi
  #:use-module (gnu packages screen)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (irssi-service))

;;; Commentary:
;;;
;;; RSS service.
;;;
;;; Code:

(define-record-type* <irssi-configuration>
  irssi-configuration make-irssi-configuration
  irssi-configuation?
  (irssi         irssi-configuration-irssi        ;<package>
                 (default irssi))
  (screen        irssi-configuration-screen       ;<package>
                 (default screen))
  (user          irssi-configuration-user)         ;string
  (group         irssi-configuration-group         ;string
                 (default "users"))
  (extra-options irssi-configuration-extra-options       ;list of strings
                 (default '())))

;;; WARNING: Are we can use any of next ways to select user:
;;; 1. Use #$sudo utility from (gnu packages admin)
;;; 2. Take /var/setuid-wrappers/sudo as in https://nixos.org/wiki/NixOS:extend_NixOS
;;; 3. Specify user, group, directory and environment to make-forkexec-constructor

(define irssi-shepherd-service
  (match-lambda
    (($ <irssi-configuration> irssi screen user group options)
        (let ((user-home (passwd:dir (getpw user))))
          (list (shepherd-service
                 (provision '(irssi))
                 (documentation "Run the irssi client in screen.")
                 (requirement '(user-processes))
                 (start #~(begin
                            (use-modules (srfi srfi-1)   ; (remove pred list)
                                         (srfi srfi-26)) ; (cut function arg1 <> arg3)
                            (make-forkexec-constructor
                             (list
                              (string-append #$screen "/bin/screen")
                              "-D" "-m" "-S" "irc"
                              (string-append #$irssi "/bin/irssi")
                              #$@options)
                             #:user #$user #:group #$group
                             #:directory #$user-home
                             #:environment-variables
                             (cons (string-append "HOME=" #$user-home)
                                   (remove #;(cut string-prefix? "HOME=" <>) ; Why this code fail with "ERROR: Unbound variable: <>"?
                                    (lambda (x) (string-prefix? "HOME=" x)) (environ))))))
                 (stop #~(make-kill-destructor))))))))

(define irssi-service-type
  (service-type (name 'irssi)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          irssi-shepherd-service)))))

(define* (irssi-service #:key (irssi irssi)
                        (screen screen)
                        user
                        (group "users")
                        (extra-options '()))
  "Return a service that runs irssi IRC client under screen from specified @var{user}.

Finally, @var{extra-options} is a list of additional command-line options
passed to @command{irssi}."
  (service irssi-service-type
           (irssi-configuration
            (irssi irssi) (screen screen)
            (user user) (group group)
            (extra-options extra-options))))

;;; irssi.scm ends here
