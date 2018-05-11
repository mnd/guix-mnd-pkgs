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

(define-module (mnd services minidlna)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (mnd packages minidlna)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (minidlna-service))

(define-record-type* <minidlna-configuration>
  minidlna-configuration make-minidlna-configuration
  minidlna-configuration?
  (minidlna      minidlna-configuration-minidlna      ;<package>
                 (default minidlna))
  (user          minidlna-configuration-user          ;string
                 (default "minidlna"))
  (mediadir      minidlna-configuration-mediadir)     ;string
  (dbdir         minidlna-configuration-dbdir         ;string
                 (default "/var/cache/minidlna"))
  (logdir        minidlna-configuration-logdir        ;string
                 (default "/var/log"))
  (pidfile       minidlna-configuration-pidfile       ;string
                 (default "/var/run/minidlna/minidlna.pid"))
  (extra-config  minidlna-configuration-extra-config  ;string
                 (default ""))
  (extra-options minidlna-configuration-extra-options ;list of strings
                 (default '())))

;;; TODO: There should be minissdpd service that will be used by minidlna
;;; to allow several UPnP devices on single machine
(define minidlna-config-file
  (match-lambda
    (($ <minidlna-configuration> minidlna user mediadir dbdir logdir pidfile extra-config extra-options)
     (plain-file "minidlna.conf"
                 (string-append "#Generated by minidlna-service
port=8200
user=" user "
media_dir=" mediadir "
db_dir=" dbdir "
log_dir=" logdir "
album_art_names=Cover.jpg/cover.jpg/AlbumArtSmall.jpg/albumartsmall.jpg/AlbumArt.jpg/albumart.jpg/Album.jpg/album.jpg/Folder.jpg/folder.jpg/Thumb.jpg/thumb.jpg
inotify=yes
strict_dlna=no
" extra-config)))))

(define minidlna-accounts
  (match-lambda
    (($ <minidlna-configuration> minidlna user mediadir dbdir logdir pidfile extra-config extra-options)
     (list (user-group (name user) (system? #t))
           (user-account
            (name user)
            (group user)
            (system? #t)
            (comment "minidlna daemon system account")
            (home-directory mediadir) ; "/var/empty" ?
            (shell (file-append shadow "/sbin/nologin")))))))

(define minidlna-activation
  (match-lambda
    (($ <minidlna-configuration> minidlna user mediadir dbdir logdir pidfile extra-config extra-options)
     #~(begin
         (use-modules (guix build utils))
         (mkdir-p #$mediadir)
         (mkdir-p #$dbdir)
         (mkdir-p #$logdir)
         (mkdir-p #$(dirname pidfile))))))

(define (minidlna-shepherd-service config)
  (match config
    (($ <minidlna-configuration> minidlna user mediadir dbdir logdir pidfile extra-config extra-options)
     (list (shepherd-service
            (provision '(minidlna))
            (documentation "Run minidlna daemon")
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      (list #$(file-append minidlna "/sbin/minidlnad")
                            "-d" "-f" #$(minidlna-config-file config)
                            "-P" #$pidfile)
                      #:pid-file #$pidfile))
            (stop #~(make-kill-destructor)))))))

(define minidlna-service-type
  (service-type (name 'minidlna)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          minidlna-shepherd-service)
                       (service-extension activation-service-type
                                          minidlna-activation)
                       (service-extension account-service-type
                                          minidlna-accounts)))))

(define* (minidlna-service #:key (minidlna minidlna)
                           (user "minidlna")
                           (mediadir "/var/lib/minidlna")
                           (dbdir "/var/cache/minidlna")
                           (logdir "/var/log")
                           (pidfile "/var/run/minidlna/minidlna.pid")
                           (extra-config  "")
                           (extra-options '()))
  "Return a service that runs @command{minidlnad} daemon with specified @var{user} and directories"
  (service minidlna-service-type
           (minidlna-configuration
            (minidlna minidlna) (user user)
            (mediadir mediadir) (dbdir dbdir)
            (logdir logdir) (pidfile pidfile)
            (extra-config extra-config)
            (extra-options extra-options))))


