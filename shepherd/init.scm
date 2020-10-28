;;; init.scm --- Shepherd init file

;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;; Copyright © 2020 Joseph LaFreniere (lafrenierejm) <joseph@lafreniere.xyz>

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(define-module (shepherd init)
  #:use-module (oop goops)
  #:use-module (shepherd service)
  #:use-module ((ice-9 ftw) :select (scandir))
  #:declarative? #f)

;; Load all the files in the directory 'init.d' with a suffix '.scm'.
(for-each
 (lambda (file)
   (load (string-append "init.d/" file)))
 (scandir (string-append (dirname (current-filename)) "/init.d")
          (lambda (file)
            (string-suffix? ".scm" file))))

;; Send shepherd into the background
(action 'shepherd 'daemonize)
