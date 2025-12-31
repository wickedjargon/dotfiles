;;; consult-erc.el --- ERC-related buffer candidate sources for consult-buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Mekeor Melire <mekeor@posteo.de>
;; Created: 19 Jan 2024
;; Homepage: https://codeberg.org/mekeor/emacs-consult-erc
;; Keywords: chat, completion, irc
;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; Package-Requires: (
;;   (emacs "27.1")
;;   (consult "1.0")
;;   (marginalia "1.0"))
;; Package-Version: 0.2
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Code:

(require 'erc)
(require 'erc-common)
(require 'erc-match)

(require 'consult)
(require 'marginalia)

(defgroup consult-erc nil
  "Variables and faces for `consult-erc' package."
  :group 'consult
  :group 'erc)

;;;; Marginalia

(defface consult-erc-annotation-server
  '((t :weight light))
  "Face for the annotation-field `server' of category `erc-buffer'."
  :group 'consult-erc)

(defface consult-erc-annotation-topic
  '((t :slant italic))
  "Face for the annotation-field `topic' of category `erc-buffer'."
  :group 'consult-erc)

(defun consult-erc-annotator (buf)
  (marginalia--fields
   ((or (with-current-buffer buf (erc-network-name)) "")
    :face 'consult-erc-annotation-server
    :width 11)
   ((or (with-current-buffer buf erc-channel-topic) "")
    :face 'consult-erc-annotation-topic)))

(add-to-list 'marginalia-annotators
             '(erc-buffer consult-erc-annotator builtin none))

;;;; Consult

;;;;; Buffer candidate sources

(defvar consult--erc-channel-buffer
  `( :name     "ERC Channel Buffer"
     :narrow   ?c
     :hidden   nil
     :category erc-buffer
     :face     consult-buffer
     :history  buffer-name-history
     :state    ,#'consult--buffer-state
     :items
     ,(lambda ()
        (consult--buffer-query
         :sort 'visibility
         :as #'buffer-name
         :predicate
         (lambda (buf)
           (with-current-buffer buf
             (erc--target-channel-p erc--target))))))
  "ERC-channel buffer candidate source for `consult-buffer'.")

(defvar consult--erc-log-buffer
  `( :name     "ERC Log Buffer"
     :narrow   ?l
     :hidden   t
     :category erc-buffer
     :face     consult-buffer
     :history  buffer-name-history
     :state    ,#'consult--buffer-state
     :items
     ,(lambda ()
        (consult--buffer-query
          :sort 'visibility
          :as #'buffer-name
          :predicate
          (lambda (buf)
            (member
              (buffer-name buf)
              (mapcar #'cdr erc-log-matches-types-alist))))))
  "ERC-log-matches buffer candidate source for `consult-buffer'.
See `erc-log-matches-types-alist' from `erc-match' module.")

(defvar consult--erc-query-buffer
  `( :name     "ERC Query Buffer"
     :narrow   ?q
     :hidden   nil
     :category erc-buffer
     :face     consult-buffer
     :history  buffer-name-history
     :state    ,#'consult--buffer-state
     :items
     ,(lambda ()
        (consult--buffer-query
          :sort 'visibility
          :as #'buffer-name
          :predicate #'erc-query-buffer-p)))
  "ERC-query buffer candidate source for `consult-buffer'.")

(defvar consult--erc-server-buffer
  `( :name     "ERC Server Buffer"
     :narrow   ?s
     :hidden   t
     :category erc-buffer
     :face     consult-buffer
     :history  buffer-name-history
     :state    ,#'consult--buffer-state
     :items
     ,(lambda ()
        (consult--buffer-query
          :sort 'visibility
          :as #'buffer-name
          :predicate #'erc-server-or-unjoined-channel-buffer-p)))
  "ERC-server buffer candidate source for `consult-buffer'.")

(defcustom consult-erc-buffer-sources
  '( consult--erc-channel-buffer
     consult--erc-log-buffer
     consult--erc-query-buffer
     consult--erc-server-buffer)
  "Sources used by `consult-erc'. See `consult--multi' for a
description of the source data structure."
  :group 'consult-erc
  :type '(repeat symbol))

;;;;; Commands

;;;###autoload
(defun consult-erc ()
  "Streamlined `consult-buffer' command for ERC-related buffers."
  (interactive)
  (consult-buffer consult-erc-buffer-sources))

;;;###autoload
(defun consult-erc-dwim (&optional arg)
  "If an ERC process is running, read an ERC-related buffer with
`consult-erc' and switch to it; otherwise, start ERC."
  (interactive "P")
  (if (or arg (not (erc-buffer-list #'erc-open-server-buffer-p)))
    (erc-tls)
    (consult-erc)))

(provide 'consult-erc)

;;; consult-erc.el ends here
