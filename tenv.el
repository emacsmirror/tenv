;;; tenv.el --- Provide functions for test environment building

;; Copyright (C) 2013  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: test
;; URL: https://github.com/aki2o/elisp-test
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 

;; Enjoy!!!


(eval-when-compile (require 'cl))
(require 'advice)

(defun tenv-get-tmp-directory (prefix &optional initialized created)
  (let* ((tmppath (expand-file-name (or (getenv "TMP")
                                        (getenv "TMPDIR")
                                        (getenv "TEMP")
                                        "/tmp")))
         (dirpath (concat tmppath "/.emacs.tenv." prefix)))
    (when (and (file-directory-p dirpath)
               initialized)
      (ad-with-auto-activation-disabled
       (flet ((y-or-n-p (prompt) t)
              (yes-or-no-p (prompt) t))
         (loop with testpath = (concat dirpath "/")
               for b in (buffer-list)
               for f = (buffer-file-name b)
               if (and (stringp f)
                       (file-exists-p f)
                       (string= (file-name-directory f) testpath))
               do (with-current-buffer b
                    (set-buffer-modified-p nil)
                    (kill-buffer)))
         (loop for f in (directory-files dirpath)
               for fpath = (concat dirpath "/" f)
               if (file-regular-p fpath)
               do (delete-file fpath))
         (delete-directory dirpath))))
    (when (and (not (file-directory-p dirpath))
               created)
      (make-directory dirpath))
    dirpath))

(defun tenv-get-tmp-file (prefix &optional filenm initialized createdir)
  (let* ((dirpath (tenv-get-tmp-directory prefix nil createdir))
         (fileidx (loop for f in (directory-files dirpath)
                        count (and (not (string= f "."))
                                   (not (string= f "..")))))
         (filepath (concat dirpath "/" (or filenm
                                           (number-to-string (incf fileidx))))))
    (when (and (file-exists-p filepath)
               initialized)
      (ad-with-auto-activation-disabled
       (flet ((y-or-n-p (prompt) t)
              (yes-or-no-p (prompt) t))
         (loop for b in (buffer-list)
               for f = (buffer-file-name b)
               if (and (stringp f)
                       (file-exists-p f)
                       (string= f filepath))
               do (with-current-buffer b
                    (set-buffer-modified-p nil)
                    (kill-buffer)))
         (delete-file filepath))))
    filepath))

(defun tenv-update-file (filepath initialized &rest insert-strings)
  (ad-with-auto-activation-disabled
   (flet ((y-or-n-p (prompt) t)
          (yes-or-no-p (prompt) t))
     (with-current-buffer (find-file-noselect filepath)
       (when initialized
         (erase-buffer))
       (goto-char (point-max))
       (dolist (s insert-strings)
         (insert s))
       (save-buffer)
       (kill-buffer)))))

(provide 'tenv)
