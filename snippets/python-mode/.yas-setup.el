;;; -*- lexical-binding: t -*-

; Copyright (C) miscellaneous contributors, see git history
; Copyright (C) 2024 Daniel Hornung <d.hornung@indiscale.com>
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as
; published by the Free Software Foundation, either version 3 of the
; License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program. If not, see <https://www.gnu.org/licenses/>.

(require 'yasnippet)
(defvar yas-text)

(defvar yas-python-regex-identifier "[[:alnum:]_]+" "Simplified Python identifier.")
(defvar yas-python-regex-quoted-or-identifier (concat
                                               "\\("
                                               yas-python-regex-identifier
                                               "\\)"
                                               "\\|" "\".*\""
                                               "\\|" "'.*'"
                                               )
  "Simplified Python identifier or quoted string.
Does not work well with multiple or escaped quotes")

(defvar python-split-arg-regex
  (concat
   "\\(" yas-python-regex-identifier "\\)"     ; name
   "\\(:[[:blank:]]*\\([][:alpha:]_[]*\\)\\)?" ; type
   "\\([[:blank:]]*=[[:blank:]]*\\("
   yas-python-regex-quoted-or-identifier       ; default
   "\\)\\)?"
   )
"Regular expression matching an argument of a python function.
Groups:
- 1: the argument name
- 3: the type
- 5: the default value")

(defvar python-split-arg-separator
"[[:space:]]*,[[:space:]]*"
"Regular expression matching the separator in a list of argument.")

(defun python-split-args (arg-string)
  "Split python argument string ARG-STRING.

The result is a list ((name, type, default), ...) of argument names, types and
default values.  An argument named `self` is omitted."
  (remove
   nil
   (mapcar (lambda (x)           ; organize output
             (when (and
                    (not (equal "self" x))
                    (string-match python-split-arg-regex x)
                    )
               (list
                (match-string-no-properties 1 x) ; name
                (match-string-no-properties 3 x) ; type
                (match-string-no-properties 5 x) ; default
                )))
           (split-string arg-string python-split-arg-separator t))))

(defun python-args-to-docstring ()
  "Return docstring format for the python arguments in yas-text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args yas-text))
         (max-len (if args (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args)) 0))
         (formatted-args (mapconcat
                          (lambda (x)
                            (concat (nth 0 x) (make-string (- max-len (length (nth 0 x))) ? ) " -- "
                                    (if (nth 1 x) (concat (nth 1 x) ": "))
                                    (if (nth 2 x) (concat "\(default " (nth 2 x) "\)"))
                                    ))
                          args
                          indent)))
    (unless (string= formatted-args "")
      (mapconcat 'identity (list "Keyword Arguments:" formatted-args) indent))))

(defun python-args-to-docstring-numpy ()
  "return docstring format for the python arguments in yas-text"
  (let* ((args (python-split-args yas-text))
         (format-arg (lambda(arg)
                       (concat (nth 0 arg) " : "            ; name
                               (if (nth 1 arg) (nth 1 arg)) ; type  TODO handle Optional[Foo] correctly
                               (if (nth 2 arg) (concat (when (nth 1 arg) ", ")
                                                       "default=" (nth 2 arg))) ; default
                               "\n")))
         (formatted-params (mapconcat format-arg args "\n"))
         (formatted-ret (mapconcat format-arg (list (list "out")) "\n")))
    (unless (string= formatted-params "")
      (mapconcat 'identity
                 (list "\nParameters\n----------" formatted-params
                       "\nReturns\n-------" formatted-ret)
                 "\n"))))


;; Tests
;; (ert-deftest test-split ()
;;   "For starters, only test a single string for expected output."
;;   (should (equal
;;            (python-split-args "_foo='this', bar: int = 2, baz: Optional[My_Type], foobar")
;;            (list '("_foo" nil "'this'")
;;                  '("bar" "int" "2")
;;                  '("baz" "Optional[My_Type]" nil)
;;                  '("foobar" nil nil)))
;;   ))

;; (ert-deftest test-argument-self ()
;;   "If an argument is called `self`, it must be omitted"
;;   (should (equal
;;            (python-split-args "self, _foo=\"this\"")
;;            (list '("_foo" nil "\"this\"")
;;                  ))
;;   ))

;; For manual testing and development:

;; (setq yas-text "foo=3, bar: int = 2, baz: Optional[MyType], foobar")
;; (split-string yas-text python-split-arg-separator t)
;;
;; (save-match-data
;;   (setq my-string "_foo: my_bar = 'this'")
;;   (string-match python-split-arg-regex my-string)
;;   (match-string 5 my-string)
;;   )
;;
;; (python-split-args yas-text)
;; (python-args-to-docstring)
;; (python-args-to-docstring-numpy)
