;;; bdo.el --- Do things to a browser page from Emacs. BETA!

;; Copyright (c) 2012 Chris Done. All rights reserved.

;; Author:   Chris Done <chrisdone@gmail.com>
;; Created:  07-April-2012
;; Version:  0.2
;; Keywords: development

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
;; Chris Done BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
;; OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.
;;
;;; Commentary:
;;
;;; Usage:
;;

;;; Code:

(require 'cl)

(defcustom bdo-port
  9090
  "The bdo port."
  :type 'integer
  :group 'bdo)

(defun bdo-refresh ()
  (interactive)
  (bdo-command "update"))

(defun bdo-clients ()
  (let ((clients (split-string (bdo-command "clients")
                               "\n\n")))))

(defun bdo-command (command)
  (shell-command-to-string (format "echo %s | nc localhost %d"
                                   command
                                   bdo-port)))
