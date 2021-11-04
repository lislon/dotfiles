(defcustom csv+-quoted-newline "\^@"
  "Replace for newlines in quoted fields."
  :group 'sv
  :type 'string)

(defun csv+-quoted-newlines (&optional b e inv)
  "Replace newlines in quoted fields of region B E by `csv+-quoted-newline'.
B and E default to `point-min' and `point-max', respectively.
If INV is non-nil replace quoted `csv+-quoted-newline' chars by newlines."
  (interactive
   (append (when (region-active-p)
         (list (region-begin)
           (region-end)))
       prefix-arg))
  (unless b (setq b (point-min)))
  (unless e (setq e (point-max)))
  (save-excursion
    (goto-char b)
    (let ((from (if inv csv+-quoted-newline "\n"))
      (to (if inv "\n" csv+-quoted-newline)))
      (while (search-forward from e t)
    (when (nth 3 (save-excursion (syntax-ppss (1- (point)))))
      (replace-match to))))))

(defsubst csv+-quoted-newlines-write-contents ()
  "Inverse operation of `csv+-quoted-newlines' for the full buffer."
  (save-excursion
    (save-restriction
      (widen)
      (let ((file (buffer-file-name))
        (contents (buffer-string)))
    (with-temp-buffer
      (insert contents)
      (csv+-quoted-newlines (point-min) (point-max) t)
      (write-region (point-min) (point-max) file)))))
  (set-visited-file-modtime)
  (set-buffer-modified-p nil)
  t ;; File contents has been written (see `write-contents-functions').
  )

(defun csv+-setup-quoted-newlines ()
  "Hook function for `csv-mode-hook'.
Transform newlines in quoted fields to `csv+-quoted-newlines'
when reading files and the other way around when writing contents."
  (add-hook 'write-contents-functions #'csv+-quoted-newlines-write-contents t t)
  (let ((modified-p (buffer-modified-p)))
    (csv+-quoted-newlines)
    (set-buffer-modified-p modified-p)))

