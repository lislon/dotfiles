
;;;###autoload
(defun org-yamaps/collect ()
  "Collects latlng from all entries in current buffer.

Each entry can contain :YAICON: property to specify icon on map.
THe list of properties is avaliable here:
https://tech.yandex.ru/maps/doc/jsapi/2.1/ref/reference/option.presetStorage-docpage/

"
  (interactive)

  (let* ((entries (org-map-entries 'org-yamaps--for-each-entry "LATLNG<>0" 'agenda))
         (entries-flat (apply 'nconc entries))
         (json (json-encode entries-flat))
         (jsonp (concat "var data = " json ";")))

    (write-region jsonp nil "~/OneDrive/dotfiles/spacemacs/layers/my-org/local/org-yamaps/data.json")
    ))

(defun org-yamaps--for-each-entry ()
  (let* ((title (nth 4 (org-heading-components)))
         (points (org-entry-get-multivalued-property (point) "LATLNG"))
         (icon (org-entry-get (point) "YAICON"))
         (body (org-yamaps-get-clean-body)))

    (mapcar (lambda (point)
            `((title . ,title)
              (latlng . ,point)
              (body . ,body)
              (icon . ,icon))
            ) points)))

(defun org-yamaps-get-clean-body ()
  "Get contents of org-entry without drawlers and properties"
  (let* ((context (cadr (org-element-context)))
         (begin (plist-get context :contents-begin))
         (end    (plist-get context :contents-end))
         (contents-with-drawler (org-no-properties (buffer-substring begin end)))
         (contents (string-trim (replace-regexp-in-string "^\\(.\\|\n\\)+:END:" "" contents-with-drawler)))
         (contents-html (replace-regexp-in-string "\n" "<br/>" contents)))
    contents-html
    ))



(provide 'org-yamaps)
