(setq yandex-geoapi-url "https://geocode-maps.yandex.ru/1.x/?geocode=%s&format=json&ll=30.355223,59.910659&spn=0.9,0.9&rspn=1")

(defgroup helm-yandex-geoapi '()
  "Customization group for `helm-yandex-geoapi'."
  :link '(url-link "http://example.com")
  :group 'convenience
  :group 'comm)

(defvar helm-yandex-geoapi-input-history nil)
(defvar helm-yandex-geoapi-pending-query nil)

(defcustom helm-yandex-geoapi-actions
  '(("Org insert latlng" . helm-yandex-geoapi-insert-point-property)
    ("browse yandex maps" . helm-yandex-geoapi-action-browser-url) )
  "List of actions for helm-yandex-geoapi sources."
  :group 'helm-yandex-geoapi
  :type '(alist :key-type string :value-type function))

(defun helm-yandex-geoapi-action-browser-url (candidate)
  (interactive "P")
  (browse-url (format "https://yandex.ru/maps/2/saint-petersburg/?mode=search&text=%s" candidate)))

(defun helm-yandex-geoapi-insert-point-property (candidate)
  "Inserts point to org-property drawler"
  (org-entry-add-to-multivalued-property (point) "LATLNG"
                                         (replace-regexp-in-string " " "," candidate)))

(setq helm-source-yandex-geoapi-data-source
  `((name . "Yandex maps")
    (action . helm-yandex-geoapi-actions)
    (display-to-real . helm-google-display-to-real)
    (candidates . helm-yandex-geoapi-search )
    (requires-pattern)
    (nohighlight)
    ;; (multiline)
    (match . identity)
    (volatile)
    ))

(defun helm-google-display-to-real (candidate)
  "Retrieve the URL from the results for the action."
  (string-match "\(\\([^)]+\\)\)" candidate)
  (match-string 1 candidate))

;;;###autoload
(defun helm-yandex-geoapi (&optional arg)
  "Preconfigured `helm' : Google search."
  (interactive)
  (let ((region
         (if (not arg)
             (if (use-region-p)
               (buffer-substring-no-properties
                (region-beginning)
                (region-end))
               (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))
               )
           arg))
        (helm-input-idle-delay 0.4))
    (helm :sources 'helm-source-yandex-geoapi-data-source
          :prompt "Yandex maps: "
          :input region
          :buffer "*helm yandex geo*"
          :history 'helm-yandex-geoapi-input-history)))

;; (helm-add-action-to-source "Raz" (lambda (candidate) (insert candidate) ) helm-source-yandex-geoapi-data-source)

(defun helm-yandex-geoapi-search ()
  "Get Google results by scraping the website."
  (let* ((results (helm-yandex-geoapi--search helm-pattern)))
    (mapcar (lambda (result)
              ;; (plist-get result :title)
              (let* ((geo-point (plist-get result :point))
                    (geo-point-lat-lng (string-join (reverse (split-string geo-point " ")) " ")))
                (concat
                 (propertize
                  (plist-get result :title)
                  'face 'font-lock-variable-name-face)
                 (propertize (format " (%s)" geo-point-lat-lng)
                             'face 'font-lock-comment-face)
                 ))
              )
            results)))

(defun helm-yandex-geoapi--search (text)
  (let* ((buf (helm-yandex-geoapi--response-buffer-from-search text))
         (results (helm-yandex-geoapi--parse buf)))
    results))

(defun helm-yandex-geoapi--response-buffer-from-search (text &optional search-url)
  (let ((url-mime-charset-string "utf-8")
        (url (format (or search-url yandex-geoapi-url) (url-hexify-string text))))
    (url-retrieve-synchronously url t)))

(defun helm-yandex-geoapi--parse (buf)
  (helm-yandex-geoapi--with-buffer buf
                            (let (results result)
                              (while (re-search-forward "\"text\":\"\\([^\"]+\\)\"" nil t)
                                (setq result (plist-put result :title (match-string-no-properties 1)))
                                (re-search-forward "\"pos\":\"\\([^\"]+\\)\"" nil t)
                                (setq result (plist-put result :point (match-string-no-properties 1)))
                                (add-to-list 'results result t)
                                (setq result nil))
                              results)))

(defmacro helm-yandex-geoapi--with-buffer (buf &rest body)
  (declare (doc-string 3) (indent 2))
  `(with-current-buffer ,buf
     (set-buffer-multibyte t)
     (goto-char url-http-end-of-headers)
     (prog1 ,@body
       (kill-buffer ,buf))))

(provide 'helm-yandex-geoapi)
