(defvar my-skillbox//base-dir "~/OneDrive/shared/skillbox" "Directory with answers")
(defvar my-skillbox//templates-dir (concat my-skillbox//base-dir "/00000_lislon/tpls") "Directory with templates")

(defvar my-skillbox//downloads-dir  "D:\\Downloads")
(defvar my-skillbox//7z-path "c:\\opt\\7-Zip\\7z.exe")
(defvar my-skillbox//react-student-root (expand-file-name "~/src/react/students/"))
(setq my-skillbox//7z-extensions '("zip" "tar" "7z" "rar" "zipx"))
(setq my-skillbox//non-7z-extensions '("txt" "sql" "java" "tsx" "jsx" "ts"))
(setq my-skillbox//courses '("java" "react"))

(setq my-skillbox//modules '(
                             "1 pro"
                             "2 pro income calc"
                             "3.1 pro"
                             "3.2 pro"
                             "3.3 pro"
                             "4.1 pro"
                             "4.2 pro"
                             "4.3 pro"
                             "5.1 pro"
                             "5.2 pro"
                             "5.3 pro"
                             "6.1 pro"
                             "6.2 pro"
                             "6.3 pro"
                             "6.4 pro"
                             "7.1 pro int & sum digits"
                             "7.2 pro max and min"
                             "7.3 pro containers"
                             "7.4 pro print code symbols"
                             "7.5 pro english text"
                             "7.6 pro birthday"
                             "8.1 pro print X"
                             "8.2 pro todo add/edit/delete"
                             "8.3 pro email"
                             "8.4 pro phone and name"
                             "8.5 pro blat numbers"
                             "9.1 pro bank 1% comission or 1 month"
                             "9.2 pro private public"
                             "9.3 pro bank 3 classa"
                             "9.4 pro company with 3 types of employees"
                             "10.1 pro sort employees"
                             "10.2 pro max salary 2017"
                             "11.1 pro catch exception"
                             "11.3 pro log4j logging"
                             "11.4 pro debug applications"
                             "11.5 pro spbmetro"
                             "12.1x pro folder size"
                             "12.2x pro copy folder"
                             "12.3x pro parsing csv"
                             "12.4x pro lenta.ru html"
                             "12.5x pro wiki parsing"
                             "12.9x sql module"
                             "12.1 pro jdbc avg orders"
                             "12.2 pro hibernate course info"
                             "12.3 pro full tables"
                             "12.4 pro PurchaseList normalize"
                             "13.1 pro image resize"
                             "13.2 pro concurrent money transfer"
                             "13.3 pro sitemap"
                             "14.1 pro spring random date"
                             "14.2 pro spring todolist entities"
                             "14.3 pro spring db add remove"
                             "14.4 pro spring thymleaf"
                             "14.5 pro spring jar"
                             "14.1 pro redis queries"
                             "16.2 pro redis first program"
                             "16.3 pro mongo queries"
                             "16.4 pro mongo magazin"
                             "17.1 pro optimize cat number"
                             "17.2 pro optimize xml memory"
                             "17.3 pro optimize xml load"
                             "18.1 pro hdfs implement"
                             "18.2 pro spark word count"
                             "19.1 pro swing fio"
                             "20.1 pro diploma"
                             "21.1 pro alhgroritms"
                             "22.1 pro linked lists"
                             "22.2 pro binary tree"
                             "22.3 pro suffix tree"
                             ;; "20.1 pro http"
                             "5 old blat"
                             "6 old polimorfism"
                             "7 old testing"
                             "8 old gui basics"
                             "10 old files network"
                             "11 old database"
                             "12 old multithreading"
                             "13 old optimization"
                             "14 old web"
                             "react 1"
                             "react 2"
                             "react 3"
                             ))

(defun my-skillbox//answer-candidates ()
  (let (collected-files)
    (dolist (filename (directory-files my-skillbox//templates-dir))
      (let ((file-extension (file-name-extension filename)))
        (when (equal file-extension "org")
          (push (concat (file-name-as-directory my-skillbox//templates-dir) filename) collected-files))))

    (mapcar (lambda (r)
              (let* ((basename (file-name-nondirectory r))
                     (no-extension (file-name-sans-extension basename))
                     (clean-name (replace-regexp-in-string "-" " " no-extension)))
                `(,clean-name . ,r))) collected-files)))


(defun my-skillbox//helm-answer-templates ()
  (interactive)
  "Source of templates"
  (helm :sources (helm-build-sync-source "Answer templates"
                   :candidates #'my-skillbox//answer-candidates
                   :keymap helm-map
                   :action '(
                            ("Create message" . my-skillbox//insert-template)
                            ("Edit" . find-file)
                            ("New template" . (lambda(_) (call-interactively #'my-skillbox//new-template)))
                            ))
        :buffer "*helm sync source*"))

(defun my-skillbox//auto-insert-module-template (module)
  "Auto insert module template"
  (dolist (candidate (my-skillbox//answer-candidates))
    (when (string-match-p (concat "^" module " skeleton pro") (car candidate))
      (my-skillbox//insert-template (cdr candidate)))))

(defun my-skillbox//insert-template (filename)
  (interactive)
  (insert-file-contents filename)
  (save-excursion
    (let ((name (my-skillbox//get-student-name-from-path (buffer-file-name))))
           (while (re-search-forward "{{name}}" nil t)
             (replace-match name)))
    (let ((name (my-skillbox//get-student-id-from-path (buffer-file-name))))
      (while (re-search-forward "{{id}}" nil t)
        (replace-match name))) ))

(defun my-skillbox//git-pull ()
  (interactive)
  (let ((default-directory (concat (my-skillbox//get-git-dir-from-inside) "/github")))
    (when (file-exists-p default-directory)
      (magit-call-git "stash" "-u")
      (magit-call-git "clean" "-f")
      (magit-call-git "pull"))
    )
  )

(defun my-skillbox//get-student-name-from-path (path)
  (string-match "_\\([^\s]+\\)" path)
  (match-string 1 path))

(defun my-skillbox//get-student-id-from-path (path)
  (string-match "[a-z0-9]+_[^\s]+" path)
  (match-string 0 path))

(defun my-skillbox//get-git-dir-from-inside ()
  "Return git from filename inside user's module"

  (let* ((student-name (my-skillbox//get-current-student)))
         (pcase (my-skillbox//get-current-student-course)
           ('java (concat (my-skillbox//get-student-root-by-student-name student-name)))
           ('react (concat my-skillbox//react-student-root student-name))
           (error "wtf")
           )))

(defun my-skillbox//get-current-module-dir ()
  (string-match "^.+_[^/]+/[^/]+" (buffer-file-name))
  (match-string 0 (buffer-file-name)))

(defun my-skillbox//new-template (template-name)
  (interactive "sNew template name: ")
  (let* ((filename (replace-regexp-in-string " " "-" template-name))
         (pathname (concat (file-name-as-directory my-skillbox//templates-dir) filename ".org")))
    (find-file pathname)
    ))


(defun org-html-quote-block (quote-block contents _info)
  "Adds a > before quotes"
  (format "<blockquote%s>\n%s</blockquote>"
          (let* ((name (org-element-property :name quote-block))
                 (attributes (org-export-read-attribute :attr_html quote-block))
                 (a (org-html--make-attribute-string
                     (if (or (not name) (plist-member attributes :id))
                         attributes
                       (plist-put attributes :id name)))))
            (if (org-string-nw-p a) (concat " " a) ""))
          (replace-regexp-in-string "<p>" "<p> &gt;" contents)))

(defun my-skillbox//get-current-module()
  "Get current module, eg. 4.1"
  (let* ((filename (buffer-file-name)))
    (string-match "^.+?\\([^/_]+_[^/]+\\)/\\([^/]+\\)" filename)
      (replace-regexp-in-string "[a-z]" "" (match-string 2 filename))
  ))

(defun my-skillbox//get-current-student()
  "Get current student name, eg. 12345_Boris Elzin"
  (let* ((filename (buffer-file-name)))
    (string-match "^.+?\\([^/_]+_[^/]+\\)" filename)
    (match-string 1 filename)
    ))

(defun my-skillbox//get-current-student-nice()
  "Get current student name, eg. 12345: Boris Elzin"
  (replace-regexp-in-string "_" ": " (my-skillbox//get-current-student))
  )


(defun my-skillbox//get-current-student-course()
  "Get current student's course name, eg. 'java"
  (my-skillbox//get-course-by-student (my-skillbox//get-current-student)))


(defun my-skillbox/copy-string-for-report ()
  "Copy report string to clipboard to paste it in excel"
  (interactive)
  (message "%s" (kill-new (let* ((date (format-time-string "%d.%m.%Y %H:%M:%S"))
                                 (student (my-skillbox//get-current-student-nice))
                                 (module (my-skillbox//get-current-module))
                                 (module-name (my-skillbox//get-module-for-report module))
                                 (success (let* ((ok (my-skillbox//string-exists-p "{{{OK}}}"))
                                                 (fail (my-skillbox//string-exists-p "{{{FAIL}}}")))
                                            (cond
                                             (fail "незачет")
                                             (ok "зачет")
                                             (t "незачет")))))
                            (concat date "\t" student "\t" module "\t" success "\t" module-name)))))


(defun my-skillbox//get-module-for-report (module)
  (pcase (my-skillbox//get-current-student-course)
    ('java (if (string-equal module "9.9") "Основы SQL" "Java 0 до PRO"))
    ('react "React.js")
    )
  )

(defun my-skillbox//parse-module-dir (modname)
  "1.1 Module name -> (java, 1.1)"
  (string-match "[0-9]+\\(\.[0-9]+\\)?" modname)
  `((course . ,(if (string-prefix-p "react" modname) 'java 'java))
    (module . ,(match-string 0 modname))))

(setq my-skillbox//module-edits-postfixes
      (append
       (list "")
       (cl-loop for i from 1 below 26 collect (concat "" (list (+ ?a i))))
       (cl-loop for i from 1 below 100 collect (concat "z" (format "%02d" i)))
       ))

(defun my-skillbox//get-prev-answer-file (student module)
  (seq-find #'file-exists-p
            (seq-map (lambda (postfix)
                       (let* ((module-directory (concat my-skillbox//base-dir "/java/" student "/" module postfix)))
                         (concat module-directory "/Answer" module ".org")
                         )
                       ) (reverse my-skillbox//module-edits-postfixes))))

;; (defun my-skillbox/open-in-idea ()
;;   "Open current project in IDEA"
;;   (interactive)
;;   )


(defun my-skillbox//get-student-root-by-student-name (student)
  (concat my-skillbox//base-dir "/java/" student "/"))


(defun my-skillbox//create-next-check-dir (student module-info)
  "Creates /student/module{,a,b,c}"
  (catch 'success
    (dolist (postfix my-skillbox//module-edits-postfixes)
      (let* ((course (alist-get 'course module-info))
             (module (alist-get 'module module-info))
             (module-directory (concat
                                my-skillbox//base-dir
                                "/" (symbol-name course)
                                "/" student
                                "/" module postfix)))
       (when (not (file-exists-p module-directory))
         (make-directory module-directory)
         (message "Directory %s created" module-directory)
         (throw 'success module-directory))))))


(defun my-skillbox//open-source-in-window-github (github-dir)
  (let* ((module (my-skillbox//get-current-module))
         (module-no-dot (replace-regexp-in-string "[.].+" "" module))
         (module-dir-cands (file-expand-wildcards (concat github-dir "/*" module-no-dot) ))
         )
    (if (eq 1 (length module-dir-cands))
        (dired-other-window (car module-dir-cands))
      (dired-other-window github-dir)
      )
    ))

(defun my-skillbox//open-source-in-window-archive  (module-dir)
  (let* ((module-dir-cands (f--files module-dir(equal (f-ext it) "java") t)))
    (when (= 1 (length module-dir-cands))
        (find-file-other-window (car module-dir-cands)))
    (when (> (length module-dir-cands) 1)
      (dired-other-window (file-name-directory (car module-dir-cands))))
    ))

(defun my-skillbox/open-source-in-window ()
  (interactive)
  (let* ((module-directory (my-skillbox//get-current-module-dir))
         (github-dir (concat module-directory "/../github")))
    (if (file-exists-p github-dir)
      (my-skillbox//open-source-in-window-github github-dir)
    (my-skillbox//open-source-in-window-archive module-directory))
    ))


(defun my-skillbox/new-check (student module-info)
  "Creates a new check"
  (interactive (list
                (completing-read "Student: " (my-skillbox//list-students))
                (my-skillbox//parse-module-dir
                 (completing-read "Module: " my-skillbox//modules))
                ))
  (let* ((course (alist-get 'course module-info))
         (module (alist-get 'module module-info))
         (module-directory (my-skillbox//create-next-check-dir student module-info))
         (archive-file (my-skillbox//find-last-archive student))
         (prev-answer-file (my-skillbox//get-prev-answer-file student module)) )
         (message "file: %s / archive: %s" (concat module-directory "/Answer" module ".org" ) archive-file)
         (find-file (concat module-directory "/Answer" module ".org" ))
         (if prev-answer-file
             (insert-file-contents prev-answer-file)
             (my-skillbox//auto-insert-module-template module))
         (when archive-file
           (my-skillbox//extract-homework-to-dir archive-file module-directory))
         (when (file-exists-p (concat module-directory "/../github"))
           (my-skillbox//git-pull))
         (my-skillbox/open-source-in-window)
  ))

(defun my-skillbox/extract-file-to-current-module ()
  "Extract archive to current module"
  (interactive)
  (let* ((module-directory (my-skillbox//get-current-module-dir))
         (student (my-skillbox//get-student-id-from-path (buffer-file-name)))
         (archive-file (my-skillbox//find-last-archive student)))
    (when archive-file
      (my-skillbox//extract-homework-to-dir archive-file module-directory))
    ))

(defun my-skillbox//find-last-archive (student)
  "Return latest archive (less then 12 h) file of student or nil"
  (let* ((student-id (car (split-string student "_")))
         (extensions `(,@my-skillbox//7z-extensions ,@my-skillbox//non-7z-extensions))
         (extensions-regex (mapconcat 'identity extensions "\\|"))
         (files-unsorted (directory-files-and-attributes (expand-file-name my-skillbox//downloads-dir) t (concat ".+\.\\(" extensions-regex "\\)$") t)))
    (caar
     (last
      (seq-filter (lambda (x)  (< (- (float-time (current-time))
                                   (float-time (nth 6 x))) (* 1 3 60)))
       (cl-sort files-unsorted
                #'time-less-p
                :key #'(lambda (x) (nth 6 x))))))))


(defun my-skillbox//extract-homework-to-dir (filename targetdir)
  "Unpacks archive to directory"
  (if (member (file-name-extension filename) my-skillbox//7z-extensions)
      (let* ((shell-cmd (concat
                         (shell-quote-argument my-skillbox//7z-path)
                         " x -o"
                         (shell-quote-argument (expand-file-name targetdir))
                         " "
                         (shell-quote-argument filename)))
             (default-directory (expand-file-name targetdir))
             (new-name (concat
                        targetdir
                        "/skillbox_dz."
                        (file-name-extension filename) )))
        (save-excursion
          (save-current-buffer
            (copy-file filename new-name t t)
             (message "unpack: %s -> %s" filename new-name)
             (message "%s x %s" my-skillbox//7z-path (file-name-nondirectory new-name))
            (start-process "*skillbox unpack*"
                           (get-buffer-create "*skillbox unpack*")
                           my-skillbox//7z-path
                           "x" (file-name-nondirectory new-name))

            (run-at-time "1 seconds" nil 'delete-file new-name)
            )))
    ;; (copy-file filename (file-name-as-directory (expand-file-name targetdir)))
    )
  (message "file: " (file-name-extension filename))

  (if (member (file-name-extension filename) my-skillbox//non-7z-extensions)
      (copy-file filename (file-name-as-directory (expand-file-name targetdir)))
    )
  )

(defun my-skillbox//list-students ()
  "Lists of students"
  (directory-files (concat my-skillbox//base-dir "/java") nil "[0-9]"))


(defun my-skillbox//string-exists-p (string)
  "Check is string exists"
  (save-excursion
    (goto-char (point-min))
    (search-forward string nil t)))


(defun my-skillbox//org-inline-css-hook (exporter)
  "Highlight source code with dark background"
  (when (eq exporter 'html)
    (let ((css-path (expand-file-name "~/OneDrive/dotfiles/spacemacs/layers/my-skillbox/theme/zenburn-code.css")))
        (setq
      org-html-head-extra
      (concat
       org-html-head-extra
       (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" />"
               css-path))))))


(defun my-skillbox//new-student (student-id course)
  "New skillbox student"
  (interactive (list
                (read-string "Name: ")
                (completing-read "Course: " my-skillbox//courses)
                ))
  ;; (interactive "sName: ")
  (let* ((student (replace-regexp-in-string "-[^ ]+ " "_" student-id))
         (full-dir (concat my-skillbox//base-dir "/java/" student "/")))
    (if (not (file-exists-p full-dir))
        (progn
          (make-directory full-dir)
          (message "Directory %s created" full-dir)
          )
      (message "Directory %s already exists" full-dir)
      )
    (write-region course nil (concat full-dir "course-" course ".txt"))

    (when (not (file-exists-p (concat full-dir "cv.org")))
      (let* ((line1 (format "course: %s\n" course))
                                      (line2 (format "full id: %s\n" student-id)))
                                  (find-file (concat full-dir "cv.org"))
                                  (insert line1)
                                  (insert line2)
                                  ))
    (write-region course nil (concat full-dir "course-" course ".txt"))
    (write-region course nil (concat full-dir "course.txt"))
    ))

(defun my-skillbox//get-course-by-student (student-name)
  "return course name by student name (123_Name SecondName)"
  (intern
   (let* ((module-root (my-skillbox//get-student-root-by-student-name student-name))
          (course-name-file (concat module-root "/course.txt")))
     (if (file-exists-p course-name-file)
         (with-temp-buffer
           (insert-file-contents course-name-file)
           (string-trim (buffer-string)))
       "java"
       ))))

(defun my-skillbox//clone-gitlab (git-link-url)
  (interactive "sUrl (https://gitlab.skillbox.): ")
  (let ((clone-url (my-skillbox//get-clone-url git-link-url))
        (git-dir (my-skillbox//get-git-dir-from-inside)))
    (make-directory git-dir t)
    (let ((default-directory git-dir))
      (magit-clone-internal clone-url "github" nil)
      )
    ))

(defun my-skillbox//open-github-folder ()
  (interactive)
  (find-file (concat (my-skillbox//get-git-dir-from-inside) "/github"))
  )

(defun my-skillbox//get-clone-url (raw-url)
  "Converts any github link to clone url"
  (cond ((eql (string-match "https://gitlab.skillbox.ru/\\([^/]+\\)" raw-url) 0)
          (let ((username (match-string 1 raw-url)))
               (concat "https://gitlab.skillbox.ru/" username "/java_basics.git")))

        ((eql (string-match "https://github.com/\\([^/]+/[^/]+\\)" raw-url) 0)
         (let ((project (match-string 1 raw-url)))
           (concat "https://github.com/" project ".git"))
         )
        (t throw (error "Bad repo url: %s" raw-url)))
  )

;; (require 'request)

;; (setq my-skillbox/spreadsheet-id "1ql-dw01Pg3LxJVVnGTW0Xg08fOFBUBNJ2Hjbenj50IA")

;; GET

;; https://docs.google.com/spreadsheets/d//edit?ts=5c51b279#gid=0

;; (request
;;  (format "https://sheets.googleapis.com/v4/spreadsheets/%s/values/Sheet1!A1:D5" my-skillbox/spreadsheet-id)
;;  :params '(("key" . "value") ("key2" . "value2"))
;;  :parser 'json-read
;;  :success (cl-function
;;            (lambda (&key data &allow-other-keys)
;;              (message "I sent: %S" (assoc-default 'args data)))))


(defun my-skillbox//insert-code-block ()
  (interactive)
  (end-of-line)
  (newline)
  ;; (message "%s" (my-skillbox//get-current-student-course))
  (cl-case (my-skillbox//get-current-student-course)
    ('react (insert "#+BEGIN_SRC typescript\n\n#+END_SRC"))
    (t (insert "#+BEGIN_SRC java\n\n#+END_SRC")))
  (forward-line -1)
  )
