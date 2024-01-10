(setq my-common//idea-path "idea64")

(defadvice dired-readin
    (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (lsn/mydired-sort))
