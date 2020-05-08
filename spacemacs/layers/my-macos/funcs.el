;; http://www.alecjacobson.com/weblog/?p=3816 Linux x11: https://emacs.stackexchange.com/questions/41016/how-can-i-yank-images-from-emacs?noredirect=1#comment64407_41016

(defun ns-yank-image-at-point-as-image ()
    "Yank the image at point to the X11 clipboard as image/png."
    (interactive)
    (let ((image (get-text-property (point) 'display)))
      (if (eq (car image) 'image)
          (let ((data (plist-get (cdr image) ':data))
                (file (plist-get (cdr image) ':file)))
            (cond (data
                   (with-temp-buffer
                     (insert data)
                     (call-shell-region
                      (point-min) (point-max)
                      "/Users/ele/Dropbox/confiles/macos/bin/impbcopy")))
                  (file
                   (if (file-exists-p file)
                       (start-process
                        "_" nil "/Users/ele/Dropbox/confiles/macos/bin/impbcopy"  (file-truename file))))
                  (t
                   (message "The image seems to be malformed."))))
        (message "Point is not at an image."))))
