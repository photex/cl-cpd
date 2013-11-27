;;;; cl-cpd.lisp

(in-package #:cpd)

(defun read-channel-data (line)
  (let ((s (make-string-input-stream line)))
    (unwind-protect
         (loop for n = (read s nil)
            while n collect n)
      (close s))))

(defun from-lines (lines)
  (let ((rate 0)
        (start 0)
        (track-length 0)
        (track-count 0))
    (loop for line in lines
       do (optima:match line
            ("{" ()) ;; start track
            ("}" ()) ;; end track
            (otherwise
             (ppcre:register-groups-bind (attr value) ("^(.+) = (.+)$" line)
               (optima:match attr
                 ("rate" (setf rate (parse-integer value)))
                 ("start" (setf start (parse-integer value)))
                 ("tracklength" (setf track-length (parse-integer value)))
                 ("tracks" (setf track-count (parse-integer value)))
                 ("name" nil)
                 ("data" nil))))))
    (list :rate rate :start start :track-length track-length :track-count track-count)))

(defun from-file (filename)
  (with-open-file (in filename
                      :direction :input)
    ;; Valid clip files surround their entire contents with { and } on their own line
    (unless (string= "{" (read-line in nil))
      (error (format nil "~a is not a valid clip file." filename)))
    
    (loop for line = (read-line in nil)
       until (string= "}" line) ;; Signals the end of the clip
       collect (if line (string-trim '(#\space) line)
                   (error "Unexpected end of file!")))))

(defun to-file () nil)
