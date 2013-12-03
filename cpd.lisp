;;;; cl-cpd.lisp

(in-package #:cpd)

(defparameter +attr-regex+ "^(.+) = (.+)$")

(defun read-channel-data (line)
  (with-input-from-string (s (format nil "#(~a)" line))
    (read s nil)))

(defmacro parse-clip-line (line)
  `(ppcre:register-groups-bind (attr value) (+attr-regex+ ,line)
     (optima:match attr
       ("rate" (list :rate (parse-integer value)))
       ("start" (list :start (parse-integer value)))
       ("tracklength" (list :track-length (parse-integer value)))
       ("tracks" (list :track-count (parse-integer value)))
       (otherwise ()))))

(defmacro parse-extend-region-type (value)
  `(optima:match ,value
     ("hold" :hold)
     ("slope" :slope)
     ("cycle" :cycle)
     ("mirror" :mirror)
     ("default" :default)))

(defmacro extend-region-type-name (region-type)
  `(optima:match ,region-type
     (:hold "hold")
     (:slope "slope")
     (:cycle "cycle")
     (:mirror "mirror")
     (:default "default")))

(defmacro parse-track-line (line)
  `(ppcre:register-groups-bind (attr value) (+attr-regex+ ,line)
     (optima:match attr
       ("name" (list :name value))
       ("data" (list :data (read-channel-data value)))
       ("lefttype" (list :left-type (parse-extend-region-type value)))
       ("righttype" (list :right-type (parse-extend-region-type value)))
       ("default" (list :default (with-input-from-string (s value) (read s nil))))
       (otherwise ()))))

(defun read-track (lines)
  (labels ((next-line (lines result)
             (let ((line (first lines)))
               (optima:match line
                 ("}" (list (rest lines) result))
                 (otherwise
                  (next-line (rest lines)
                             (append result (parse-track-line line))))))))
    (next-line lines ())))

(defun read-clip (lines)
  (labels ((next-line (lines result)
             (if lines
                 (let ((line (first lines)))
                   (optima:match line
                     ("{" (let ((track-result (read-track (rest lines))))
                            (push (second track-result) (getf result :tracks))
                            (next-line (first track-result) result)))
                     (otherwise
                      (next-line (rest lines)
                                 (append result (parse-clip-line line))))))
                 result)))
    (next-line lines '(:tracks ()))))

(defun from-file (filename)
  (with-open-file (in filename :direction :input)
    ;; Valid clip files surround their entire contents with { and } on their own line
    (unless (string= "{" (read-line in nil))
      (error (format nil "~a is not a valid clip file." filename)))
    
    (read-clip
     (loop for line = (read-line in nil)
        until (string= "}" line) ;; Signals the end of the clip
        collect (if line (string-trim '(#\space) line)
                    (error "Unexpected end of file!"))))))

(defun to-file (clip filename &key (padding "  "))
  (labels ((write-track (track out)
             (let ((name (getf track :name))
                   (data (getf track :data))
                   (left-type (getf track :left-type))
                   (right-type (getf track :right-type))
                   (default (getf track :default)))
               (format out "~a{~%" padding)
               (format out "~a~aname = ~a~%" padding padding name)
               (format out "~a~adata = " padding padding)
               (loop for i across data
                  do (format out "~F " i))
               (format out "~%")
               (when left-type
                 (format out "~a~alefttype = ~a~%" padding padding (extend-region-type-name left-type)))
               (when right-type
                 (format out "~a~arighttype = ~a~%" padding padding (extend-region-type-name right-type)))
               (when default
                 (format out "~a~adefault = ~a~%" padding padding default))
               (format out "~a}~%" padding))))
    (let ((rate (format nil "rate = ~a" (getf clip :rate)))
          (start (format nil "start = ~a" (getf clip :start)))
          (track-length (format nil "tracklength = ~a" (getf clip :track-length)))
          (track-count (format nil "tracks = ~a" (getf clip :track-count))))
      (with-open-file (out filename :direction :output)
        (format out "{~%")
        (loop for item in (list rate start track-length track-count)
           do (format out "~a~a~%" padding item))
        (loop for track in (getf clip :tracks)
           do (write-track track out))
        (format out "}")))))
