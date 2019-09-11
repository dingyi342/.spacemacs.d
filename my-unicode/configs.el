(put 'eof-error 'error-conditions '(error eof-error))
(put 'eof-error 'error-message "End of stream")
(put 'bad-byte 'error-conditions '(error bad-byte))
(put 'bad-byte 'error-message "Not a UTF-8 byte")

(defclass stream ()
  ((bytes :initarg :bytes :accessor bytes-of)
   (position :initform 0 :accessor position-of)))

(defun logbitp (byte bit) (not (zerop (logand byte (ash 1 bit)))))

(defmethod read-byte ((this stream) &optional eof-error eof)
  (with-slots (bytes position) this
    (if (< position (length bytes))
        (prog1 (aref bytes position) (incf position))
      (if eof-error (signal eof-error (list position)) eof))))

(defmethod unread-byte ((this stream))
  (when (> (position-of this) 0) (decf (position-of this))))

(defun read-utf8-char (stream)
  (let ((byte (read-byte stream 'eof-error)))
    (if (not (logbitp byte 7)) byte
      (let ((numbytes
             (cond
              ((not (logbitp byte 5))
               (setf byte (logand #2r11111 byte)) 1)
              ((not (logbitp byte 4))
               (setf byte (logand #2r1111 byte)) 2)
              ((not (logbitp byte 3))
               (setf byte (logand #2r111 byte)) 3))))
        (dotimes (b numbytes byte)
          (let ((next-byte (read-byte stream 'eof-error)))
            (if (and (logbitp next-byte 7) (not (logbitp next-byte 6)))
                (setf byte (logior (ash byte 6) (logand next-byte #2r111111)))
              (signal 'bad-byte (list next-byte)))))
        (signal 'bad-byte (list byte))))))

(defun load-corrupt-file (file)
  (interactive "fFile to load: ")
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-literally file)
    (with-output-to-string
      (set-buffer-multibyte t)
      (loop with stream = (make-instance 'stream :bytes (buffer-string))
            for next-char =
            (condition-case err
                (read-utf8-char stream)
              (bad-byte (message "Fix this byte %s" (cdr err)))
              (eof-error nil))
            while next-char
            do (write-char next-char)))))
