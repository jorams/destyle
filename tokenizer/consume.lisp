(in-package :destyle/tokenizer)

;;; Basic machinery

(defvar *input* ""
  "A string containing the entire CSS input.")
(defvar *index* -1
  "The index of the current code point.")

(defun consume (&optional (amount 1))
  (if (>= (+ *index* amount) (length *input*))
      (and (setf *index* (length *input*)) nil)
      (char *input* (incf *index* amount))))

(defun peek (relative-index)
  (if (>= (+ *index* relative-index) (length *input*))
      nil
      (char *input* (+ *index* relative-index))))

(defun consume-while (predicate &optional max)
  (coerce (loop for count from 0
                while (or (null max)
                          (and max (< count max)))
                while (not (null (peek 1)))
                while (funcall predicate (peek 1))
                collect (consume))
          'string))

;;; Utilities

(defun make-adjustable-string ()
  (make-array 10 :element-type 'character
                 :adjustable t
                 :fill-pointer 0))

;; TODO: PARSE-NUMBER is dangerously ugly.
(defun parse-number (string)
  (bind (((:values i offset)
          (parse-integer string :junk-allowed t))
         (s (if (and (numberp i) (minusp i)) -1 1))
         (i (if i (abs i) 0))
         ((:values f d)
          (if (and (< offset (length string))
                   (char= #\. (char string offset)))
              (parse-integer string :junk-allowed t
                                    :start (1+ offset))
              (values 0 0)))
         ((:values f offset d)
          (values (or f 0)
                  (if (plusp d) d offset)
                  (if (plusp d)
                      (- d 1 offset)
                      0)))
         (e (or (and (< offset (length string))
                     (parse-integer string :junk-allowed t
                                           :start (1+ offset)))
                0))
         (u (if (minusp e) -1 1))
         (e (abs e)))
    (* s (+ i (* f (expt 10 (- d)))) (expt 10 (* u e)))))

(defun consume-name ()
  (bind ((string (make-adjustable-string))
         ((:flet add (char))
          (when (characterp char)
            (vector-push-extend (if (char= #\\ char)
                                    (consume-escaped-character)
                                    char)
                                string))))
    (loop for char = (consume)
          for next = (peek 1)
          while (and (characterp next)
                     (or (name-character-p next)
                         (valid-escape-p next (peek 2))))
          do (add char)
          finally (add char)
                  (return string))))


;;; Predicates

(defun whitespace-p (char)
  (member char '(#\newline #\tab #\space #\return)))

(defun hex-digit-p (char)
  (digit-char-p char 16))

(defun name-start-character-p (char)
  (and (characterp char)
       (or (alpha-char-p char)
           (char= #\_ char)
           (<= 128 (char-code char)))))

(defun name-character-p (char)
  (or (name-start-character-p char)
      (digit-char-p char)
      (char= #\- char)))

(defun valid-escape-p (char1 char2)
  (and (char= #\\ char1)
       (char/= #\newline char2)))

(defun non-graphic-character-p (char)
  (let ((code (char-code char)))
    (or (< #x0 code #x8)
        (= #xb code)
        (< #xe code #x1f)
        (= #x7f code))))

(defun number-start-p (char1 char2 char3)
  (case char1
    ((#\+ #\-)
     (or (digit-char-p char2)
         (and (char= #\. char2)
              (digit-char-p char3))))
    (#\. (digit-char-p char2))
    (t (digit-char-p char1))))

(defun can-consume-number-p ()
  (number-start-p (peek 1) (peek 2) (peek 3)))

(defun identifier-start-p (char1 char2 char3)
  (case char1
    (#\- (or (name-start-character-p char2)
             (valid-escape-p char2 char3)))
    (#\\ (valid-escape-p char1 char2))
    (t (name-start-character-p char1))))

(defun can-consume-identifier-p ()
  (identifier-start-p (peek 1) (peek 2) (peek 3)))


;;; Consumers

(defun consume-hex-number ()
  (let ((string (consume-while #'hex-digit-p 6)))
    (when (whitespace-p (peek 1)) (consume))
    (parse-integer string :radix 16)))

(defun consume-escaped-character ()
  (let ((char (peek 1)))
    (if (hex-digit-p char)
        (code-char (consume-hex-number))
        (consume))))

(defun consume-comment ()
  (when (and (char= #\/ (peek 1))
             (char= #\* (peek 2)))
    (make-instance '<comment-token>
                   :value (consume-while
                           (let ((expecting-/ nil)
                                 (done nil))
                             (lambda (char)
                               (unless done
                                 (prog1 t
                                   (case char
                                     (#\* (setf expecting-/ t))
                                     (#\/ (and expecting-/
                                               (setf done t)))
                                     (t (setf expecting-/ nil)))))))))))

(defun consume-delim ()
  (make-instance '<delim-token> :value (consume)))

(defun consume-string (end-char)
  (loop with string = (make-array 1 :element-type 'character
                                    :adjustable t
                                    :fill-pointer 0)
        for char = (consume)
        for next = (peek 1)
        while (and (char/= char end-char)
                   (not (null char)))
        do (if (char= next #\newline)
               (return (make-instance '<bad-string-token>))
               (vector-push-extend (if (valid-escape-p char next)
                                       (consume-escaped-character)
                                       char)
                                   string))
        finally (return (make-instance '<string-token>
                                       :value string))))

(defun consume-cdo ()
  (and (char= #\< (peek 1))
       (char= #\! (peek 2))
       (char= #\- (peek 3))
       (char= #\- (peek 4))
       (consume 4)
       (make-instance '<cdo-token>)))

(defun consume-cdc ()
  (and (char= #\- (peek 1))
       (char= #\- (peek 2))
       (char= #\> (peek 3))
       (consume 3)
       (make-instance '<cdc-token>)))

;;; Numbers

(defun consume-number ()
  (bind ((string (make-adjustable-string))
         (type :integer)
         ;; This is a bit ugly, but STRING should stay adjustable
         ((:flet consume-digits ())
          (loop for char across (consume-while #'digit-char-p)
                do (vector-push-extend char string))))
    (when (member (peek 1) '(#\+ #\-))
      (vector-push-extend (consume) string))
    (consume-digits)
    ;; Check if it's a floating point number
    (when (and (characterp (peek 1))
               (characterp (peek 2))
               (char= #\. (peek 1))
               (digit-char-p (peek 2)))
      (setf type :number)
      (vector-push-extend (consume) string)
      (consume-digits))
    ;; Check if it's a number of the form ##e##
    (when (and (characterp (peek 1))
               (characterp (peek 2))
               (char-equal #\e (peek 1))
               (or (digit-char-p (peek 2))
                   (and (characterp (peek 3))
                        (digit-char-p (peek 3))
                        (member (peek 2) '(#\- #\+)))))
      (vector-push-extend (consume) string)
      (vector-push-extend (consume) string)
      (consume-digits))
    (values string (parse-number string) type)))

(defun consume-numeric ()
  (when (can-consume-number-p)
    (multiple-value-bind (repr number type)
        (consume-number)
      (cond
        ((can-consume-identifier-p)
         (make-instance '<dimension-token>
                        :value number
                        :number-type type
                        :representation repr
                        :unit (consume-name)))
        ((and (characterp (peek 1))
              (char= #\% (peek 1)))
         (consume)
         (make-instance '<percentage-token>
                        :value number
                        :representation repr))
        (t (make-instance '<number-token>
                          :value number
                          :number-type type
                          :representation repr))))))

;;; URLs

(defun consume-url-remnants ()
  (loop for char = (consume)
        while (char/= char #\))
        do (when (valid-escape-p char (peek 1))
             (consume-escaped-character)))
  (make-instance '<bad-url-token>))

(defun consume-string-url ()
  (let ((string-token (consume-string (consume))))
    (cond
      ((typep string-token '<bad-string-token>)
       (consume-url-remnants))
      (t
       (consume-while #'whitespace-p)
       (if (char= #\) (peek 1))
           (progn (consume) (make-instance '<url-token>
                                           :value (value string-token)))
           (consume-url-remnants))))))

(defun consume-bare-url ()
  (loop with string = (make-array 1 :element-type 'character
                                    :adjustable t
                                    :fill-pointer 0)
        for char = (consume)
        do (cond
             ((or (char= #\) char) (null char))
              (return (make-instance '<url-token> :value string)))
             ((whitespace-p char)
              (consume-while #'whitespace-p)
              (if (char= #\) (peek 1))
                  (progn (consume)
                         (return (make-instance '<url-token>
                                                :value string)))
                  (return (consume-url-remnants))))
             ((or (member char '(#\" #\' #\())
                  (non-graphic-character-p char))
              (return (consume-url-remnants)))
             ((char= #\\ char) (if (valid-escape-p char (peek 1))
                                   (vector-push-extend (consume-escaped-character)
                                                       string)
                                   (return (consume-url-remnants))))
             (t (vector-push-extend char string)))))


(defun consume-url ()
  (consume-while #'whitespace-p)
  (if (member (peek 1) '(#\" #\'))
      (consume-string-url)
      (consume-bare-url)))

;;; Names

(defun consume-ident-like ()
  (let ((name (consume-name)))
    (cond
      ((and (string-equal "url" name)
            (characterp (peek 1))
            (char= #\( (peek 1)))
       (consume)
       (consume-url))
      ((and (characterp (peek 1))
            (char= #\( (peek 1)))
       (make-instance '<function-token> :value name))
      (t
       (make-instance '<ident-token> :value name)))))

(defun consume-at-keyword ()
  (consume)
  (if (can-consume-identifier-p)
      (make-instance '<at-keyword-token>
                     :value (consume-name))
      (make-instance '<delim-token> :value #\@)))

(defun consume-hash ()
  (consume)
  (if (and (characterp (peek 1))
           (or (name-character-p (peek 1))
               (and (characterp (peek 2))
                    (valid-escape-p (peek 1) (peek 2)))))
      (let ((type
              (if (and (characterp (peek 3))
                       (identifier-start-p (peek 1) (peek 2) (peek 3)))
                  :id :unrestricted)))
        (make-instance '<hash-token>
                       :hash-type type
                       :value (consume-name)))))

;;; Unicode ranges

(defun valid-unicode-range-character-p (char)
  (or (digit-char-p char 16)
      (char= char #\?)))

(defun consume-unicode-range-element (can-contain-?-p)
  (flet ((make-hex-consumer (amount &optional to-consume)
           (lambda (char) (and (or (and to-consume (char= char to-consume))
                                   (digit-char-p char 16))
                               (plusp amount)
                               (decf amount)))))
    (if (not can-contain-?-p)
        (parse-integer (consume-while (make-hex-consumer 6)) :radix 16)
        (let* ((token-start (consume-while (make-hex-consumer 6)))
               (token-end (consume-while
                           (make-hex-consumer (- 6 (length token-start))
                                              #\?)))
               (token (concatenate 'string token-start token-end)))
          (values
           (parse-integer (substitute #\0 #\? token) :radix 16)
           (and (/= 0 (length token-end))
                (parse-integer (substitute #\F #\? token) :radix 16)))))))

(defun consume-unicode-range ()
  (and (or (char= #\u (peek 1))
           (char= #\U (peek 1)))
       (char= #\+ (peek 2))
       (valid-unicode-range-character-p (peek 3))
       (consume 2)
       (multiple-value-bind (start end)
           (consume-unicode-range-element t)
         (make-instance '<unicode-range-token>
                        :start start
                        :end (or end
                                 (and (characterp (peek 1))
                                      (char= #\- (peek 1))
                                      (consume)
                                      (consume-unicode-range-element nil))
                                 start)))))
