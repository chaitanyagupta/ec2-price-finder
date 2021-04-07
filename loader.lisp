(in-package #:ec2-price-finder)

(defparameter *aws-regions*
  '(("af-south-1" "Africa (Cape Town)")
    ("ap-east-1" "Asia Pacific (Hong Kong)")
    ("ap-northeast-1" "Asia Pacific (Tokyo)")
    ("ap-northeast-2" "Asia Pacific (Seoul)")
    ("ap-northeast-3" "Asia Pacific (Osaka)")
    ("ap-south-1" "Asia Pacific (Mumbai)")
    ("ap-southeast-1" "Asia Pacific (Singapore)")
    ("ap-southeast-2" "Asia Pacific (Sydney)")
    ("ca-central-1" "Canada (Central)")
    ("eu-central-1" "Europe (Frankfurt)" "EU (Frankfurt)")
    ("eu-north-1" "Europe (Stockholm)" "EU (Stockholm)")
    ("eu-south-1" "Europe (Milan)" "EU (Milan)")
    ("eu-west-1" "Europe (Ireland)" "EU (Ireland)")
    ("eu-west-2" "Europe (London)" "EU (London)")
    ("eu-west-3" "Europe (Paris)" "EU (Paris)")
    ("me-south-1" "Middle East (Bahrain)")
    ("sa-east-1" "South America (São Paulo)" "South America (Sao Paulo)")
    ("us-east-1" "US East (N. Virginia)")
    ("us-east-2" "US East (Ohio)")
    ("us-west-1" "US West (N. California)")
    ("us-west-2" "US West (Oregon)")
    ("us-west-2-lax" "US West (Los Angeles)")))

(defun find-aws-region (code)
  (assoc code *aws-regions* :test #'string=))

(defun aws-region-code (region)
  (car region))

(defun aws-region-names (region)
  (cdr region))

(defun aws-region-name (region)
  (first (aws-region-names region)))

(defun aws-region-short-name (region)
  (let ((name (aws-region-name region)))
    (subseq name (1+ (position #\( name)) (position #\) name))))

(defun aws-region-code-to-name (code)
  (first (aws-region-names (assoc code *aws-regions* :test #'string=))))

(defun aws-region-name-to-code (name)
  (let ((row (find-if (lambda (row) (member name (cdr row) :test #'string=)) *aws-regions*)))
    (assert (not (null row)))
    (car row)))

(defparameter *aws-region-groups*
  '(("af" "Africa")
    ("ap" "Asia Pacific")
    ("ca" "Canada")
    ("eu" "Europe")
    ("me" "Middle East")
    ("sa" "South America")
    ("us-east" "US East")
    ("us-west" "US West")))

(defun find-aws-region-group (prefix)
  (assoc prefix *aws-region-groups* :test #'string=))

(defun aws-region-group-name (region-group)
  (second region-group))

(defun aws-region-group-prefix (region-group)
  (first region-group))

(defun aws-region-belongs-to-group (region group)
  (= (mismatch (aws-region-group-prefix group)
               (aws-region-code region)
               :test #'string=)
     (length (aws-region-group-prefix group))))

(defparameter *aws-operating-systems*
  '("Linux" "SUSE" "RHEL" "Windows"))

(defparameter *aws-instance-purposes*
  '("General purpose" "Compute optimized" "Memory optimized" "Storage optimized"
    "GPU instance" "Machine Learning ASIC Instances" "FPGA Instances"))

(defun lispify-header-name (string &optional package)
  (let* ((joiner #\-)
         (name (with-output-to-string (str)
                 (loop
                   with previous-in-char = nil
                   with previous-out-char = nil
                   for char across string
                   do
                      (cond ((not (alphanumericp char))
                             (when (not (eql previous-out-char joiner))
                               (write-char joiner str)
                               (setf previous-out-char joiner)))
                            ((upper-case-p char)
                             (when (and (not (null previous-out-char))
                                        (not (eql previous-out-char joiner))
                                        (not (upper-case-p previous-in-char)))
                               (write-char joiner str))
                             (write-char char str)
                             (setf previous-out-char char))
                            (t (write-char (char-upcase char) str)
                               (setf previous-out-char char)))
                      (setf previous-in-char char)))))
    (if package
        (intern name package)
        (intern name :keyword))))

(defun parse-memory (string)
  (wu-decimal:parse-decimal string :end (position #\Space string)))

(defun parse-memory-with-junk (string)
  (handler-case (parse-memory string)
    (parse-error () nil)))

(defun family-for-instance-type (instance-type)
  (subseq instance-type 0 (position #\. instance-type)))

(defclass instance-def ()
  ((instance-type :initarg :instance-type :reader instance-type)
   (instance-family :reader instance-family)
   (purpose :initarg :purpose :reader instance-purpose)
   (v-cpu :initarg :v-cpu :reader v-cpu)
   (ecu :initarg :ecu :reader ecu)
   (memory :initarg :memory :reader memory)
   (storage :initarg :storage :reader storage)
   (network-performance :initarg :network-performance :reader network-performance)
   (physical-processor :initarg :physical-processor :reader physical-processor)))

(defmethod initialize-instance :after ((object instance-def) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value object 'instance-family) (family-for-instance-type (instance-type object))))

(defmethod print-object ((object instance-def) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A: ~A vCPU, ~A GiB"
            (instance-type object)
            (v-cpu object)
            (memory object))))

(defun find-def-for-instance-type (instance-type defs)
  (find instance-type defs :test #'string= :key #'instance-type))

(defclass instance-price ()
  ((instance-type :initarg :instance-type :reader instance-type)
   (region-code :initarg :region-code :reader region-code)
   (operating-system :initarg :operating-system :reader operating-system)
   (hourly :initarg :hourly :reader hourly)))

(defmethod print-object ((object instance-price) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A: ~A ~A ~4$"
            (instance-type object)
            (region-code object)
            (operating-system object)
            (hourly object))))

(defclass document ()
  ((publication-date :initarg :publication-date :reader publication-date)
   (version :initarg :version :reader version)
   (defs :initarg :defs :reader defs)
   (prices :initarg :prices :reader prices)
   (instance-family-groups :initarg :instance-family-groups :reader instance-family-groups)))

(defmethod print-object ((object document) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "Version ~A, ~A defs, ~A prices"
            (version object)
            (length (defs object))
            (length (prices object)))))

(defvar *document*)

(defun find-cheapest (&key
                        (document *document*)
                        (v-cpu 0)
                        (memory 0)
                        (operating-system "Linux")
                        region-codes
                        instance-family)
  (let ((defs (remove-if-not (lambda (def)
                               (and (>= (v-cpu def) v-cpu)
                                    (>= (memory def) memory)
                                    (if instance-family
                                        (if (find #\Space instance-family)
                                            (string= (instance-purpose def) instance-family)
                                            (string= (instance-family def) instance-family))
                                        t)))
                             (defs document)))
        (prices (remove-if-not (lambda (price)
                                 (and (string= (operating-system price) operating-system)
                                      (if region-codes
                                          (member (region-code price) region-codes :test #'string=)
                                          t)))
                               (prices document)))
        (defs-with-prices nil))
    (dolist (price prices)
      (let* ((instance-type (instance-type price))
             (def-with-prices (find instance-type defs-with-prices
                                    :key (lambda (def-with-prices)
                                           (instance-type (car def-with-prices)))
                                    :test #'string=)))
        (if def-with-prices
            (push price (cdr def-with-prices))
            (let ((def (find-def-for-instance-type instance-type defs)))
              (when def
                (setf def-with-prices (list def price))
                (push def-with-prices defs-with-prices))))))
    (setf defs-with-prices (nreverse defs-with-prices))
    defs-with-prices))

(defun process-row (row field-names defs)
  (let (alist)
    (flet ((lookup (field-name)
             (cdr (assoc field-name alist))))
      (loop
        for field-name in field-names
        for value in row
        do (push (cons field-name value) alist))
      (when (and (string= (lookup 'product-family) "Compute Instance")
                 (string= (lookup 'capacity-status) "Used")
                 (string= (lookup 'tenancy) "Shared")
                 (string= (lookup 'location-type) "AWS Region")
                 (string= (lookup 'term-type) "OnDemand")
                 (string= (lookup 'pre-installed-s-w) "NA")
                 (string= (lookup 'current-generation) "Yes")
                 (not (string= (lookup 'license-model) "Bring your own license"))
                 (not (search "GovCloud" (lookup 'location)))
                 (not (search "Verizon" (lookup 'location))))
        (list (unless (find-def-for-instance-type (lookup 'instance-type) defs)
                ;; return def only if newly created
                (make-instance 'instance-def
                               :instance-type (lookup 'instance-type)
                               :purpose (lookup 'instance-family)
                               :v-cpu (parse-integer (lookup 'v-cpu))
                               :ecu (lookup 'ecu)
                               :memory (parse-memory (lookup 'memory))
                               :storage (lookup 'storage)
                               :network-performance (lookup 'network-performance)
                               :physical-processor (lookup 'physical-processor)))
              (make-instance 'instance-price
                             :instance-type (lookup 'instance-type)
                             :region-code (aws-region-name-to-code (lookup 'location))
                             :operating-system (lookup 'operating-system)
                             :hourly (wu-decimal:parse-decimal (lookup 'price-per-unit))))))))

(defun instance-family-groups-from-defs (defs)
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (def defs)
      (pushnew (instance-family def) (gethash (instance-purpose def) groups)
               :test #'string=))
    (maphash (lambda (purpose instance-families)
               (setf (gethash purpose groups) (sort instance-families #'string<)))
             groups)
    groups))

(defun read-pricing-file (path &key progress-fn)
  (let (publication-date
        version
        defs
        prices)
    (with-open-file (in path)
      ;; format version
      (let ((row (read-csv:read-csv in #\, nil nil)))
        (assert (string= (first row) "FormatVersion"))
        (assert (string= (second row) "v1.0")))
      ;; disclaimer
      (read-line in)
      ;; publication date
      (let ((row (read-csv:read-csv in #\, nil nil)))
        (assert (string= (first row) "Publication Date"))
        (setf publication-date (local-time:parse-timestring (second row))))
      ;; version
      (let ((row (read-csv:read-csv in #\, nil nil)))
        (assert (string= (first row) "Version"))
        (setf version (second row)))
      ;; offer code
      (read-line in)
      ;; actual csv data starts here
      (let ((field-names (mapcar (lambda (name)
                                   (lispify-header-name name #.*package*))
                                 (read-csv:read-csv in #\, nil nil))))
        (loop
          for row = (read-csv:read-csv in #\, nil nil)
          for count upfrom 1
          while row
          do (with-simple-restart (next-row "Continue with next row")
               (destructuring-bind (&optional def price)
                   (process-row row field-names defs)
                 (when def
                   (push def defs))
                 (when price
                   (push price prices))))
             (when progress-fn
               (funcall progress-fn :count count))
          finally (funcall progress-fn :count count :done t)))
      (setf prices (sort prices
                         (lambda (price1 price2)
                           (or (string< (operating-system price1) (operating-system price2))
                               (and (string= (operating-system price1) (operating-system price2))
                                    (< (hourly price1) (hourly price2)))))))
      ;; return the results
      (make-instance 'document
                     :publication-date publication-date
                     :version version
                     :defs defs
                     :prices prices
                     :instance-family-groups (instance-family-groups-from-defs defs)))))
