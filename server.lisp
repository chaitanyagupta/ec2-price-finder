(in-package #:ec2-price-finder)

(defparameter *cache-buster* 5)

(defparameter *default-region-codes*
  '("us-east-1" "eu-west-1" "ap-south-1"))

(defparameter *css-beautifier* "https://cdn.jsdelivr.net/npm/picnic")

(defun app-css ()
  (lass:compile-and-write
   '(html
     :box-sizing border-bod)
   '((:or * (:and * :before) (:and * :after))
     :box-sizing border-box)
   '(body
     :font-family "system-ui" "-apple-system" "Segoe UI" "Roboto" "Helvetica Neue" "Arial" "Noto Sans" "Liberation Sans" "sans-serif" "Apple Color Emoji" "Segoe UI Emoji" "Segoe UI Symbol" "Noto Color Emoji"
     :padding-left 1rem
     :padding-right 1rem)
   '(h1
     :font-size 1.5em)
   '((h1 + p)
     :margin-top 0)
   '(.no-top-padding
     :padding-top 0)
   '(.monospace
     :font-family "SFMono-Regular" "Menlo" "Monaco" "Consolas" "Liberation Mono" "Courier New" "monospace")
   '(.align-right
     :text-align right)
   '(.quantity
     :font-family "SFMono-Regular" "Menlo" "Monaco" "Consolas" "Liberation Mono" "Courier New" "monospace"
     :text-align right)
   '(.form-region-group
     :display grid
     :grid-template-columns "repeat(auto-fill, minmax(230px, 1fr))"
     :margin-bottom 0.5em)
   '(.form-region-group-name
     :font-weight bold)
   '(.cheapest
     :font-weight bold)
   '(.pseudo.toggle
     :color "#0074d9")
   '(.form-group
     :display flex
     :margin-top 1em)
   '((:and .form-group :first-child)
     :margin-top 0)
   '((.form-group :first-child)
     :width 33.33%)
   '((.form-group (:nth-child 2))
     :width 66.66%)
   '((.form-group label)
     :padding-top 4px)
   '(th
     :text-align center)
   '(footer
     :margin-top 1em)
   '((form details)
     :max-width 720px
     :border "2px groove rgb(240, 240, 240)"
     :margin-top 1em
     :padding 0.5em)
   '((form (:and details :first-child))
     :margin-top 0)
   '((details > summary)
     :font-size "1.1em"
     :font-weight bold
     :cursor pointer
     :display "list-item")
   '((details fieldset)
     :padding "0 1em 1em 0")
   '(.instance-type-button
     :background-color transparent
     :color "#0074d9"
     :margin 0
     :padding 0)))

(defun head-common ()
  (spinneret:with-html
    ;; Make sure we look good on mobile
    (:meta :name "viewport" :content "width=device-width,initial-scale=1,user-scalable=no")
    ;; Tell browsers not to look for a favicon
    (:link :rel "icon" :href "data:,")
    ;; Beautify our page
    (:link :rel "stylesheet" :href *css-beautifier*)
    ;; Common CSS
    (:style (:raw (app-css)))))

(defun not-found ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:title "Not Found")
      (head-common))
     (:body
      (:h1 "Not Found")
      (:p "The page you are requesting cannot be found. "
          (:a :href "/" "Please start again."))))))

(defun render-memory (memory)
  (cond ((null memory) nil)
        ((integerp memory) (format nil "~D" memory))
        (t (format nil "~$" memory))))

(defun render-find-form (&key
                           (document *document*)
                           (open-sections nil)
                           v-cpu
                           memory
                           operating-system
                           (region-codes *default-region-codes*)
                           instance-family
                           more-specs
                           price-per
                           autofocus)
  (spinneret:with-html
    (:form#find-form
     :method :get :action "/find"
     (:input :type "hidden" :name "c" :value *cache-buster*)
     (:input :type "hidden" :name "v" :value (version document))
     (:details
      :open (member 'specs open-sections)
      (:summary "Specs")
      (:fieldset
       (:div.form-group
        (:label :for "v-cpu" "Minimum vCPUs")
        (:input#v-cpu.two-third :name "v-cpu"
                                :type "text"
                                :value v-cpu
                                :autofocus autofocus
                                :placeholder "Optional, Integer"))
       (:div.form-group
        (:label :for "memory" "Minimum memory " (:small "(In GiB)"))
        (:input#memory :name "memory"
                       :type "text"
                       :value (render-memory memory)
                       :placeholder "Optional, Number"))
       (:div.form-group
        (:label :for "operating-system" "Operating System")
        (:select#operating-system :name "os"
                                  (loop for os in *aws-operating-systems*
                                        do (:option :value os
                                                    :selected (string= operating-system os)
                                                    os))))
       (:div.form-group
        (:label :for "instance-family" "Instance Family")
        (:select#instance-family :name "if"
                                 (:option :value "" "All")
                                 (loop
                                   for a-purpose in *aws-instance-purposes*
                                   for families = (gethash a-purpose (instance-family-groups document))
                                   do (:optgroup :label a-purpose
                                                 (:option :value a-purpose
                                                          :selected (string= instance-family a-purpose)
                                                          (format nil "All ~A" a-purpose))
                                                 (loop
                                                   for a-family in families
                                                   do (:option :value a-family
                                                               :selected (string= instance-family a-family)
                                                               a-family))))))))
     (:details
      :open (member 'regions open-sections)
      (:summary "Regions")
      (:fieldset
       (dolist (region-group *aws-region-groups*)
         (let ((regions (sort (remove-if-not (lambda (region)
                                               (aws-region-belongs-to-group region region-group))
                                             *aws-regions*)
                              #'string<
                              :key #'aws-region-short-name)))
           (:div (:span.form-region-group-name (aws-region-group-name region-group))
                 (:div.form-region-group
                  (loop for region in regions
                        for region-code = (aws-region-code region)
                        for input-id = (format nil "region-check-~A" region-code)
                        do (:div.form-region
                            (:input :type "checkbox"
                                    :id input-id
                                    :name "region"
                                    :value region-code
                                    :checked (member region-code region-codes
                                                     :test #'string=))
                            (:label.checkable :for input-id
                                              (aws-region-short-name region)
                                              " "
                                              (:small (format nil "(~A)" (aws-region-code region))))))))))))
     (:details
      :open (member 'display open-sections)
      (:summary "Display")
      (:fieldset
       (:div
        (:input :type "checkbox"
                :id "more-specs"
                :name "more-specs"
                :checked more-specs)
        (:label.checkable :for "more-specs"
                          "More specs (ECU, Storage, etc.)"))
       (:div.form-group
        (:label :for "price-per" "Price per")
        (:select#price-per :name "price-per"
                           (:option :value "hour" :selected (string= price-per "hour") "Hour")
                           (:option :value "month" :selected (string= price-per "month") "Month")))))
     (:p
      (:button :type "submit" "Find cheapest instances")))))

(defun render-instance-type-form (&key (document *document*))
  (spinneret:with-html
    (:form#instance-type-form
     :method :get
     (:input :type "hidden" :name "c" :value *cache-buster*)
     (:input :type "hidden" :name "v" :value (version document)))))

(defun footer ()
  (spinneret:with-html
    (:footer
     (:small "By " (:a :href "https://lisper.in" "Chaitanya Gupta"))
     (:small " · ")
     (:small (:a :href "https://github.com/chaitanyagupta/ec2-price-finder" "Code")))))

(easy-routes:defroute home ("/")
    ()
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:title "EC2 Price Finder")
      (head-common))
     (:body
      (:h1 (:a :href "/" "EC2 Price Finder"))
      (:p "Quickly find the cheapest EC2 instance for your needs")
      (render-find-form :open-sections '(specs) :autofocus t)
      (footer)))))

(defun parse-integer-with-junk (string)
  (parse-integer string :junk-allowed t))

(defun parse-float-with-junk (string)
  (parse-float:parse-float string :junk-allowed t))

(defun string-or-nil (string)
  (if (string= string "")
      nil
      string))

(easy-routes:defroute find-instances ("/find")
    ((v-cpu :parameter-type 'parse-integer-with-junk)
     (memory :parameter-type 'parse-memory-with-junk)
     (operating-system :real-name "os" :parameter-type 'string :init-form "Linux")
     (region-codes :real-name "region" :parameter-type '(list string))
     (instance-family :real-name "if" :parameter-type 'string-or-nil)
     (price-per :parameter-type 'string :init-form "hour")
     (more-specs :parameter-type 'boolean))
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:title "Find cheapest instances | EC2 Price Finder")
      (head-common))
     (:body
      (:h1 (:a :href "/" "EC2 Price Finder"))
      (render-find-form :open-sections '(display)
                        :v-cpu v-cpu
                        :memory memory
                        :operating-system operating-system
                        :region-codes region-codes
                        :instance-family instance-family
                        :price-per price-per
                        :more-specs more-specs)
      (render-instance-type-form)
      (let* ((regions (remove-if-not (lambda (region)
                                       (if region-codes
                                           (member (aws-region-code region) region-codes
                                                   :test #'string=)
                                           (member (aws-region-code region) *default-region-codes*
                                                   :test #'string=)))
                                     *aws-regions*))
             (results (find-cheapest :v-cpu (or v-cpu 0)
                                     :memory (or memory 0)
                                     :operating-system operating-system
                                     :region-codes (mapcar #'aws-region-code regions)
                                     :instance-family instance-family)))
        (if results
            (:div
             (:div
              (:small (:span "Last updated on ")
                      (:time :datetime (local-time:format-timestring nil (publication-date *document*))
                             (format nil "~A. "
                                     (local-time:format-rfc1123-timestring nil (publication-date *document*))))
                      (:span "Prices displayed are for " (:b "On-Demand") " instances only.")))
             (:table
              (:thead
               (:tr
                (:th :scope "col" "Instance Type")
                (:th :scope "col" "vCPU")
                (when more-specs
                  (:th :scope "col" "ECU"))
                (:th :scope "col" "Memory" (:br) "(GiB)")
                (when more-specs
                  (:th :scope "col" "Storage")
                  (:th :scope "col" "Network"))
                (dolist (region regions)
                  (:th :scope "col"
                       (aws-region-short-name region)
                       (:br)
                       (format nil "(~A)" (aws-region-code region))))))
              (:tbody
               (dolist (result results)
                 (let* ((def (car result))
                        (instance-type (instance-type def))
                        (cheapest (apply #'min (mapcar (lambda (price)
                                                         (if (string= price-per "month")
                                                             (* (hourly price) 730)
                                                             (hourly price)))
                                                       (cdr result)))))
                   (:tr
                    (:td (:button.instance-type-button
                          :form "instance-type-form"
                          :formaction (format nil "/instance-types/~A" instance-type)
                          instance-type))
                    (:td.quantity (v-cpu def))
                    (when more-specs
                      (:td (ecu def)))
                    (:td.quantity (format nil "~2$" (memory def)))
                    (when more-specs
                      (:td (storage def))
                      (:td (network-performance def)))
                    (dolist (region regions)
                      (let* ((price (find (aws-region-code region) (cdr result)
                                          :test #'string= :key #'region-code))
                             (amount (when price
                                       (if (string= price-per "month")
                                           (* (hourly price) 730)
                                           (hourly price)))))
                        (:td.quantity
                         :class (if (and price (= amount cheapest)) "cheapest" nil)
                         (if price (format nil "$~4$" amount) ""))))))))))
            (:div
             "Sorry, we could not find any results. Please modify your search and try again.")))
      (footer)))))

(defconstant +ec2-price-limit+ (expt 10 9)
  "Price of an EC2 instance won't go beyond this - this is needed for MIN
  comparisons.")

(easy-routes:defroute show-instance ("/instance-types/:instance-type")
    ()
  (let ((def (find-def-for-instance-type instance-type (defs *document*))))
    (if def
        (spinneret:with-html-string
          (:doctype)
          (:html
           (:head
            (:title (format nil "~A | EC2 Price Finder" instance-type))
            (head-common))
           (:body
            (:h1 (:a :href "/" "EC2 Price Finder"))
            (:h2.no-top-padding instance-type)
            (:h3 "Specs")
            (:table
             (:thead
              (:tr
               (:th :scope "col" "Property")
               (:th :scope "col" "Value")))
             (:tbody
              (:tr
               (:td "Purpose")
               (:td (instance-purpose def)))
              (:tr
               (:td "Instance Family")
               (:td (instance-family def)))
              (:tr
               (:td "vCPU")
               (:td (v-cpu def)))
              (:tr
               (:td "ECU")
               (:td (ecu def)))
              (:tr
               (:td "Memory (in GiB)")
               (:td (format nil "~2$" (memory def))))
              (:tr
               (:td "Storage")
               (:td (storage def)))
              (:tr
               (:td "Network")
               (:td (network-performance def)))
              (:tr
               (:td "CPU")
               (:td (physical-processor def)))))
            (:h3 "Prices")
            (let ((prices (remove-if-not (lambda (price)
                                           (string= (instance-type price) instance-type))
                                         (prices *document*)))
                  (cheapest-table (make-hash-table :test #'equal)))
              (dolist (price prices)
                (symbol-macrolet ((cheapest (gethash (operating-system price) cheapest-table +ec2-price-limit+)))
                  (setf cheapest (min cheapest (hourly price)))))
              (:table
               (:thead
                (:tr
                 (:th :scope "col" "Region")
                 (dolist (os *aws-operating-systems*)
                   (:th :scope "col" os))))
               (:tbody
                (dolist (region *aws-regions*)
                  (:tr
                   (:td (aws-region-short-name region)
                        " "
                        (:small (format nil "(~A)" (aws-region-code region))))
                   (dolist (os *aws-operating-systems*)
                     (let ((cheapest (gethash os cheapest-table +ec2-price-limit+))
                           (price (find-if (lambda (price)
                                             (and (string= (region-code price) (aws-region-code region))
                                                  (string= (operating-system price) os)))
                                           prices)))
                       (:td.quantity
                        :class (if (and price (= cheapest (hourly price))) "cheapest" nil)
                        (when price
                          (format nil "$~4$" (hourly price)))))))))))
            (footer))))
        (not-found))))
