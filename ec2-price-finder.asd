(asdf:defsystem #:ec2-price-finder
  :depends-on (:read-csv
               :parse-float
               :hunchentoot
               :spinneret
               :easy-routes
               :lass
               :local-time
               :wu-decimal)
  :serial t
  :components ((:file "package")
               (:file "loader")
               (:file "server")
               (:file "main")))
