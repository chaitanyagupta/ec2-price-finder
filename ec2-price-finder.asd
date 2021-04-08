(asdf:defsystem #:ec2-price-finder
  :description "Quickly find the cheapest EC2 instance that you need across regions"
  :author "Chaitanya Gupta <mail@chaitanyagupta.com>"
  :license "BSD-3-Clause"
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
