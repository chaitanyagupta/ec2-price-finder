;;; Loads the pricing file and dumps an SBCL image to make startups faster

(push *default-pathname-defaults* asdf:*central-registry*)

(ql:quickload "ec2-price-finder")

(ec2-price-finder:load-pricing-file (or (first (uiop:command-line-arguments)) "index.csv"))

(ec2-price-finder:build-image)
