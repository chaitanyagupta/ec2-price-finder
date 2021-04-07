# EC2 Price Finder

[EC2 Price Finder][ec2-price-finder] helps you quickly find the cheapest EC2
instances for your needs. Also allows you to compare an instance's price across
different AWS regions in one go.

[ec2-price-finder]: https://ec2-price-finder.lisper.in

## Installation

Until the project is added to quicklisp, the easiest way to install it is to
clone the repo, create a symlink to the project directory inside
`~/quicklisp/local-projects/`, or add the project's directory to
`ASDF:*CENTRAL-REGISTRY*`, then run:

```cl
(ql:quickload "ec2-price-finder")
```

You will also need to download the bulk pricing CSV for EC2 from AWS. It is
available at the following URL (warning: 1.2 GB file):

https://pricing.us-east-1.amazonaws.com/offers/v1.0/aws/AmazonEC2/current/index.csv

## Usage

Load the previously downloaded pricing file in your Lisp image:

```cl
(ec2-price-finder:load-pricing-file "/path/to/index.csv")
```

After this, you can start the Hunchentoot server.

```cl
(ec2-price-finder:start-server 8080)
```

Now open http://localhost:8080 in the browser to find your instance!

After you are done, simply stop the server as follows:

```cl
(ec2-price-finder:stop-server)
```
