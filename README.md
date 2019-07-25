### Automatic package installer
This code (which will become a package) automatically scans a .R file for functions, and installs any packages required to use those functions. If a function could belong to multiple packages it asks the user for guidance. Currently only works for CRAN packages. 

# Requires:
Collidr library (will be added as dependancy to package)

