How to use repo
===============

To use this repo you MUST call guix like

    GUIX_PACKAGE_PATH="/path/to/pkgs" guix package -i php

Another form of call

    guix package -L "/path/to/pkgs"-i php -n

Can't find patch files for now (28.12.2015).

How to run apache with php on GUIX SD
-------------------------------------

*WARNING*: this package must be rewritten in more GUIX-way and must not be used before.

To install php (and httpd as propagated-inputs) you must call

    GUIX_PACKAGE_PATH="/path/to/pkgs" guix package -i php

To run apache with httpd you must:

1. Copy `.guix-profile/etc/httpd/httpd.conf` file into writeable place.
2. Replace pathes to log files and pid files to writeable place.
3. Replace daemon Username and Group to appropriate one.
4. Replace port to one allowed for you user.
5. Enable php5 module in httpd.conf file:

    LoadModule php5_module modules/libphp5.so
    <FilesMatch ".+\.ph(p[345]?|t|tml)$">
        SetHandler application/x-httpd-php
    </FilesMatch>

6. Run apache with next command

    apachectl -d $HOME/.guix-profile/ -f /path/to/modified/httpd.conf
