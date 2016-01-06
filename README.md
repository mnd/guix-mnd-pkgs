How to use repo
===============

To use this repo you can call guix like

    GUIX_PACKAGE_PATH="/path/to/pkgs" guix package -i php

or

    guix package -L "/path/to/pkgs" -i php

Current state of php package
----------------------------

Php package for now is quite usable. It can builds and pass php tests.
For now php compiles with fpm and with build-in pdo-pgsql.
This config selected because GuixSD for now have services for nginx and postgresql.

Next step is creating service for php-fpm and correct config for nginx.
