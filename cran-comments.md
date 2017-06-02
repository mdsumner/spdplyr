## Test environments

* local Ubuntu install, R 3.4.0
* ubuntu 12.04 (on travis-ci), R 3.4.0
* win-builder (devel)

## R CMD check results

0 errors | 1 warnings | 0 notes

winbuilder devel gives me this warning, but other systems do not: 

Conversion of 'README.md' failed:
pandoc.exe: Could not fetch https://img.shields.io/codecov/c/github/mdsumner/spdplyr/master.svg
TlsExceptionHostPort (HandshakeFailed Error_EOF) "img.shields.io" 443