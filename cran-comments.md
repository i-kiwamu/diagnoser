## Resubmission

This is a resubmission after fixing errors.

## Major comment

This is the first registry to CRAN.

## Test environments

### Local

x86_64-apple-darwin20 (macOS Ventura 13.5, Intel Core i5)

R 4.3.1 compiled by Apple clang version 14.0.3 and GNU Fortran (GCC) 12.2.0

No ERRORs, WARNINGs, and NOTEs.

### Remote

- win-builder
  * check_win_devel(): No ERRORs and WARNINGs
    There are 3 NOTEs: these are not related to my package.
    ```
    * checking CRAN incoming feasibility ... NOTE
      Maintainer: 'Kiwamu Ishikura <ishikura.kiwamu@gmail.com>'

      New submission
    ```
    ```
    * checking for non-standard things in the check directory ... NOTE
    Found the following files/directories:
      ''NULL''
    ```
    ```
    * checking for detritus in the temp directory ... NOTE
    Found the following files/directories:
      'lastMiKTeXException'
    ```
  * check_win_release(): No ERRORs and WARNINGs
    There is 1 NOTE: this is not related to my package.
    ```
    * checking CRAN incoming feasibility ... NOTE
      Maintainer: 'Kiwamu Ishikura <ishikura.kiwamu@gmail.com>'

      New submission
    ```
- check_rhub()
  * Windows: No ERRORs and WARNINGs
    There are 3 NOTEs: these are not related to my package.
    ```
    * checking CRAN incoming feasibility ... NOTE
      Maintainer: 'Kiwamu Ishikura <ishikura.kiwamu@gmail.com>'

      New submission
    ```
    ```
    * checking for non-standard things in the check directory ... NOTE
    Found the following files/directories:
      ''NULL''
    ```
    ```
    * checking for detritus in the temp directory ... NOTE
    Found the following files/directories:
      'lastMiKTeXException'
    ```
  * Ubuntu: Success
  * Fedora: No ERRORs and WARNINGs
    There are 3 NOTEs: these are not related to my package.
    ```
    * checking CRAN incoming feasibility ... NOTE
      Maintainer: 'Kiwamu Ishikura <ishikura.kiwamu@gmail.com>'

      New submission
    ```
    ```
    * checking for non-standard things in the check directory ... NOTE
    Found the following files/directories:
      ''NULL''
    ```
    ```
    * checking for detritus in the temp directory ... NOTE
    Found the following files/directories:
      'lastMiKTeXException'
    ```

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## revdepcheck results

There are currently no downstream dependencies for this package.
