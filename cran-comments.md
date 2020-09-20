## Resubmission

This is a resubmission. In this version I have added some new features. See the NEWS.md file for details

## Test environments

* local OS X install, R 4.0.2
* local Windows install, R 4.0.2
* win-builder (devel)
* Rhub

## R CMD check results

There were no ERRORs or WARNINGs. There was one note related to the two links below. This occurs because access requires a gitlab accout. I'd prefer to keep these links in the documentation if possible

```
URL: https://gitlab.com/profile/keys
  From: inst/doc/gitgadget.html
        README.md
  Status: 503
  Message: Service Unavailable
URL: https://gitlab.com/profile/personal_access_tokens
  From: inst/doc/gitgadget.html
        README.md
  Status: 503
```

## Previous cran-comments

## Resubmission

This is a resubmission. In this version I have added some new features and fixed a few bugs. See the NEWS.md file for details

## Test environments

* local OS X install, R 4.0.2
* local Windows install, R 4.0.2
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs.

## Previous cran-comments

## Resubmission

This is a resubmission. In this version I have added some new features and fixed a few bugs. See the NEWS.md file for details

## Test environments

* local OS X install, R 3.6.1
* local Windows install, R 3.1.1
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs.


## Resubmission

This is a resubmission. In this version I have added some new features and fixed a few bugs. See the NEWS.md file for details

## Test environments

* local OS X install, R 3.5.2
* local Windows install, R 3.5.2
* local Ubuntu 18.04, R 3.5.2
* win-builder (release)

## R CMD check results

There were no ERRORs or WARNINGs.

## Previous cran-comments

## Resubmission

This is a resubmission. In this version I have:

- Fixed a bug in the `create_repo` function when a GitLab group already exists
- Updated links to source code and issue tracker
- Exported the main functions used in the package
- Added a vignette based on README.md

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE: Possibly mis-spelled words. Spelling is correct however.

## Previous cran-comments

## Resubmission

This is a resubmission. In this version I have:

* Changed github and gitlab to GitHub and GitLab in DESCRIPTION

## Previous cran-comments

## Resubmission

This is a resubmission. In the updated cran-comments file I provide a reply to Uwe Ligges' question: "The package does not contain any code that runs during R CMD check. How are we supposed to check that this works in principle?"

Reply:

* gitgadget is an rstudio addin with a structure similar to a shiny app. The UI code is in R/gadget.R and must be tested interactively. The code in R/git.R deals with the GitLab API and can only be tested with access to passwords and tokens. In sum, testing must be done by the package maintainer and the end-user. To test interactively you would need to install and run the app from the addins menu in Rstudio. Alternatively, install and run gitgadget:::gitgadget(). However, I understand that this is not something CRAN has the resources to do.
* Other, related, packages that are published on CRAN also lack traditional unit testing and examples. A list of Rstudio addins is [here](https://github.com/daattali/addinslist), several of which are published on CRAN. Most have no unit testing because, I assume, it is not clear how to do it effectively for CRAN
* The addin package that is most similar to gitgadget and is published on CRAN is [googleAuthR](https://github.com/MarkEdmondson1234/googleAuthR). It does have tests but they are deliberately skipped when run on CRAN because they require authentication/tokens that will not be available on other machines.
* Another package related to gitgadget that is published on CRAN is [gitlabr](https://cran.r-project.org/web/packages/gitlabr/index.html). Again, it does have tests but they are deliberately skipped when run on CRAN because they require authentication/tokens that will not be available on other machines.

## Resubmission
This is a resubmission. In this version I have:

* Provided a more informative description of what the package does and the functionality it adds beyond what Rstudio already offers.

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE: New submission

## Test environments
* local OS X install, R 3.3.1
* local Windows install, R 3.3.1
* ubuntu 12.04 (on travis-ci), R 3.3.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE: New submission
