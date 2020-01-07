# gitgadget 0.5.4.0

* Various updates to allow using gitgadget with shiny server

# gitgadget 0.5.3.0

* Warning if user tries to `create` a repo from the base git directory
* Code base restructured
* Added inst/app/app.R for use with shiny server

# gitgadget 0.5.2.0

* Set `warn = FALSE` for `readLines`
* Permissions are now set at the repo/projects level, rather than at the group level
* Both Create and Collect now have the option to _Show_ (i.e., add permissions) or _Hide_ (i.e., remove permissions) from a repo
* Both Create and Collect now have the option load a csv file with TA information. TAs are added as "Maintainer" to the class repo. Also, TAs are added as a "maintainer" to all repos forked for students (or teams). 
* The Collect tab also an option to _Hide_ and _Show_ forks created for students or teams.  
to _Show_ (i.e., add permissions) or _Hide_ (i.e., remove permissions) from a repo
* Fix to address that on Windows the global .gitconfig file may be put in the Documents folder
* Added a `Check` button to the Introduce tab so you can easily check and edit the .Renviron and .Rprofile settings using `usethis` functions
* Allow both the "Owner" and the "Maintainer" of a repo to create merge requests with Collect & Fetch

# gitgadget 0.4.4.0

* `.gitignore` file was not picked up if it already existed. Fixed in this release
* Fix for generating ssh key with alternative name when .ssh/config does not yet exist 

# gitgadget 0.4.2.0

* Enhancement for initial setup with SSH
* Add .gitlab-ci.yml to avoid warning emails about CI pipelines

# gitgadget 0.3.3.0

* Collect and Fetch assignments using either HTTPS or SSH

# gitgadget 0.3.2.0

* Use ssh keys with GitLab

# gitgadget 0.3.0.0

* Option to provide GitLab or GitHub personal access tokens in _Introduce_
* Create a repo on GitLab or GitHub in _Create_. `usethis::use_github` is used to create a repo on GitHub

# gitgadget 0.2.9.0

* Improved reporting when creating and forking repos
* When credentials have not yet been stored in a keychain cloning (and creating) with throw an error in Rstudio (i.e., fatal: rpostback-askpass). If you start `gitgadget` to clone a repo it will switch to the terminal tab in Rstudio and insert a git clone command. That command will ask for credentials and clone the repo as requested. From then on, cloning and creating should work fine because credentials have been stored

# gitgadget 0.2.8.5

* Open project in new or existing session on clone
* Open to remove local git repo, remote gitlab repo, and student forks after confirmation
* Better error messages
* Updated to work with V4 of the gitlab API
* Added _Check tokens_ button in Create tab. Button is shown when file with student tokens specified. All student tokens will then be checked on GitLab

# gitgadget 0.2.7.1

* Upgraded dplyr dependency to 0.7.1

# gitgadget 0.2.6.0

- Added commit, push, pull, and reset, to Sync tab
- Added placeholders for text inputs
- Added choose.dir script for mac
- Update documentation
- Added confirmation dialog for destructive commands (red buttons)
- Added help button to gadget header
- Specify user type in Introduction tab
- Hide inputs in Collect tab from students
- Option to remove multiple (student) branches in Branch tab
- Remove option to use SSH
- Local branches will now be updated if student MR was updated

# gitgadget 0.2.3.0

## Bug fixes

- Fix for `create_repo` when group already exists
- Updated links to source code and issue tracker
- Export main functions
- Avoid error when an account has multiple forks by picking the first
- Specify specifically that merge requests should be Fetched
- Improved regex for Rproj file creation on clone
- Update branch list on collect and fetch
- Get full remote origin list from .git/config

## Features 

- Automatically replace any number of "\\" by "/" in input file and directory paths
- Added option to remove previous .git and remote repo before creating new local and remote repos
