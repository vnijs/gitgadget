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
- Option to remove multiple (student) brances in Branch tab
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
- Get full remote orgin list from .git/config

## Features 

- Automatically replace any number of "\\" by "/" in input file and directory paths
- Added option to remove previous .git and remote repo before creating new local and remote repos
