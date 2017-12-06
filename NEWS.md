# CHANGES IN gitgadget VERSION 0.2.8.4

* Open project in new or existing session on clone
* Open to remove local git repo, remote gitlab repo, and student forks after confirmation
* Better error messages
* Updated to work with V4 of the gitlab API
* Added _Check tokens_ button in Create tab. Button is shown when file with student tokens specified. All student tokens will then be checked on GitLab

# CHANGES IN gitgadget VERSION 0.2.7.1

* Upgraded dplyr dependency to 0.7.1

# CHANGES IN gitgadget VERSION 0.2.6

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

# CHANGES IN gitgadget VERSION 0.2.3

## BUG FIXES

- Fix for `create_repo` when group already exists
- Updated links to source code and issue tracker
- Export main functions
- Avoid error when an account has multiple forks by picking the first
- Specify specifically that merge requests should be Fetched
- Improved regex for Rproj file creation on clone
- Update branch list on collect and fetch
- Get full remote orgin list from .git/config

## FEATURES 

- Automatically replace any number of "\\" by "/" in input file and directory paths
- Added option to remove previous .git and remote repo before creating new local and remote repos
