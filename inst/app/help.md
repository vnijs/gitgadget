# Git Gadget

[![Build Status](https://travis-ci.org/vnijs/gitgadget.png?branch=master)](https://travis-ci.org/vnijs/gitgadget)

`gitgadget` is an R-studio addin for version control and assignment management using git. The assignment management functions currently support the [GitLab](https://gitlab.com) API. PRs for GitHub, bitbucket, etc. are welcome.

`gitgadget` is not intended as a tool for all-things-git. For that you need something like <a href="http://www.sourcetreeapp.com/" target="_blank">sourcetree</a>. It's goal is to provide a few additional features not (yet) available in R-studio that can be useful to students and faculty using git for classes. For an excellent discussion of this level of functionality see [happy git with R](http://happygitwithr.com/) by Jenny Bryan.

Please use the issue tracker on GitHub to suggest enhancements or report problems: https://github.com/vnijs/gitgadget/issues.

> Note: Hover over any button in the application for additional information about what clicking the button will accomplish

## Introduce

To start using git and `gitgadget` [introduce yourself to git](http://happygitwithr.com/hello-git.html). Set a global user name and email for git (e.g., your GitLab user name and email). By default, `gitgadget` will try to turn on credential helpers for your platform. See [http://happygitwithr.com/credential-caching.html](http://happygitwithr.com/credential-caching.html) for additional information.

<!-- If available, `gitgadget` will put `~/.ssh/id_rsa.pub` into the clipboard when you click the `SSH key` button. Provide the key to github or gitlab for authentication. -->

Unless you plan to use `gitgadget` to manage student assignments use `student` as the `User type`.

> To avoid possible authentication issues with SSH on GitLab we recommend you clone, push, pull, etc. using HTTPS

## Create (GitLab only)

Enter the path to a local directory to create a repo on GitLab. If the local directory does not yet exist it will be created. For existing directories you can click the `Open` button and navigate to the desired directory. If a `Group name` is provided it will be used to place the repo on Gitlab. This is recommended if you are using `gitgadget` for assignment management. If left blank, the GitLab user name will be used as the group. A `Prefix` can be added and will default to the value of `git.prefix` in .Rprofile in your home directory (see _Initial settings_ below). If a default value is not available, a "-" will be added to the provided `Group name`. The prefix is used for assignment management to avoid conflicting assignment names across classes.

If you selected `faculty` as the `User type` in the _Introduce_ tab you will have the option to upload a CSV file with student tokens (see below). If a user file is provided a radio button will be shown that can be used to indicate if the work should be completed individually or in teams.

## Clone

Clone a repo from GitLab (or GitHub) (e.g., https://github.com/vnijs/gitgadget-test-repo). The name for the directory placed inside `Base directory to clone repo into` will be taken from the repo name unless a `Custom directory to clone repo into` is provided. If there is no R-studio project file (`.Rproj`) in the repo one will be created.

> To activate an HTTPS credential helper the very first time you clone a repo from GitHub or GitLab you should use a terminal and run git from the command line (i.e., git clone https://repo-to-clone)

## Branch

Create a local branch from the active branch by providing a name for the new branch and clicking `Create local`. Link and push to the (GitLab) remote by clicking the `Link remote` button. If remote branches exists click the `Check out` button to work with a branch locally. If branches other than `master` exist you can `Merge branches` or delete them if they are no longer needed. To undo a merge-in-progress, e.g., with merge conflicts, click the `Abort merge` button. `Unlink remote` will not remove the local or the remote branch but only the link to the remote (i.e., the push/pull arrows in R-studio will gray-out).

> Changes to the list of available branches may only be visible in R-studio after clicking the `refresh` button in the Git tab.

## Sync

To commit changes to the local copy of your repo provide a `Commit message` and then click the `Commit` button. If the remote repo contains changes you do not yet have locally press the `Pull` button. To add committed local changes to the remote repo, click the `Push` button. It you completely broke the local repo and want to start over with a clean copy of the remote repo, press the `Reset` button. 

If the repo you are working on is a fork you probably want to make sure it is up to date and merge any changes into the version you are working on locally. The first step is to ensure that git knows about the `upstream` repo you forked from. Copy the HTTPS link to clone the original repo into the `Sync repo with remote it was forked from` input in the _Sync_ tab and then press `Sync`. The repo you forked will now be added as a _remote_. Click `Merge` to update your local version with any changes to the original remote repo. Fix merge conflicts if they pop up and proceed to work on the local repo. To undo a merge with merge conflicts click the `Abort merge` button. 

## Collect (GitLab only)

Collect assignments from students/teams using Merge Requests (MR) on GitLab. Inputs will only be shown if the user type in the _Introduce_ tab is set to `faculty`. 

First, provide `User name`, `Password`, and the GitLab `Group name`. Then click the `List` button to show available assignments. Select the desired assignment name from the dropdown and the load the user file with student GitLab tokens and press the `Collect` button to generate Merge Requests
 for all students (or team leads). Once this step is completed you can review and comment on the MRs using the GitLab UI.
 
To view, run, and edit the MRs locally press the `Fetch` button. After fetching all MRs they are converted to branches using the student-id as the branch name and pushed back up to the server. Switch between branches to test code and provide comments and/or fixes. Use the Git tab in R-studio or the _Synch_ tab in `gitgadget` to push local changes to the remote branch. Notify students of the comments/fixes by providing a link to the branch in the comments on the original MR.

If students update their assignment repo and you want to `Fetch` the updates for local review you should first delete the existing branches for one or more students. This can be done through the _Branch_ tab by selecting all branches you want to delete and clicking the `Delete local` button.

## Assignment management

Key functions in `git.R` that can be accessed through the `gitgadget` interface are:

`create_group(...)`

Used to create a group for a course. The `Group name` variable should be set to something like "school-courseid-year". This creates the group and adds students as `reporters` with read-only access on GitLab. Access this functionality through the _Create_ tab in `gitgadget`. Creating a group requires an input file of the following form:

| userid | team  | email              | token     |
|--------|-------|--------------------|-----------|
| id1    | team1 | student1@gmail.com | px....... |
| id2    | team1 | student2@gmail.com | n9....... |
| id3    | team2 | student3@gmail.com | VR....... |

Note: Markdown for the table above was generated using <a href="http://www.tablesgenerator.com/markdown_tables" target="_blank">tablegenerator.com</a>

`create_repo(...)`

To create an assignment repo set `Local directory` to the directory with the assignment files. A `Prefix` should be added to avoid conflicts across courses that might use the same assignment name (e.g., assignment1). Access this functionality through the _Create_ tab in `gitgadget`.

`assign_work(...)`

Forks the assignment repo uploaded by a faculty member or TA for each student/team and creates the appropriate groups for team work. Requires a csv file with the structure shown above, including the students' (GitLab) private token. Getting access to these tokens will most likely require that student accounts are created centrally for the class/program.

`collect_work(...)`

Generates Merge Requests for all students or teams. Requires a csv file with the structure shown above.

`fetch_work(...)`

Fetch Merge Requests for all students or teams from the GitLab server. After fetching all MRs they are converted to branches using the student-id as the branch name and pushed back up to the server. Switch between branches to test code and provide comments. Push to update the remote branch with the local changes.

## Initial settings

Gitgadget supports the following input from an .Rprofile file in your home directory:

```bash
options(git.user = "yourgitlabid")
options(git.email = "yourgitlabemail@gmail.com")
options(git.password = "")
options(git.home = "/Users/you/Desktop/git")
options(git.server = "https://gitlab.com/api/v3/")
options(git.group = "school-courseid-2016")
options(git.prefix = "school-courseid-2016-")
options(git.userfile = "path-to-user-file.csv")
options(git.user.type = "faculty")
```

Use a text editor (e.g., notepad on Windows) to create or edit .Rprofile in your home directory.
