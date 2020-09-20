# Git Gadget

[![Build Status](https://travis-ci.org/vnijs/gitgadget.png?branch=master)](https://travis-ci.org/vnijs/gitgadget)

`gitgadget` is an R-studio addin for version control and assignment management using git. The assignment management functions currently support the [GitLab](https://gitlab.com) API. PRs for use with GitHub are welcome. For alternative that uses GitHub see https://github.com/rundel/ghclass although this does not (currently) have a shiny user interface.

`gitgadget` is not intended as a tool for all-things-git. For that you need something like <a href="https://www.gitkraken.com" target="_blank">gitkraken</a>. `gitgadget` provides a few additional features not (yet) available in R-studio that can be useful to students and faculty using git for classes. For an excellent discussion of this level of functionality see [happy git with R](http://happygitwithr.com/) and [talk about version control](https://peerj.com/preprints/3159/) by Jenny Bryan and [git for humans](https://speakerdeck.com/alicebartlett/git-for-humans) by Alice Bartlett.

`gitgadget` requires Rstudio version 1.2 or later. Please use the [issue tracker](https://github.com/vnijs/gitgadget/issues) on GitHub to suggest enhancements or report problems. To install the latest version of `gitgadget` use the command below:

```r
install.packages("gitgadget", repos = "https://radiant-rstats.github.io/minicran/")
```

To start `gitgadget` click on the Addins menu in Rstudio and then on `gitgadget`.

On macOS, you will you will need to run the command below from a terminal to get access to git commands:

```bash
xcode-select --install;
```

On Windows you will need to install git bash from https://git-scm.com/download/win. Alternatively, you can use the following installer that we use with our students to ensure git is setup correctly for use with gitgadget:

https://rady.ucsd.edu/faculty/directory/vnijs/RSM-MSBA-Computing-Environment-Latest.exe

> Note: Hover over any button in the `gitgadget` application for additional information about what clicking the button will accomplish

## Introduce

To start using git and `gitgadget` first [introduce yourself to git](https://happygitwithr.com/hello-git.html). Set a global user name and email for git (e.g., your GitLab user name and email). By default, `gitgadget` will try to turn on an appropriate credential helper for your operating system. See [https://happygitwithr.com/credential-caching.html](https://happygitwithr.com/credential-caching.html) for additional information. 

To create and fork repos and collect merge requests you will need to provide a GitLab personal access token. First create the [token on GitLab](https://gitlab.com/profile/personal_access_tokens) and then copy-and-paste it in the `Token` input.

Unless you plan to use `gitgadget` to manage student assignments, select `student` as the `User type`. Finally, enter the main directory where you plan to clone code repos (e.g., "C:/Users/me/git")

If you want to use an SSH key, make sure to click on the `Introduce` button, restart Rstudio, and then click on the `SSH key` button. Copy the key shown in GitGadget to the [gitlab.com page](https://gitlab.com/profile/keys) that should have opened in your default browser. To securely connect to gitlab from your computer you will need to restart Rstudio and use `git clone git@some-private-repo` from a terminal in Rstudio the first time you clone a repo. After that, cloning, creating, etc. from GitGadget should work smoothly. See the videos below for a demonstration.

> Note: On Windows we recommend cloning and creating repos using HTTPS rather than SSH

#### Setup for git and gitlab

https://youtu.be/FQx_3EDQaXc

#### Individual assignment practice

https://youtu.be/CANV6-mfZ9I

#### Merge conflicts

https://youtu.be/elq5UUG0RbE

#### Group assignment practice

https://youtu.be/uwqUHl3z37o

> Note: In these videos we use a docker container (see https://github.com/radiant-rstats/docker/tree/master/install) but this is not required

## Create (GitLab only)

Enter the path to a local directory to create a repo on GitLab. If the local directory does not yet exist it will be created. You can also click the `Open` button and navigate to an existing directory. If a `Group name` is provided it will be used to place the repo on GitLab. This is recommended if you are using `gitgadget` for assignment management. If left blank, the GitLab user name will be used as the group. A `Prefix` can be added and will default to the value of `git.prefix` in .Renviron (see _Initial settings_ below). The prefix can be used to avoid conflicting assignment names across classes (e.g., multiple classes using "assignment1").

If you selected `faculty` as the `User type` in the _Introduce_ tab you will have the option to upload a CSV file with student tokens (see below). If a user file is provided, a radio button will be shown that can be used to indicate if the work should be completed individually or in teams. Furthermore, you can choose if you want to _hide_ or _show_ the master class repo to students. _Hiding_ the master repo can avoid confusion where a student might accidentally clone the wrong repo (i.e., the class repo rather than their own fork) and not be able to push there changes back to gitlab. _Showing_ the master repo, on the other hand, will make it possible to _sync_ any changes to the class repo after it was forked for each student.

If a CSV file with TA information is specified they will be added as "Maintainer" to the class repo. The will also be added as "Maintainer" to all created student (or team) forks. TA information should include `userid`, `token`, and `email`. 

## Clone

Clone a repo from GitLab (or GitHub) (e.g., `git@gitlab.com:username/test-repo.git`). The name for the directory placed inside `Base directory to clone repo into` will be taken from the repo name unless a `Custom directory to clone repo into` is provided. If there is no R-studio project file (`.Rproj`) in the remote repo, one will be created.

> Note: To activate an HTTPS credential helper the first time you clone a repo from GitHub or GitLab you will be asked to provide your username and password in the Rstudio terminal

> Note: To use an SSH key with GitGadget you should clone from a terminal the first time. See the video linked above (_Setup for git and gitlab_) for a demo. As mentioned above, for Windows users we recommend using HTTPS to clone and create repos.

## Branch

Create a local branch from the active branch by providing a name for the new branch and clicking `Create local`. Link and push to the (GitLab) remote by clicking the `Link remote` button. If remote branches exist click the `Check out` button to work with a branch locally. If branches other than `master` exist you can `Merge branches` or delete them if they are no longer needed. To undo a merge-in-progress, e.g., with merge conflicts, click the `Abort merge` button. `Unlink remote` will not remove the local or the remote branch but only the link to the remote (i.e., the push/pull arrows in R-studio will gray-out).

> Note: Changes to the list of available branches may only be visible in R-studio after clicking the `refresh` button in the Git tab.

## Sync

To commit changes to the local copy of your repo provide a `Commit message` and then click the `Commit` button. If you committed changes you are not sure about, the `Undo` button will let you revert the latest commit (after confirmation).

If the remote repo contains changes you do not yet have locally press the `Pull` button. To add committed local changes to the remote repo, click the `Push` button. It you completely broke the local repo and want to start over with a clean copy of the remote repo, press the `Reset` button. 

If the repo you are working on is a fork you probably want to make sure it is up to date and merge any changes into the version you are working on locally. The first step is to ensure that git knows about the upstream repo you forked from. Copy the SSH or HTTPS link to clone the original repo into the `Sync repo with remote it was forked from` input in the _Sync_ tab and then press `Sync`. The repo you forked will now be added as a _remote_. Click `Merge` to update your local copy of the repo with any changes to the original remote repo. Fix merge conflicts, if any, and proceed to work on the local repo. To undo a merge with merge conflicts click the `Abort merge` button. Click the `Unlink` to remove the reference to the upstream repo you forked from.

## Collect (GitLab only)

Collect assignments from students/teams using Merge Requests (MR) on GitLab. Inputs will only be shown if the user type in the _Introduce_ tab is set to `faculty`. 

If a CSV file with TA information is specified they removed from (_Hide_) or added to (_Show_) to all created student (or team) forks as a "Maintainer". TA information should include `userid`, `token`, and `email`. 

To collect and fetch assignments linked to class repo you must first open a clone of that repo in Rstudio and provide your `Token`. The `Assignment name` input should now show the assignment repo name on gitlab.com. Next, indicate of the assignment was `individual` or `team` and click the `Collect` button to generate Merge Requests for all students (or team leads). Once this step is completed you can review and comment on the MRs using the GitLab UI. Collecting assignments requires that MRs be generated, which in turn requires that the class repo be visible to students. The `Hide` button can be used to ensure students will not have access to the MRs. However, since this is likely to be after the assignment due data, visibility of the class repo and MRs may not be or much concern. The `Show` button will provide students access to the class repo and MRs.
 
To view, run, and edit the MRs locally, press the `Fetch` button. After fetching all MRs they are converted to branches using the student-id as the branch name and pushed back up to the server. Switch between branches to test code and provide comments and/or fixes. Use the Git tab in R-studio or the _Sync_ tab in `gitgadget` to push local changes to the remote branch. Notify students of the comments/fixes by providing a link to the branch in the comments on the original MR.

If students update their assignment repo these changes will be visible in the MRs as long as the class repo is visible to students (click the `Show` button to ensure access). If you want to `Fetch` these updates for local review you should first delete the existing branches for one or more students. This can be done through the _Branch_ tab by selecting all branches you want to delete and clicking the `Delete local` button. Then click the `Fetch` button in the _Collect_ tab again to obtain the latest version.

## Assignment management

Key functions in `git.R` that can be accessed through the `gitgadget` interface are:

`create_group(...)`

Used to create a group for a course. The `Group name` variable should be set to something like "school-courseid-year". This creates the group and adds students as, for example, `reporters` with read-only access on GitLab. Make sure to specify a [valid permission number](https://docs.gitlab.com/ee/api/access_requests.html). 

`create_repo(...)`

To create an assignment repo set `Local directory` to the directory with the assignment files. A `Prefix` could be added to avoid conflicts across courses that might use the same assignment name (e.g., assignment1). Access this functionality through the _Create_ tab in `gitgadget`.

`assign_work(...)`

Forks the assignment repo uploaded by a faculty member or TA for each student/team and creates the appropriate groups for team work. This requires a csv file with the structure shown below, including the students' (GitLab) private token. 

| userid | team  | email              | token     |
|--------|-------|--------------------|-----------|
| id1    | team1 | student1@gmail.com | px....... |
| id2    | team1 | student2@gmail.com | n9....... |
| id3    | team2 | student3@gmail.com | VR....... |

Note: Markdown for the table above was generated using <a href="https://www.tablesgenerator.com/markdown_tables" target="_blank">tablegenerator.com</a>

`collect_work(...)`

Generates Merge Requests for all students or teams. Requires a csv file with the structure shown above.

`fetch_work(...)`

Fetch Merge Requests for all students or teams from the GitLab server. After fetching all MRs they are converted to branches using the student-id as the branch name and pushed back up to the server. Switch between branches to test code and provide comments. Push to update the remote branch with the local changes.

## Initial settings

GitGadget supports the following input from an .Renviron file (e.g., "C:/Users/me/.Renviron"). The easiest way to view `.Renviron` is to click on the `Check` button in the _Introduce_ tab or use `usethis::edit_r_environ()`.

```bash
git.user = "your-gitlab-id"
git.email = "youremail@ucsd.edu"
git.token = "abc123"
git.home = "~/git"
git.server = "https://gitlab.com/api/v4/"
git.group = "school-courseid-2019"
git.prefix = "school-courseid-2019-"
git.userfile = "path-to-user-file.csv"
git.tafile = "path-to-ta-file.csv"
git.user.type = "faculty"
```
