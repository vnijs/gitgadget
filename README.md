# Git Gadget

> Warning: `gitgadget` is still very early in its development. Use at your own risk. May blow-up your computer.

`gitgadget` is an Rstudio addin for version control and assignment management using git. The assignment management functions currently only support the [gitlab](https://gitlab.com) API. PRs for githubs, bitbucket, etc welcome.

`gitgadget` is not intended as a tool for all-things-git. For that you need something like <a href="http://www.sourcetreeapp.com/" target="_blank">sourcetree</a>. Its goal is to provide a few additional features not (yet) available in Rstudio that can be useful to students and faculty using git for classes. For an excellent discussion of this type of functionality see [happy git with R](http://happygitwithr.com/) by Jenny Bryan.

Please use the issue tracker on GitHub to suggest enhancements or report problems: https://github.com/vnijs/gitgadget/issues.

## Introduce

To start using git and gitgadget [introduce yourself to git](http://happygitwithr.com/hello-git.html)

Set a global user name and email for git. Please use your gitlab user name. If available, `gitgadget` will put `~/.ssh/id_rsa.pub` into the clipboard when you click the `SSH key` button. Provide the key to github or gitlab for authentication. By default, `gitgadget` will try to turn on credential helpers for your platform. See <a href="http://happygitwithr.com/credential-caching.html target="_blank">http://happygitwithr.com/credential-caching.html</a> for additional information

> To avoid possible authentication issues with SSH on gitlab we recommend you clone, push, pull, etc. using HTTPS

## Create

Enter the path to a local directory to create a repo on gitlab. If the local directory does not yet exist it will be created. For existing directories you can click the `Open` button and navigate to a file in the directory of interest. The directory path will be parsed from the provided file path. If a `Group name` is provided it will be used to place the repo on gitlab. If left blank, the user name will be used as the group. A `Prefix` can be used and will default to the value of `git.prefix` in .Rprofile in your home directory (see _Initial settings_ below). The prefix is used for assignment management to avoid conflicting assignment/case names.

`User file` is only relevant for assignment management (see below). If a user file is provided a radio button will be shown that can be used to indicate if the work should be completed individually or in teams.

## Clone

Clone a repo from gitlab (or github) (e.g., https://github.com/vnijs/gitgadget-test-repo). The name for the directory placed inside `Clone into` will be taken from the repo name unless an alternative name is provided in `Clone to`.

## Branch

Create a local branch from the active repo by providing a `Branch name` and clicking `Create local`. Link and push to the (gitlab) remote by clicking the `Link remote` button. If branches other than `master` exist you can `Merge` them with the master branch or delete them if no longer needed. `Unlink remote` will not remove the local or the remote repo but only the link to the remote (i.e., the push/pull arrows in Rstudio will gray-out).

Note that changes may only be visible in the Rstudio after clicking the `refresh` button in the Git tab.

## Sync

If the repo you are working on is a fork you probably want to make sure it is up to date and merge any changes into the version you are working on locally. The first step is to ensure that git knows about the `upstream` repo you forked from. Copy the HTTPS link to clone the original repo into the `Sync from` input in the _Sync_ tab and then press `Sync`. The repo you forked will now be added as a _remote_. Click `Merge` to update your local version with any changes to the original. Fix merge conflicts if they pop up and proceed working on the repo. To undo the merge simply click the `Undo` button. When you are done commit and then push your changes to gitlab.

> Make sure **not** to click the `Undo` button more than once or you may lose previous (local) commits.

## Collect

Collect assignments from students/teams using Merge (Pull) Requests on gitlab. When you navigate to the _Collect_ tab gitgadget will search for all assignments in the specified group on gitlab. Once all inputs are provided, including the path to a file with student information, press the `Collect` button. Merge (pull) requests will be generated for all students (or team leads). Once this step is completed you can review and comment on the MRs on gitlab. To view the MRs locally press the `Fetch` button.

## Assignment management

Key functions in `git.R` that can be accessed through the `gitgadget` interface are:

`create_group(...)`

Used to create a group for a course. The `Group name` variable should be set to something like "school-courseid-year". This creates the group and adds students as `reporters` with read-only access on gitlab. Access this functionality through the _Create_ tab in `gitgadget`. Creating a group requires an input file of the following form:

| userid | team  | email              | token     |
|--------|-------|--------------------|-----------|
| id1    | team1 | student1@gmail.com | px....... |
| id2    | team1 | student2@gmail.com | n9....... |
| id3    | team2 | student3@gmail.com | VR....... |


Note: Markdown for the table above was generated using <a href="http://www.tablesgenerator.com/markdown_tables" target="_blank">tablegenerator.com</a>

`create_repo(...)`

To create an assignment repo set `Local directory` to the directory with the assignment files. A `Prefix` should be added to avoid conflicts across courses that might use the same assignment name (e.g., assignment1). Access this functionality through the _Create_ tab in `gitgadget`.

`assign_work(...)`

Forks the assignment repo uploaded by a faculty member or TA earlier for each student/team and creates the appropriate groups for team work. Requires a csv file with the structure shown above, including the students' (gitlab) private token. Getting access to these tokens will most likely require that student accounts are created centrally for the class/program (e.g., running your own gitlab server).

`collect_work(...)`

Generates merge (pull) request for all students or students teams. Requires an input file of the form specified above.

## Initial settings

Gitgadget supports the following input from an .Rprofile file:

```bash
options(git.user = "yourgitlabid")
options(git.email = "yourgitlabemail@gmail.com")
options(git.password = "")
options(git.home = "/Users/you/Desktop/git")
options(git.server = "https://gitlab.com/api/v3/")
options(git.group = "school-courseid-2016")
options(git.prefix = "school-courseid-2016-")
options(git.userfile = "path-to-user-file.csv")
```

On Windows use a text editor (e.g., notepad) to create .Rprofile in your home directory. To find out where R thinks your home directory is enter `Sys.getenv("HOME")` in the R(studio) console
