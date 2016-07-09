# Git Gadget

> Warning: `gitgadget` is still very early in its development. Use at your own risk. May blow-up your computer.

`gitgadget` is an Rstudio addin for version control and assignment management using git. The assignment management functions currently only support the [gitlab](https://gitlab.com) API. PRs welcome.

`gitgadget` is not intended as tool for all-things-git. For that you need something like <a href="http://www.sourcetreeapp.com/" target="_blank">sourcetree</a>. Its goal is to provide a few additional features not (yet) available in Rstudio that can be useful to students and faculty using git for classes. For an excellent discussion of this type of functionality see [happy git with R](http://happygitwithr.com/) by Jenny Bryan.

Please use the issue tracker on GitHub to suggest enhancements or report problems: https://github.com/vnijs/gitgadget/issues.

## Introduce

[Introduce yourself to git](http://happygitwithr.com/hello-git.html)

- Sets a global user name and email for git. Although not required we recommend using your github or gitlab username here
- If available, `gitgadget` will put `~/.ssh/id_rsa.pub` into the clipboard if you click the `SSH key`. Provide the key to github or gitlab for authentication
- By default, `gitgadget` will try to turn on credential helpers for your platform. See <a href="http://happygitwithr.com/credential-caching.html target="_blank">http://happygitwithr.com/credential-caching.html</a> for additional information

> To avoid possible authentication issues with SSH on gitlab we recommend you clone, push, pull, etc. using HTTPS

## Create

Create a repo on gitlab from a local directory. If the local directory does not yet exist it will be created. If a `Group name` is provided it will be used to locate the repo. If left blank, the user name will be used as the group. Choose a name in `Repo name`. Note that `Local directory` is automatically updated using the content of `Repo name`. A `Prefix` can be used and will default to `git.prefix` in .Rprofile. Mainly used for assignment management to avoid conflicting assignment/case names.

`User file` is only relevant for assignment management (see section below). If a user file is provided a radio button is shown that can be used to indicate if the work should be completed individually or in teams.

## Clone

Clone a repo from gitlab or github (e.g., https://github.com/vnijs/gitgadget.git). The directory name inside `Clone into` will be taken from the repo name unless a name is provided in `Clone to`.

## Branch

Create a local branch from the active repo by providing a `Branch name` and clicking `Create local`. Link and push to the (gitlab) remote by clicking the `Link remote` button. If branches other than `master` exist you can `Merge` with `master` or delete the branches. `Unlink remote` will not remove the local or the remote repo, it will just remove the link (i.e., the push/pull arrows in Rstudio will gray-out).

Note that changes will only be visible in the Rstudio git interface after clicking the `refresh` button in the Git tab.

## Collect

Collect assignments from users/students using Merge (Pull) requests. TODO

## Assignment management

Key functions in `git.R` that can be accessed through the `gitgadget` interface are:

`create_group(...)`

Used to create a group for a course. The `Group name` variable should be set to something like "school-courseid-year". This creates the group and adds students as `reporters` with read-only access on gitlab. Access this functionality through the _Create_ tab in `gitgadget`.

Requires an input file of the following form:

| userid | team  | email              | pw |
|--------|-------|--------------------|----|
| id1    | team1 | student1@gmail.com | pw |
| id2    | team1 | student2@gmail.com | pw |
| id3    | team2 | student3@gmail.com | pw |


Note: Markdown for the table above was generated using <a href="http://www.tablesgenerator.com/markdown_tables" target="_blank">tablegenerator.com</a>

`create_repo(...)`

To create an assignmetn repo set `Local directory` to the directory with the assignment files.
Choose an assignment/case name in `Repo name`. A `Prefix` should be added to avoid conflicts across courses that might use the same assignment name (e.g., assignment1). Access this functionality through the _Create_ tab in `gitgadget`.

`assign_work(...)`

Forks the assignment repo uploaded earlier for each student/team and creates the appropriate groups for team work. Requires a csv file with the content shown below, including the students' (gitlab) passwords. Access to these password is feasible when student accounts were created centrally for the program (e.g., when running your own gitlab server).

`collect_work(...)`

TODO. Requires an input file of the form shown earlier.

## Initial settings

Supports the following starting values from an .Rprofile file:

```bash
options(git.user = "yourgitlabid")
options(git.email = "yourgitlabemail@gmail.com")
options(git.password = "")
options(git.home = "/Users/you/Desktop/git")
options(git.server = "https://gitlab.com/api/v3/")
options(git.prefix = "")
```

On Windows use a text editor (e.g., notepad) to create the .Rprofile file in your home directory. To find out what R thinks your home directory is enter `Sys.getenv("HOME")` in the R(studio) console
