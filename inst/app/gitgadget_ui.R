gitgadget_ui <- function() {
  miniPage(
    miniTitleBar(
      paste0("GITGADGET (", packageVersion("gitgadget"), ")"),
      right = actionButton(
        "done", "Done",
        class = "btn-sm btn-primary",
        onclick = "setTimeout(function(){window.close();}, 100);"
      ),
      left = miniTitleBarButton("help", "Help", primary = FALSE)
    ),
    includeCSS(file.path(system.file("app", package = "gitgadget"), "www/style.css")),
    miniTabstripPanel(
      id = "tabs",
      miniTabPanel("Introduce",
        value = "intro", icon = icon("hand-paper", verify_fa = FALSE),
        miniContentPanel(
          HTML("<h2>Introduce yourself to git</h2>"),
          textInput(
            "intro_user_name", "User name:",
            value = Sys.getenv("git.user"),
            placeholder = "Provide GitLab/GitHub user name"
          ),
          textInput(
            "intro_user_email", "User email:",
            value = Sys.getenv("git.email"),
            placeholder = "Provide GitLab/GitHub user email"
          ),
          textInput("intro_server", "Server API:", value = Sys.getenv("git.server", "https://gitlab.com/api/v4/")),
          fillRow(
            height = "70px", width = "475px",
            passwordInput("intro_token_gl", "GitLab token:", value = Sys.getenv("git.token")),
            uiOutput("ui_intro_get_token")
          ),
          # fillRow(height = "70px", width = "475px",
          #   passwordInput("intro_token_gh","GitHub token:", value = Sys.getenv("GITHUB_PAT")),
          #   actionButton(
          #     "intro_token_gh_get", "Create",
          #     title = "Browse to GitHub to get a PAT", style = "margin-top: 25px;",
          #     onclick = "window.open('https://github.com/settings/tokens/new?scopes=repo,gist&description=R:GITHUB_PAT', '_blank')"
          #   )
          # ),
          radioButtons(
            "intro_user_type", "User type:", c("student", "faculty"),
            Sys.getenv("git.user.type", "student"),
            inline = TRUE
          ),
          fillRow(
            height = "70px", width = "475px",
            uiOutput("ui_intro_git_home"),
            shinyFiles::shinyDirButton(
              "intro_git_home_open", "Open",
              title = "Browse and select a local directory",
              style = "margin-top: 25px;"
            )
          ),
          uiOutput("ui_intro_buttons"),
          hr(),
          verbatimTextOutput("introduce_output")
        )
      ),
      miniTabPanel("Create",
        value = "create", icon = icon("git", verify_fa = FALSE),
        miniContentPanel(
          # HTML("<h2>Create a repo on GitLab or GitHub</h2>"),
          HTML("<h2>Create a repo on GitLab</h2>"),
          selectInput("create_remote", NULL, choices = "GitLab", selected = "GitLab"),
          # selectInput("create_remote", NULL, choices = c("GitLab", "GitHub"), selected = "GitLab"),
          conditionalPanel(
            "input.create_remote == 'GitLab'",
            textInput("create_server", "API server:", value = Sys.getenv("git.server", "https://gitlab.com/api/v4/"))
          ),
          radioButtons("create_ssh", "Authentication type:", c("ssh", "https"), "ssh", inline = TRUE),
          fillRow(
            height = "70px", width = "300px",
            textInput("create_user_name", "User name:", value = Sys.getenv("git.user")),
            uiOutput("ui_create_token")
          ),
          conditionalPanel(
            "input.create_remote == 'GitLab'",
            fillRow(
              height = "70px", width = "300px",
              textInput("create_group", "Group name:", value = Sys.getenv("git.group")),
              uiOutput("ui_create_pre")
            )
          ),
          fillRow(
            height = "70px", width = "475px",
            uiOutput("ui_create_directory"),
            shinyFiles::shinyDirButton(
              "create_directory_find", "Open",
              title = "Browse and select a local directory",
              style = "margin-top: 25px;"
            )
          ),
          conditionalPanel(
            "input.intro_user_type == 'faculty' && input.create_remote == 'GitLab'",
            fillRow(
              height = "70px", width = "475px",
              uiOutput("ui_create_user_file"),
              shinyFiles::shinyFilesButton(
                "create_file_find", "Open",
                multiple = FALSE,
                title = "Browse and select a CSV file with student id and token information. Used for assignment management by instructors"
              )
            ),
            fillRow(
              height = "70px", width = "475px",
              uiOutput("ui_create_ta_file"),
              shinyFiles::shinyFilesButton(
                "create_tafile_find", "Open",
                multiple = FALSE,
                title = "Browse and select a CSV file with TA id and token information. Used for assignment management by instructors",
                style = "margin-top: 25px;"
              )
            ),
            conditionalPanel(
              "input.intro_user_type == 'faculty' && input.create_user_file != ''",
              actionButton("create_check_tokens", "Check tokens", title = "Check student token information on GitLab"),
              radioButtons("create_type", "Assignment type:", c("individual", "team"), "individual", inline = TRUE)
            )
          ),
          HTML("<h4>Remove existing remote repo local .git directory</h4>"),
          actionButton(
            "remove_remote_show", "Remove remote",
            title = "Remove remote repo if present", class = "btn-danger"
          ),
          actionButton(
            "remove_git_show", "Remove .git",
            title = "Remove local .git directory if present", class = "btn-danger"
          ),
          HTML("<h4>Create local .git and remote repo</h4>"),
          uiOutput("ui_create_buttons"),
          hr(),
          verbatimTextOutput("create_output")
        )
      ),
      miniTabPanel("Clone",
        value = "clone", icon = icon("clone", verify_fa = FALSE),
        miniContentPanel(
          HTML("<h2>Clone a repo</h2>"),
          textInput(
            "clone_from", "Repo to clone from remote git server:",
            placeholder = "Provide https or ssh link to repo", value = ""
          ),
          fillRow(
            height = "70px", width = "475px",
            uiOutput("ui_clone_into"),
            shinyFiles::shinyDirButton(
              "clone_into_open", "Open",
              title = "Browse and select a local directory",
              style = "margin-top: 25px;"
            )
          ),
          textInput(
            "clone_to", "Custom directory to clone repo into:",
            placeholder = "Use for custom directory only", value = ""
          ),
          {
            if (rstudioapi::isAvailable()) {
              radioButtons(
                "clone_proj", "Open project in:",
                c("current session" = "curr", "new session" = "new"),
                "new",
                inline = TRUE
              )
            }
          },
          actionButton(
            "clone", "Clone",
            title = "Clone a repo from, e.g., github or gitlab over HTTPS or SSH. By default, the name of the remote repo and the local clone will be the same. To change the name for the local repo to create, provide an alternative in the 'Custom directory' input\n\nGit command:\ngit clone <remote url>\n\nNote: To activate a credential helper the first time you clone a (private) repo from, e.g., github or gitlab, run 'git clone <remote url>' from the command line"
          ),
          hr(),
          verbatimTextOutput("clone_output")
        )
      ),
      miniTabPanel("Directory",
        value = "directory", icon = icon("folder", verify_fa = FALSE),
        miniContentPanel(
          fillRow(
            height = "40px", width = "475px",
            HTML("<h4>Change the repo directory</h4>"),
            tags$a(id = "repo_refresh", href = "#", class = "action-button", list(icon("sync", verify_fa = FALSE), ""))
          ),
          fillRow(
            height = "40px", width = "475px",
            uiOutput("ui_repo_directory"),
            shinyFiles::shinyDirButton(
              "repo_directory_find", "Open",
              title = "Browse and select a repo directory"
            )
          ),
          verbatimTextOutput("repo_output")
        )
      ),
      miniTabPanel("Sync",
        value = "sync", icon = icon("sync", verify_fa = FALSE),
        miniContentPanel(
          HTML("<h2>Commit changes locally</h2>"),
          uiOutput("ui_sync_commit_message"),
          actionButton(
            "sync_stage", "Stage",
            title = "Stage files and show changes\n\nGit commands:\ngit add .\ngit diff --staged "
          ),
          actionButton(
            "sync_commit", "Commit",
            title = "Commit all updated files to the local repo\n\nGit commands:\ngit add .\ngit commit -m \"Commit message\""
          ),
          actionButton(
            "sync_undo_commit_show", "Undo",
            class = "btn-danger",
            title = "Undo the latest local commit\n\nGit command:\ngit reset ~HEAD"
          ),
          HTML("<h2>Sync with remote</h2>"),
          actionButton("sync_pull", "Pull", title = "Pull updates from remote repo\n\nGit command: git pull"),
          actionButton(
            "sync_push", "Push",
            title = "Push all commited updates to the remote repo\n\nGit command: git push"
          ),
          actionButton(
            "sync_check", "Check",
            class = "btn-success",
            title = "Check the git server for the pushed changes"
          ),
          actionButton(
            "sync_reset_show", "Reset",
            class = "btn-danger",
            title = "Completely reset local repo to remote main branch\n\nGit commands:\ngit fetch --all\ngit reset --hard origin/main"
          ),
          uiOutput("ui_sync_check"),
          HTML("<h2>Sync a fork</h2>"),
          uiOutput("ui_sync_from"),
          actionButton(
            "sync", "Sync",
            title = "Link the local repo with the original from which it was forked and pull an updated copy into an upstream/ branch\n\nGit commands:\ngit remote add upstream <remote url>\ngit fetch upstream"
          ),
          actionButton(
            "sync_merge", "Merge",
            title = "Merge the upstream/ branch(es) from the original with the local branch(es)\n\nGit commands:\ngit checkout main\ngit merge upstream/main"
          ),
          actionButton(
            "synch_abort", "Abort merge",
            title = "Abort the merge in progress\n\nGit command:\ngit merge --abort"
          ),
          actionButton(
            "sync_unlink", "Unlink",
            title = "Remove a link between a local repo and the original from which it was forked\n\nGit command:\ngit remote remove upstream"
          ),
          hr(),
          verbatimTextOutput("sync_output")
        )
      ),
      miniTabPanel("Branch",
        value = "branch", icon = icon("code-branch", verify_fa = FALSE),
        miniContentPanel(
          HTML("<h4>Create a new branch</h4>"),
          uiOutput("ui_branch_create_name"),
          actionButton(
            "branch_create", "Create local",
            title = "Create a new local branch based on the currently active branch. Click the refresh button in Rstudio's Git tab to view the updated list of branches\n\nGit command:\ngit branch -b <branch>"
          ),
          actionButton(
            "branch_link", "Link remote",
            title = "Link the local branch to a (new) remote branch\n\nGit command:\ngit push --set-upstream origin <branch>"
          ),
          actionButton(
            "branch_create_from_mr", "Create from MR",
            title = "Create a local branch from a Merge/Pull request\n"
          ),
          HTML("<h4>Check out a branch</h4>"),
          fillRow(
            height = "40px", width = "420px",
            uiOutput("ui_branch_checkout_name"),
            actionButton("branch_checkout", "Check out", title = "Check out a branch\n\nGit command:\ngit checkout <branch>")
          ),
          HTML("<h4>Merge branches</h4>"),
          uiOutput("ui_branch_merge_branches"),
          actionButton(
            "branch_merge", "Merge branches",
            title = "Merge the 'from' branch into the 'into' branch\n\nGit commands:\ngit checkout <from branch>\ngit merge <into branch>"
          ),
          actionButton(
            "branch_abort", "Abort merge",
            title = "Abort the merge in progress\n\nGit command:\ngit merge --abort"
          ),
          HTML("<h4>Delete existing branch(es)</h4>"),
          uiOutput("ui_branch_delete_name"),
          actionButton(
            "branch_unlink", "Unlink remote",
            title = "Unlink the local and the remote branch(es). The remote branch(es) will not be deleted\n\nGit command:\ngit branch -d -r origin/<branch>"
          ),
          actionButton(
            "branch_delete", "Delete local",
            title = "Remove the local branch(es)\n\nGit commands:\ngit checkout main\ngit branch -D <branch>"
          ),
          br(), br()
        )
      ),
      miniTabPanel("Collect",
        value = "collect", icon = icon("cloud-download-alt", verify_fa = FALSE),
        miniContentPanel(
          conditionalPanel(
            "input.intro_user_type == 'faculty'",
            HTML("<h2>Collect assignments</h2>"),
            passwordInput("collect_token", "Token:", value = Sys.getenv("git.token")),
            uiOutput("ui_collect_assignment"),
            conditionalPanel(
              "input.collect_assignment != undefined && input.collect_assignment != null &&
                input.collect_assignment.length > 0",
              fillRow(
                height = "70px", width = "475px",
                uiOutput("ui_collect_user_file"),
                shinyFiles::shinyFilesButton(
                  "collect_file_find", "Open",
                  multiple = FALSE,
                  title = "Browse and select a CSV file with student id and token information. Used for assignment management by instructors"
                )
              ),
              downloadButton("collect_check_status", "Check status", title = "Check status of student repo", class = "btn-warning"),
              fillRow(
                height = "70px", width = "475px",
                uiOutput("ui_collect_ta_file"),
                shinyFiles::shinyFilesButton(
                  "collect_tafile_find", "Open",
                  multiple = FALSE,
                  title = "Browse and select a CSV file with TA id and token information. Used for assignment management by instructors",
                  style = "margin-top: 25px;"
                )
              ),
              conditionalPanel(
                "input.intro_user_type == 'faculty' && input.collect_ta_file != ''",
                actionButton(
                  "collect_hide_from_ta", "Hide",
                  title = "Hide student forks from TA",
                  class = "btn-warning"
                ),
                actionButton("collect_show_to_ta", "Show", title = "Show student forks to TA", class = "btn-success")
              ),
              textInput(
                "collect_server", "API server:",
                value = Sys.getenv("git.server", "https://gitlab.com/api/v4/")
              ),
              radioButtons("collect_type", "Assignment type:", c("individual", "team"), "individual", inline = TRUE),
              actionButton(
                "collect", "Collect",
                title = "Create merge requests from all student forks using the gitlab API. Used for assignment management by instructors"
              ),
              actionButton(
                "collect_fetch", "Fetch",
                title = "Create local branches from all merge requests and link them to (new) remote branches. Used for assignment management by instructors"
              ),
              actionButton("collect_hide_repo", "Hide", title = "Hide class repo from students", class = "btn-warning"),
              actionButton("collect_show_repo", "Show", title = "Show class repo to students", class = "btn-success")
            ),
            hr(),
            verbatimTextOutput("collect_output")
          ),
          conditionalPanel(
            "input.intro_user_type != 'faculty'",
            HTML("<h2>Used only for assignment management by faculty</h2>"),
            HTML("<h2>Change setting to 'faculty' in the Introduction tab if needed</h2>")
          )
        )
      )
    )
  )
}