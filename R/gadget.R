## copied from https://github.com/rstudio/shiny
## as not exported
get_port <- function() {
  randomInt <- function(min, max) {
    min + sample(max-min, 1)-1
  }

  while (TRUE) {
    port <- randomInt(3000, 8000)
    # Reject ports in this range that are considered unsafe by Chrome
    # http://superuser.com/questions/188058/which-ports-are-considered-unsafe-on-chrome
    # https://github.com/rstudio/shiny/issues/1784
    if (!port %in% c(3659, 4045, 6000, 6665:6669, 6697)) {
      break
    }
  }
  port
}

#' Launch gitgadget in Rstudio viewer if available
#'
#' @details See \url{https://github.com/vnijs/gitgadget} for documentation
#'
#' @param port Port to use for the app
#'
#' @export
gitgadget <- function(port = get_port()) {

  os_type <- Sys.info()["sysname"]

  find_home <- function(os_type = Sys.info()["sysname"]) {
    if (os_type == "Windows") {
      ## gives /Users/x and not /Users/x/Documents
      normalizePath(
        file.path(Sys.getenv("HOMEDRIVE"), Sys.getenv("HOMEPATH")),
        winslash = "/"
      )
    } else {
      Sys.getenv("HOME")
    }
  }

  homedir <- find_home()
  renvirdir <- Sys.getenv("HOME")
  projdir <- basedir <- NULL

  ## setting up volumes for shinyFiles
  gg_volumes <- c(Home = homedir)
  git_home <- Sys.getenv("git.home")

  ## needed if using gitgadget with both native and docker version of Rstudio
  if (git_home != "" && !file.exists(git_home)) {
    git_alt <- file.path(homedir, basename(git_home))
    if (file.exists(git_alt)) {
      git_home <- git_alt
    } else {
      git_alt <- file.path(homedir, basename(dirname(git_home)), basename(git_home))
      if (file.exists(git_alt)) {
        git_home <- git_alt
      } else {
        git_home <- ""
      }
    }
  }

  ## setting up volumes for shinyFiles
  if (git_home != "") {
    gg_volumes <- setNames(c(git_home, gg_volumes), c(basename(git_home), names(gg_volumes)))
  }

  if (rstudioapi::isAvailable()) {
    projdir <- basedir <- rstudioapi::getActiveProject()
    if (rstudioapi::getVersion() < "1.1") stop("GitGadget requires Rstudio version 1.1 or later")
  }

  ## setting up volumes for shinyFiles
  if (length(projdir) == 0) {
    if (git_home == "") {
      projdir <- basedir <- file.path(homedir, "GitLab")
    } else {
      projdir <- basedir <- git_home
    }
  } else {
    gg_volumes <- setNames(c(projdir, gg_volumes), c(basename(projdir), names(gg_volumes)))
  }

  ui <- miniPage(
    miniTitleBar(
      paste0("GITGADGET (", packageVersion("gitgadget"), ")"),
      right = miniTitleBarButton("done", "Done", primary = TRUE),
      left = miniTitleBarButton("help", "Help", primary = FALSE)
    ),
    includeCSS(file.path(system.file("app", package = "gitgadget"), "www/style.css")),
    miniTabstripPanel(id = "tabs",
      miniTabPanel("Introduce", value = "intro", icon = icon("hand-paper-o"),
        miniContentPanel(
          HTML("<h2>Introduce yourself to git</h2>"),
          textInput("intro_user_name","User name:", value = Sys.getenv("git.user"), placeholder = "Provide GitLab/GitHub user name"),
          textInput("intro_user_email","User email:", value = Sys.getenv("git.email"), placeholder = "Provide GitLab/GitHub user email"),
          fillRow(height = "70px", width = "475px",
            passwordInput("intro_token_gl","GitLab token:", value = Sys.getenv("git.token")),
            actionButton("intro_token_gl_get", "Create", title = "Browse to GitLab to get a PAT", style = "margin-top: 25px;")
          ),
          # fillRow(height = "70px", width = "475px",
          #   passwordInput("intro_token_gh","GitHub token:", value = Sys.getenv("GITHUB_PAT")),
          #   actionButton("intro_token_gh_get", "Create", title = "Browse to GitHub to get a PAT", style = "margin-top: 25px;")
          # ),
          radioButtons("intro_user_type", "User type:", c("student","faculty"), Sys.getenv("git.user.type", "student"), inline = TRUE),
          fillRow(height = "70px", width = "475px",
            uiOutput("ui_intro_git_home"),
            shinyFiles::shinyDirButton("intro_git_home_open", "Open", title = "Browse and select a local directory", style = "margin-top: 25px;")
         ),
          uiOutput("ui_intro_buttons"),
          hr(),
          verbatimTextOutput("introduce_output")
        )
      ),
      miniTabPanel("Create", value = "create", icon = icon("git"),
        miniContentPanel(
          HTML("<h2>Create a repo on GitLab or GitHub</h2>"),
          selectInput("create_remote", NULL, choices = "GitLab", selected = "GitLab"),
          # selectInput("create_remote", NULL, choices = c("GitLab", "GitHub"), selected = "GitLab"),
          conditionalPanel("input.create_remote == 'GitLab'",
            textInput("create_server","API server:", value = Sys.getenv("git.server", "https://gitlab.com/api/v4/"))
          ),
          radioButtons("create_ssh", "Authentication type:", c("ssh","https"), "ssh", inline = TRUE),
          fillRow(height = "70px", width = "300px",
            textInput("create_user_name", "User name:", value = Sys.getenv("git.user")),
            # passwordInput("create_token", "Token:", value = Sys.getenv("git.token"))
            uiOutput("ui_create_token")
          ),
          conditionalPanel("input.create_remote == 'GitLab'",
            fillRow(height = "70px", width = "300px",
              textInput("create_group","Group name:", value = Sys.getenv("git.group")),
              uiOutput("ui_create_pre")
            )
          ),
          fillRow(height = "70px", width = "475px",
            uiOutput("ui_create_directory"),
            shinyFiles::shinyDirButton("create_directory_find", "Open", title = "Browse and select a local directory", style = "margin-top: 25px;")
          ),
          conditionalPanel("input.intro_user_type == 'faculty' && input.create_remote == 'GitLab'",
            fillRow(height = "70px", width = "475px",
                uiOutput("ui_create_user_file"),
                shinyFiles::shinyFilesButton("create_file_find", "Open", multiple = FALSE, title = "Browse and select a CSV file with student id and token information. Used for assignment management by instructors")
            ),
            conditionalPanel("input.create_user_file != ''",
              actionButton("create_check_tokens", "Check tokens", title = "Check student token information on GitLab"),
              radioButtons("create_type", "Assignment type:", c("individual","team"), "individual", inline = TRUE)
            )
          ),
          HTML("<h4>Remove existing remote repo local .git directory</h4>"),
          actionButton("remove_remote_show", "Remove remote", title = "Remove remote repo if present", class = "btn-danger"),
          actionButton("remove_git_show", "Remove .git", title = "Remove local .git directory if present", class = "btn-danger"),
          HTML("<h4>Create local .git and remote repo</h4>"),
          actionButton("create", "Create", title = "Create a new repo using the gitlab API"),
          hr(),
          verbatimTextOutput("create_output")
        )
      ),
      miniTabPanel("Clone", value = "clone", icon = icon("clone"),
        miniContentPanel(
          HTML("<h2>Clone a repo</h2>"),
          textInput("clone_from","Repo to clone from remote git server:", placeholder = "Provide https or ssh link to repo", value = ""),
          fillRow(height = "70px", width = "475px",
            uiOutput("ui_clone_into"),
            # actionButton("clone_into_open", "Open", title = "Browse and select a local directory", style = "margin-top: 25px;")
            shinyFiles::shinyDirButton("clone_into_open", "Open", title = "Browse and select a local directory", style = "margin-top: 25px;")
          ),

          textInput("clone_to","Custom directory to clone repo into:", placeholder = "Use for custom directory only", value = ""),
          radioButtons("clone_proj", "Open project in:", c("current session" = "curr", "new session" = "new"), "new", inline = TRUE),
          actionButton("clone", "Clone", title = "Clone a repo from, e.g., github or gitlab over HTTPS or SSH. By default, the name of the remote repo and the local clone will be the same. To change the name for the local repo to create, provide an alternative in the 'Custom directory' input\n\nGit command:\ngit clone <remote url>\n\nNote: To activate a credential helper the first time you clone a (private) repo from, e.g., github or gitlab, run 'git clone <remote url>' from the command line"),
          hr(),
          verbatimTextOutput("clone_output")
        )
      ),
      miniTabPanel("Branch", value = "branch", icon = icon("code-fork"),
        miniContentPanel(
          br(),
          HTML("<h4>Create a new branch</h4>"),
          textInput("branch_create_name", NULL, value = "", placeholder = "Provide a name for the new branch"),
          actionButton("branch_create", "Create local", title = "Create a new local branch based on the currently active branch. Click the refresh button in Rstudio's Git tab to view the updated list of branches\n\nGit command:\ngit branch -b <branch>"),
          actionButton("branch_link", "Link remote", title = "Link the local branch to a (new) remote branch\n\nGit command:\ngit push --set-upstream origin <branch>"),
          actionButton("branch_create_from_mr", "Create from MR", title = "Create a local branch from a Merge/Pull request\n"),
          HTML("<h4>Check out a branch</h4>"),
          fillRow(height = "40px", width = "420px",
            uiOutput("ui_branch_checkout_name"),
            actionButton("branch_checkout", "Check out", title = "Check out a branch\n\nGit command:\ngit checkout <branch>")
          ),
          HTML("<h4>Merge branches</h4>"),
          uiOutput("ui_branch_merge_branches"),
          actionButton("branch_merge", "Merge branches", title = "Merge the 'from' branch into the 'into' branch\n\nGit commands:\ngit checkout <from branch>\ngit merge <into branch>"),
          actionButton("branch_abort", "Abort merge", title = "Abort the merge in progress\n\nGit command:\ngit merge --abort"),
          HTML("<h4>Delete existing branch(es)</h4>"),
          uiOutput("ui_branch_delete_name"),
          actionButton("branch_unlink", "Unlink remote", title = "Unlink the local and the remote branch(es). The remote branch(es) will not be deleted\n\nGit command:\ngit branch -d -r origin/<branch>"),
          actionButton("branch_delete", "Delete local", title = "Remove the local branch(es)\n\nGit commands:\ngit checkout master\ngit branch -D <branch>"),
          br(), br()
        )
      ),
      miniTabPanel("Sync", value = "sync", icon = icon("refresh"),
        miniContentPanel(
          HTML("<h2>Commit changes locally</h2>"),
          textAreaInput("sync_commit_message", "Commit message:", rows = 2, resize = "both", value = "", placeholder = "Provide a commit message that describes the changes you made to the repo"),
          actionButton("sync_commit", "Commit", title = "Commit all updated files to the local repo\n\nGit commands:\ngit add .\ngit commit -m \"Commit message\""),
          actionButton("sync_undo_commit_show", "Undo", class = "btn-danger", title = "Undo the latest local commit\n\nGit command:\ngit reset ~HEAD"),
          HTML("<h2>Sync with remote</h2>"),
          actionButton("sync_pull", "Pull", title = "Pull updates from remote repo\n\nGit command: git pull"),
          actionButton("sync_push", "Push", title = "Push all commited updates to the remote repo\n\nGit command: git push"),
          actionButton("sync_reset_show", "Reset", class = "btn-danger", title = "Completely reset local repo to remote master branch\n\nGit commands:\ngit fetch --all\ngit reset --hard origin/master"),
          HTML("<h2>Sync a fork</h2>"),
          uiOutput("ui_sync_from"),
          actionButton("sync", "Sync", title = "Link the local repo with the original from which it was forked and pull an updated copy into an upstream/ branch\n\nGit commands:\ngit remote add upstream <remote url>\ngit fetch upstream"),
          actionButton("sync_merge", "Merge", title = "Merge the upstream/ branch(es) from the original with the local branch(es)\n\nGit commands:\ngit checkout master\ngit merge upstream/master"),
          actionButton("synch_abort", "Abort merge", title = "Abort the merge in progress\n\nGit command:\ngit merge --abort"),
          actionButton("sync_unlink", "Unlink", title = "Remove a link between a local repo and the original from which it was forked\n\nGit command:\ngit remote remove upstream"),
          hr(),
          verbatimTextOutput("sync_output")
        )
      ),
      miniTabPanel("Collect", value = "collect", icon = icon("cloud-download"),
        miniContentPanel(
          conditionalPanel("input.intro_user_type == 'faculty'",
            HTML("<h2>Collect assignments</h2>"),
            passwordInput("collect_token","Token:", value = Sys.getenv("git.token")),
            # fillRow(height = "70px", width = "500px",
            #   textInput("collect_group","Group name:", value = Sys.getenv("git.group", ""), placeholder = "Enter group name on GitLab"),
            #   actionButton("collect_list", "List", title = "Collect the list of assignments associated with the specified group. Used for assignment management by instructors")
            # ),
            uiOutput("ui_collect_assignment"),
            conditionalPanel("input.collect_assignment != undefined && input.collect_assignment != null &&
                              input.collect_assignment.length > 0",
              fillRow(height = "70px", width = "475px",
                uiOutput("ui_collect_user_file"),
                shinyFiles::shinyFilesButton("collect_file_find", "Open", multiple = FALSE, title = "Browse and select a CSV file with student id and token information. Used for assignment management by instructors")
              ),
              textInput("collect_server","API server:", value = Sys.getenv("git.server", "https://gitlab.com/api/v4/")),
              radioButtons("collect_type", "Assignment type:", c("individual","team"), "individual", inline = TRUE),
              actionButton("collect", "Collect", title = "Create merge requests from all student forks using the gitlab API. Used for assignment management by instructors"),
              actionButton("collect_fetch", "Fetch", title = "Create local branches from all merge requests and link them to (new) remote branches. Used for assignment management by instructors")
            ),
            hr(),
            verbatimTextOutput("collect_output")
          ),
          conditionalPanel("input.intro_user_type != 'faculty'",
            HTML("<h2>Used only for assignment management by faculty</h2>"),
            HTML("<h2>Change setting to 'faculty' in the Introduction tab if needed</h2>")
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

    observeEvent(input$edit_r_files, {
      usethis::edit_r_environ()
      usethis::edit_r_profile()
    })

    if (!is.null(getOption("git.user"))) {
      showModal(
        modalDialog(title = "Move git settings to .Renviron",
          span("All git related settings should be moved from .Rprofile to .Renviron. Click on the
          \"edit .R files\" button to open both .Rprofile and .Renviron. Move all lines that
          contain 'git.' out of .Rprofile and to the .Renviron file. Then remove 'options' from the
          new lines in .Renviron. For example: \"options(git.user = 'abc123')\" in .Rprofile should
          be \"git.user = 'abc123'\" in .Renviron."),
          br(), br(),
          span("If you run into any problems, please
          post an issue to"),
          HTML("<a href='https://github.com/vnijs/gitgadget/issues' target='_blank'>https://github.com/vnijs/gitgadget/issues</a>"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("edit_r_files", "Edit .R files")
          )
        )
      )
    }

    observeEvent(input$help, {
      viewer <- getOption("viewer", default = browseURL)
      viewer("https://github.com/vnijs/gitgadget")
    })

    observeEvent(grep("([\\]+)|([/]{2,})", input$intro_git_home), {
      if (!is_empty(input$intro_git_home))
        updateTextInput(session = session, "intro_git_home", value = gsub("([\\]+)|([/]{2,})","/", input$intro_git_home))
    })

    observeEvent(grep("([\\]+)|([/]{2,})", input$create_directory), {
      updateTextInput(session = session, "create_directory", value = gsub("([\\]+)|([/]{2,})","/", input$create_directory))
    })

    observeEvent(grep("([\\]+)|([/]{2,})", input$create_user_file), {
      updateTextInput(session = session, "create_user_file", value = gsub("([\\]+)|([/]{2,})","/", input$create_user_file))
    })

    observeEvent(grep("([\\]+)|([/]{2,})", input$clone_into), {
      updateTextInput(session = session, "clone_into", value = gsub("([\\]+)|([/]{2,})","/", input$clone_into))
    })

    observeEvent(grep("([\\]+)|([/]{2,})", input$clone_to), {
      updateTextInput(session = session, "clone_to", value = gsub("([\\]+)|([/]{2,})","/", input$clone_to))
    })

    observeEvent(grep("([\\]+)|([/]{2,})", input$collect_user_file), {
      updateTextInput(session = session, "collect_user_file", value = gsub("([\\]+)|([/]{2,})","/", input$collect_user_file))
    })

    observeEvent(input$intro_token_gl_get, {
      utils::browseURL("https://gitlab.com/profile/personal_access_tokens")
    })

    observeEvent(input$intro_token_gh_get, {
      ## based on usethis::browse_github_pat
      utils::browseURL("https://github.com/settings/tokens/new?scopes=repo,gist&description=R:GITHUB_PAT")
    })

    observeEvent(input$intro_git, {

      if (!is_empty(input$intro_user_name)) {
        cmd <- paste("git config --global --replace-all user.name", input$intro_user_name)
        resp <- system(cmd, intern = TRUE)
        cat("Used:", cmd, "\n")

        ## based on https://twitter.com/joshua_ulrich/status/1115578106105028610
        ## avoid "a ton of confusing merge commits on master"
        # system("git config --global branch.autosetuprebase always")

        renvir <- file.path(renvirdir, ".Renviron")
        if (file.exists(renvir)) {
          readLines(renvir) %>%
            .[!grepl("git.user\\s*=", .)] %>%
            paste0(collapse = "\n") %>%
            paste0(., "\ngit.user = \"", input$intro_user_name, "\"\n") %>%
            cat(file = renvir)
        } else {
          paste0("git.user = \"", input$intro_user_name, "\"\n") %>% cat(file = renvir)
        }
      }

      if (!is_empty(input$intro_user_email)) {
        cmd <- paste("git config --global --replace-all user.email", input$intro_user_email)
        resp <- system(cmd, intern = TRUE)
        cat("Used:", cmd, "\n")

        renvir <- file.path(renvirdir, ".Renviron")
        if (file.exists(renvir)) {
          readLines(renvir) %>%
            .[!grepl("git.email\\s*=",.)] %>%
            paste0(collapse = "\n") %>%
            paste0(., "\ngit.email = \"", input$intro_user_email, "\"\n") %>%
            cat(file = renvir)
        } else {
          paste0("git.email = \"", input$intro_user_email, "\"\n") %>% cat(file = renvir)
        }
      }

      if (!is_empty(input$intro_token_gl)) {
        renvir <- file.path(renvirdir, ".Renviron")
        if (file.exists(renvir)) {
          readLines(renvir) %>%
            .[!grepl("git.token\\s*=",.)] %>%
            paste0(collapse = "\n") %>%
            paste0(., "\ngit.token = \"", input$intro_token_gl, "\"\n") %>%
            cat(file = renvir)
        }
      }

      if (!is_empty(input$intro_token_gh)) {
        renvir <- file.path(renvirdir, ".Renviron")
        if (file.exists(renvir)) {
          readLines(renvir) %>%
            .[!grepl("GITHUB_PAT\\s*=",.)] %>%
            paste0(collapse = "\n") %>%
            paste0(., "\nGITHUB_PAT = \"", input$intro_token_gh, "\"\n") %>%
            cat(file = renvir)
        }
      }

      ## set git.home option
      git_home <- gsub("^\\s+|\\s+$", "", input$intro_git_home)
      if (!is_empty(git_home) && git_home != Sys.getenv("git.home")) {
        if (!dir.exists(git_home)) dir.create(git_home, recursive = TRUE)
        renvir <- file.path(renvirdir, ".Renviron")
        if (file.exists(renvir)) {
          readLines(renvir) %>%
            .[!grepl("git.home\\s*=",.)] %>%
            paste0(collapse = "\n") %>%
            paste0(., "\ngit.home = \"", git_home, "\"\n") %>%
            cat(file = renvir)
        } else {
          paste0("git.home = \"", git_home, "\")\n") %>% cat(file = renvir)
        }
        cat("Updated git home in .Renviron. Restart Rstudio to see the changes\n")
      }

      ## set git.user.type option
      git_user_type <- input$intro_user_type
      if (!is_empty(git_user_type) && git_user_type != Sys.getenv("git.user.type", "student")) {
        # renvir <- file.path(renvirdir, ".Rprofile")
        renvir <- file.path(renvirdir, ".Renviron")
        if (file.exists(renvir)) {
          readLines(renvir) %>%
            # .[!grepl("options\\(git.user.type\\s*=",.)] %>%
            .[!grepl("git.user.type\\s*=",.)] %>%
            paste0(collapse = "\n") %>%
            # paste0(., "\noptions(git.user.type = \"", git_user_type, "\")\n") %>%
            paste0(., "\ngit.user.type = \"", git_user_type, "\"\n") %>%
            cat(file = renvir)
        } else {
          # paste0("options(git.user.type = \"", git_user_type, "\")\n") %>% cat(file = renvir)
          paste0("git.user.type = \"", git_user_type, "\"\n") %>% cat(file = renvir)
        }
        # cat("Updated user type in .Rprofile. Restart Rstudio to see the changes\n")
        cat("Updated user type in .Renviron. Restart Rstudio to see the changes\n")
      }

      message("Introduction attempt completed. Check console for messages\n")
    })

    .ssh_exists <- reactive({
      ## update after pressing the intro_ssh button
      input$intro_ssh
      input$intro_keyname

      keyname <- ifelse(is_empty(input$intro_keyname), "id_rsa", input$intro_keyname)

     .ssh_path <- file.path(homedir, ".ssh", paste0(keyname, ".pub"))

      if (file.exists(.ssh_path)) .ssh_path else ""
    })

    shinyFiles::shinyDirChoose(input, "intro_git_home_open", roots = gg_volumes)

    output$ui_intro_git_home <- renderUI({
      init <- Sys.getenv("git.home", basedir)
      if (!is.integer(input$intro_git_home_open)) {
        init <- shinyFiles::parseDirPath(gg_volumes, input$intro_git_home_open)
      }

      textInput("intro_git_home","Base directory to clone repos into:",
        value = init,
        placeholder = "Choose directory to store repos"
      )
    })


    output$ui_intro_buttons <- renderUI({
      ## intend to remove SSH functionality below
      tagList(
        fillRow(height = "70px", width = "300px",
          textInput("intro_keyname","Key name:", value = "id_rsa"),
          textInput("intro_passphrase","Pass-phrase:", value = "")
        ),
        actionButton("intro_git", "Introduce", title = "Introduce yourself to git\n\nGit commands:\ngit config --global --replace-all user.name <username>\ngit config --global --replace-all user.email <useremail>\ngit config --global credential.helper <credential helper>"),
        actionButton("intro_ssh", "SSH key", title = "Create an SSH key and copy the public-key to the clipboard"),
        actionButton("intro_restart", "Restart", title = "Restart GitGadget")
      )

     # actionButton("intro_git", "Introduce", title = "Introduce yourself to git\n\nGit commands:\ngit config --global --replace-all user.name <username>\ngit config --global --replace-all user.email <useremail>\ngit config --global credential.helper <credential helper>")
    })

    intro_ssh <- eventReactive(input$intro_ssh, {
      if (os_type != "Windows") {
        email <- system("git config --global --list", intern = TRUE) %>%
          .[grepl("^user.email",.)] %>%
          gsub("user.email=","",.)

        if (length(email) == 0) {
          cat("Make sure you have an email address and user name set before generating the SSH key")
          return(invisible())
        }

        keyname <- ifelse(is_empty(input$intro_keyname), "id_rsa", input$intro_keyname)
        ssh_dir <- file.path(homedir, ".ssh")
        if (is_empty(.ssh_exists())) {
          if (!dir.exists(ssh_dir)) dir.create(ssh_dir)
          paste0("ssh-keygen -t rsa -b 4096 -C \"", email, "\" -f ", ssh_dir, "/", keyname," -N '", input$intro_passphrase, "'") %>%
           system(.)

          key <- suppressWarnings(readLines(paste0(ssh_dir, "/", keyname, ".pub")))

          if (os_type == "Darwin") {ls

            out <- pipe("pbcopy")
            cat(key, file = out)
            close(out)
            cat("\nYour new public SSH key has been copied to the clipboard. Navigate to https://gitlab.com/profile/keys in your browser, paste the key into the 'Key' text input on gitlab, and click 'Add key'\n")
          } else {
            cat(paste0("\n", key))
            cat("\n\nCopy the new public SSH key to https://gitlab.com/profile/keys. Paste the key into the 'Key' text input on gitlab, and click 'Add key'\n")
          }
        } else {
          key <- suppressWarnings(readLines(.ssh_exists()))
          if (os_type == "Darwin") {
            out <- pipe("pbcopy")
            cat(key, file = out)
            close(out)
            cat("\nYour public SSH key has been copied to the clipboard. Navigate to https://gitlab.com/profile/keys in your browser, paste the key into the 'Key' text input on gitlab, and click 'Add key'\n")
          } else {
            cat(paste0("\n", key))
            cat("\n\nCopy the public SSH key to https://gitlab.com/profile/keys. Paste the key into the 'Key' text input on gitlab, and click 'Add key'\n")
          }
        }

        ## set environment variable
        Sys.setenv(GIT_SSH_COMMAND=paste0("'ssh -i '", ssh_dir, "/", keyname))

        if (file.exists(file.path(ssh_dir, "known_hosts"))) {
          if (!any(grepl("gitlab\\.com", readLines(file.path(ssh_dir, "known_hosts"))))) {
            system(paste0("ssh-keyscan -t rsa,dsa gitlab.com >> ", ssh_dir, "/known_hosts"))
          }
          if (!any(grepl("github\\.com", readLines(file.path(ssh_dir, "known_hosts"))))) {
            system(paste0("ssh-keyscan -t rsa,dsa github.com >> ", ssh_dir, "/known_hosts"))
          }
        } else {
          system(paste0("ssh-keyscan -t rsa,dsa gitlab.com >> ", ssh_dir, "/known_hosts"))
          system(paste0("ssh-keyscan -t rsa,dsa github.com >> ", ssh_dir, "/known_hosts"))
        }

        if (keyname != "id_rsa") {
          cat("\nYou will also need to add the lines below to ~/.ssh/config\n")
          cat("\nHost gitlab.com\n")
          cat(paste0("    IdentityFile ~/.ssh/", keyname))
          rstudioapi::navigateToFile("~/.ssh/config", line = 1000L)
        }

        browseURL("https://gitlab.com/profile/keys")
        # browseURL("https://github.com/settings/keys")

      # } else if (os_type == "Windows") {
      } else {
        if (!is_empty(.ssh_exists())) {
          ssh_dir <- file.path(homedir, ".ssh")
          if (file.exists(file.path(ssh_dir, "known_hosts"))) {
            if (!any(grepl("gitlab\\.com", readLines(file.path(ssh_dir, "known_hosts"))))) {
              system(paste0("ssh-keyscan -t rsa,dsa gitlab.com >> ", ssh_dir, "/known_hosts"))
            }
            if (!any(grepl("github\\.com", readLines(file.path(ssh_dir, "known_hosts"))))) {
              system(paste0("ssh-keyscan -t rsa,dsa github.com >> ", ssh_dir, "/known_hosts"))
            }
          } else {
            system(paste0("ssh-keyscan -t rsa,dsa gitlab.com >> ", ssh_dir, "/known_hosts"))
            system(paste0("ssh-keyscan -t rsa,dsa github.com >> ", ssh_dir, "/known_hosts"))
          }
          key <- suppressWarnings(readLines(.ssh_exists()))
          cat(key, file = "clipboard")
          cat("\nYour public SSH key has been copied to the clipboard. Navigate to https://gitlab.com/profile/keys in your browser, paste the key into the 'Key' text input on gitlab, and click 'Add key'\n")
       } else {
          cat("\nSSH keys cannot be generated from Git Gadget on Windows. In RStudio go to Tools > Global Options and select Git/SVN. Click 'Create RSA Key' and then 'View public key'. Copy the key to the clipboard, navigate to https://gitlab.com/profile/keys in your browser, paste the key into the 'Key' text input on gitlab, and click 'Add key'\n")
        }
      }
    })

    observeEvent(input$intro_restart, {
      ## https://github.com/rstudio/rstudioapi/issues/111
      stopApp(cat("\nUse Session > Restart R to update your settings in memory.\nThen start Git Gadget again to clone, create, etc.\n\n"))
      # cmd <- "gitgadget:::gitgadget()"
      # ret <- .rs.restartR(cmd)
      # rstudioapi::restartSession(cmd)
    })

    output$introduce_output <- renderPrint({
      input$intro_git
      if (file.exists(file.path(find_home(), ".gitconfig"))) {
        ret <- system("git config --global --list", intern = TRUE) %>%
          .[grepl("^user",.)]
      } else {
        ret <- c()
      }
      if (length(ret) == 0) {
        cat("No user information set. Enter a user name and email and click the 'Introduce' button\n\nSet user.name : git config --global user.name 'Your Name'\nSet user.email: git config --global user.email 'myemail@gmail.com'\n")
        return(invisible())
      } else {

        ptext <- if (not_pressed(input$intro_git)) "Checking credentials" else "Working on introduction"
        withProgress(message = ptext, value = 0, style = "old", {

          crh <- system("git config --global --list", intern = TRUE) %>%
            .[grepl("^credential.helper",.)]

          if (length(crh) == 0) {
            if (os_type == "Darwin") {
              system("git config --global credential.helper osxkeychain")
            } else if (os_type == "Windows") {
              system("git config --global credential.helper wincred")
            } else {
              system("git config --global credential.helper 'cache --timeout=32000000'")
            }

            crh <- system("git config --global --list", intern = TRUE) %>%
              .[grepl("^credential.helper",.)]
          }
        })

        if (length(crh) == 0) {
          cat("\nSetting up credential help failed. Go to http://happygitwithr.com/credential-caching.html in your browser for additional suggestions\n")
          crh <- ""
        }

        paste(c(ret, crh), collapse = "\n") %>%
          paste0("Show settings: git config --global --list\n\n", ., "\n") %>%
          paste0("git.home=", Sys.getenv("git.home", "<restart Rstudio to view updates>"),"\n") %>%
          cat()
      }

      if (pressed(input$intro_ssh)) {
        intro_ssh()
      } else {
        if (!is_empty(.ssh_exists()) ) {
          cat("\nSSH keys seem to exist on your system. Click the 'SSH key' button to copy them to the clipboard")
        } else {
          cat("\nNo SSH keys seem to exist on your system. Click the 'SSH key' button to generate them")
        }
      }

    })

    output$ui_create_token <- renderUI({
      req(input$create_remote)
      if (input$create_remote == "GitLab") {
        passwordInput("create_token", "GitLab token:", value = Sys.getenv("git.token"))
      } else {
        passwordInput("create_token", "GitHub token:", value = Sys.getenv("GITHUB_PAT"))
      }
    })

    output$ui_create_pre <- renderUI({
      req(input$create_group)
      # textInput("create_pre","Prefix:", value = Sys.getenv("git.prefix", paste0(input$create_group,"-")))
      textInput("create_pre","Prefix:", value = Sys.getenv("git.prefix"))
    })

    shinyFiles::shinyDirChoose(input, "create_directory_find", roots = gg_volumes)

    output$ui_create_directory <- renderUI({
      init <- projdir
      if (!is.integer(input$create_directory_find)) {
        init <- shinyFiles::parseDirPath(gg_volumes, input$create_directory_find)
      }
      textInput("create_directory","Local directory:", value = init, placeholder = "Base directory for the git repo")
    })

    create_uploadfile <- shinyFiles::shinyFileChoose(
      input = input,
      id = "create_file_find",
      roots = gg_volumes,
      session = session,
      filetype = "csv"
    )

    output$ui_create_user_file <- renderUI({
      init <- Sys.getenv("git.userfile")
      if (!is.integer(input$create_file_find)) {
        chosen <- shinyFiles::parseFilePaths(gg_volumes, input$create_file_find)
        if (nrow(chosen) > 0) {
          init <- chosen$datapath
        }
      }
      textInput("create_user_file","Upload file with student tokens:", value = init, placeholder = "Open student CSV file")
    })

    observeEvent(input$create_check_tokens, {
      withProgress(message = "Checking student tokens on GitLab", value = 0, style = "old", {
        check_tokens(input$create_user_file)
      })
      message("\nToken check completed. Check the console for messages\n")
    })

    ## Show remove_git modal when button is clicked.
    observeEvent(input$remove_git_show, {
      ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
      showModal(
        modalDialog(title = "Remove local .git directory",
          span("Are you sure you want to remove the local .git directory? Use only if you want to destroy all local history and restart!"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("remove_git", "Remove .git", title = "Remove previous .git directory if present", class = "btn-danger")
          )
        )
      )
    })

    remove_git <- observeEvent(input$remove_git, {
      removeModal()
      if (!dir.exists(input$create_directory)) {
        cat("\nThe specified directory does not exist\n")
        return(invisible())
      }

      if (!dir.exists(file.path(input$create_directory, ".git"))) {
        cat("\nNo .git directory found\n")
      } else {
        cat("\nA .git directory was found and removed\n")
        unlink(file.path(input$create_directory, ".git"), recursive = TRUE, force = TRUE)
      }
    })

    ## Show remove_git modal when button is clicked.
    observeEvent(input$remove_remote_show, {
      req(input$create_remote)
      ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
      if (input$create_remote == "GitLab") {
        showModal(
          modalDialog(title = "Remove remote GitLab repo",
            span("Are you sure you want to remove the remote repo on GitLab? Use only if you want to destroy all remote files and history and restart!"),
            footer = tagList(
              with(tags, table(
                align = "right",
                td(modalButton("Cancel")),
                td(conditionalPanel("input.create_user_file != ''",
                  actionButton("remove_forks", "Remove forks", title = "Remove forks from current repo created for students", class = "btn-danger")
                )),
                td(actionButton("remove_gitlab", "Remove remote", title = "Remove previous remote repo if present", class = "btn-danger"))
              ))
            )
          )
        )
      } else {
        showModal(
          modalDialog(title = "Remove remote GitHub repo",
            span("This feature has not yet been implemented for GitHub repos")
            # span("Are you sure you want to remove the remote repo on GitLab? Use only if you want to destroy all remote files and history and restart!"),
            # footer = tagList(
            #   with(tags, table(
            #     align = "right",
            #     td(modalButton("Cancel")),
            #     td(conditionalPanel("input.create_user_file != ''",
            #       actionButton("remove_forks", "Remove forks", title = "Remove forks from current repo created for students", class = "btn-danger")
            #     )),
            #     td(actionButton("remove_gitlab", "Remove remote", title = "Remove previous remote repo if present", class = "btn-danger"))
            #   ))
            # )
          )
        )

      }
    })

    remove_gitlab <- observeEvent(input$remove_gitlab, {
      removeModal()
      if (is_empty(input$create_token)) {
        cat("Token required to remove remote repo")
        return(invisible())
      }

      if (!dir.exists(input$create_directory)) {
        cat("The specified directory does not exist. Create the directory and try again")
        return(invisible())
      }

      withProgress(message = "Removing remote repo", value = 0, style = "old", {
        create_group_lc <- tolower(input$create_group) %>%
          {ifelse(is_empty(.), Sys.getenv("git.user", ""), .)}

        if (is_empty(create_group_lc)) {
          cat("No group or user name provided. Please provide this information and try again")
          return(invisible())
        }

        create_pre_lc <- tolower(input$create_pre)
        repo <- basename(input$create_directory)

        cat("Removing remote repo ...\n")

        id <- projID(paste0(create_group_lc, "/", create_pre_lc, repo), input$create_token, input$create_server)
        if (id$status == "OKAY") {
          resp <- remove_project(input$create_token, id$project_id, input$create_server)
          if (checkerr(resp$status_code))
            cat("\nRemote repo successfully removed\n")
          else
            cat("\nProblem removing remote repo. See the console for messages\n")
        } else {
          cat("\nNo remote repo found\n")
        }
      })
    })

    remove_forks <- observeEvent(input$remove_forks, {
      removeModal()
      if (is_empty(input$create_token)) {
        cat("Token required to remove student forks")
        return(invisible())
      }

      if (!dir.exists(input$create_directory)) {
        cat("The specified directory does not exist. Create the directory and try again")
        return(invisible())
      }

      withProgress(message = "Removing student forks", value = 0, style = "old", {
        create_group_lc <- tolower(input$create_group)
        create_pre_lc <- tolower(input$create_pre)
        repo <- basename(input$create_directory)

        message("Removing student forks ...\n")

        students <- read.csv(input$create_user_file, stringsAsFactors = FALSE)
        for (i in seq_len(nrow(students))) {
          id <- projID(paste0(students[i, "userid"], "/", create_pre_lc, repo), students[i, "token"], "https://gitlab.com/api/v4/")
          if (id$status == "OKAY") {
            remove_project(students[i, "token"], id$project_id, "https://gitlab.com/api/v4/")
            message(paste0("Project fork ", id$project_id, " removed for ", students[i, "userid"], " in ", students[i, "team"]))
          }
        }
        message("\nFork removal process complete. Check the console for messages\n")
      })
    })

    create <- eventReactive(input$create, {

      if (is_empty(input$create_user_name) || is_empty(input$create_token)) {
        cat("Username and token required to create a new repo")
        return(invisible())
      }

      if (!dir.exists(input$create_directory)) {
        cat("The specified directory does not exist. Create the directory and try again")
        return(invisible())
      }

      repo <- basename(input$create_directory)

      if (grepl("[^A-z0-9_\\.\\-]", repo)) {
        cat("The repo name cannot contain spaces or symbols. Please change the name and try again")
        return(invisible())
      }

      directory <- dirname(input$create_directory)

      if (input$create_remote == "GitLab") {
        withProgress(message = "Creating and forking repo", value = 0, style = "old", {

          create_group_lc <- tolower(input$create_group)
          create_pre_lc <- tolower(input$create_pre)

          if (create_group_lc != "" && create_group_lc != Sys.getenv("git.user")) {
            cat("Creating group ...\n")
            create_group(
              input$create_token, create_group_lc, input$create_user_file,
              permission = 20, server = input$create_server
            )
          }

          cat("Creating repo ...\n")
          create_repo(
            input$create_user_name, input$create_token, repo, directory, create_group_lc,
            pre = create_pre_lc, ssh = ifelse(isTRUE(input$create_ssh == "ssh"), TRUE, FALSE), server = input$create_server
          )
          if (!is_empty(input$create_user_file)) {
            if (is_empty(input$create_group)) {
              cat("A groupname is required when assigning work.\n")
              cat("Add a groupname and try again ...\n")
            } else {
              cat("Assigning work ...\n")
              assign_work(
                input$create_token, create_group_lc, repo,
                input$create_user_file, type = input$create_type, pre = create_pre_lc,
                server = input$create_server
              )
            }
          }
        })
      } else {
        cat("Creating repo ...\n")
        curr <- setwd(input$create_directory)
        on.exit(setwd(curr))
        usethis::use_git()
        usethis::use_github(protocol = "https", auth_token = input$create_token)
      }

      message("\nCreate process complete. Check the console for messages\n")
    })

    output$create_output <- renderPrint({
      input$create  ## creating a dependency
      ret <- create()
    })

    clone <- eventReactive(input$clone, {
      to_return <- c()
      if (input$clone_from != "") {
        if (input$clone_into != "") {
          owd <- setwd(input$clone_into)
          on.exit(setwd(owd))
        }
        clone_from <- cmd_from <- input$clone_from
        cmd <- paste("git clone", clone_from)
        cmdclean <- paste("git clone", input$clone_from)

        clone_to <- input$clone_to
        if (!is_empty(clone_to)) {
          cmd <- paste(cmd, clone_to)
          cmdclean <- paste(cmdclean, clone_to)
          cloneto <- file.path(getwd(), clone_to)
        } else {
          clone_to <- file.path(getwd(), basename(clone_from) %>% tools::file_path_sans_ext())
        }
        cat("Used:", cmdclean, "\n\n")

        withProgress(message = "Cloning repo", value = 0, style = "old", {
          ret <- suppressWarnings(system(paste(cmd, "2>&1"), intern = TRUE))
          if (any(grepl("rpostback-askpass", ret)) || any(grepl("could not read Username", ret))) {
            rstudioapi::terminalActivate()
            Sys.sleep(1)
            tid <- rstudioapi::terminalVisible()
            rstudioapi::terminalSend(tid, paste("git clone", clone_from, clone_to, "\n"))
            showModal(
              modalDialog(
                title = "Provide user name and password",
                span("Provide user name and password in Rstudio > Terminal to clone from GitLab (GitHub)")
              )
            )
          } else if (any(grepl("fatal:", ret))) {
            cat(ret, sep = "\n")
          } else {
            cat(ret, sep = "\n")
            to_return <- 0
          }
        })
      }
      return(to_return)
    })

    shinyFiles::shinyDirChoose(input, "clone_into_open", roots = gg_volumes)

    output$ui_clone_into <- renderUI({
      init <- Sys.getenv("git.home", basedir)
      if (!is.integer(input$clone_into_open)) {
        init <- shinyFiles::parseDirPath(gg_volumes, input$clone_into_open)
      }

      textInput("clone_into","Base directory to clone repo into:",
        value = init,
        placeholder = "Choose directory to store repo"
      )
    })

    output$clone_output <- renderPrint({
      input$clone
      ret <- clone()
      isolate({
        if (is_empty(input$clone_from)) {
          cat("Nothing was returned. Make sure you specified a repo to clone from")
        } else if (length(ret) > 0 && ret == 0) {
          if (input$clone_to == "") {
            dir <- file.path(input$clone_into, gsub("\\.git\\s*$", "", basename(input$clone_from)))
          } else {
            dir <- file.path(input$clone_into, input$clone_to)
          }

          rproj <- list.files(path = dir, pattern = "*.Rproj")
          if (length(rproj) == 0) {
            "Version: 1.0\n\nRestoreWorkspace: No\nSaveWorkspace: No\nAlwaysSaveHistory: Default\n\nEnableCodeIndexing: Yes\nUseSpacesForTab: Yes\nNumSpacesForTab: 2\nEncoding: UTF-8\n\nRnwWeave: knitr\nLaTex: pdfLaTex\n\nAutoAppendNewline: Yes\n\nBuildType: Package\nPackageUseDevtools: Yes\nPackageInstallArgs: --no-multiarch --with-keep.source\nPackageRoxygenize: rd,collate,namespace\n" %>%
              cat(file = file.path(dir, paste0(basename(dir),".Rproj")))
          }

          gitignore <- list.files(path = dir, pattern = ".gitignore")
          if (length(gitignore) == 0)
            cat(".Rproj.user\n.Rhistory\n.RData\n.Ruserdata\n.DS_Store\n", file = file.path(dir, ".gitignore"))

          cat("Repo was sucessfully cloned into", dir)

          if (length(rproj) == 0)
            rproj <- list.files(path = dir, pattern = "*.Rproj", full.names = TRUE)[1]
          else
            rproj <- file.path(dir, rproj[1])

          rstudioapi::openProject(rproj, newSession = input$clone_proj == "new")
        } else {
          cat("There was an error cloning the repo. Check the R console for output")
        }
      })
    })

    branches <- reactive({
      input$branch_delete
      input$branch_create
      input$branch_checkout
      br <- system("git branch -a", intern = TRUE)
      brs <- attr(br, "status")
      ## need both conditions because output on windows and mac differs
      if (length(br) == 0 || (!is.null(brs) && brs == 128)) {
        c()
      } else {
        br %>% gsub("[\\* ]+", "", .) %>%
        {.[!grepl("(^master$)|(^remotes/origin/master$)|(^remotes/origin/HEAD)",.)]}
      }
    })

    observeEvent(input$branch_create, {
      if (input$branch_create_name != "") {
        withProgress(message = "Creating branch", value = 0, style = "old", {
          paste("git checkout -b", input$branch_create_name) %>%
            system(.)
        })
      }
    })

    observeEvent(input$branch_create_from_mr, {
      remote_fetch <- system("git config --get-all remote.origin.fetch", intern = TRUE)
      if (!"+refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*" %in% remote_fetch) {
        system("git config --add remote.origin.fetch +refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*")
      }
      # system("git fetch origin +refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*")
      # branches <- system("git branch ", intern = TRUE) %>% gsub("[\\* ]+", "", .)
    })

    observeEvent(input$branch_merge, {
      from <- input$branch_merge_from
      into <- input$branch_merge_into
      if (!is.null(from) || !is.null(into)) {
        withProgress(message = "Merging branch", value = 0, style = "old", {
          system(paste("git checkout", into))
          system(paste("git merge", from))
        })
      }
    })

    observeEvent(input$branch_abort, {
      system("git merge --abort")
    })

    observeEvent(input$branch_link, {
      if (input$branch_create_name != "") {
        ## would prefer to do this without 'push' -- however then I can't unlink for some reason
        paste("git push --set-upstream origin", input$branch_create_name) %>%
          system(.)
      }
    })

    observeEvent(input$branch_unlink, {
      branch <- input$branch_delete_name
      if (is_empty(branch)) input$branch_create_name
      if (!is_empty(branch)) {
        for (ib in branch) {
          system(paste0("git branch -d -r origin/", ib))
        }
      }
    })

    observeEvent(input$branch_delete, {
      if (!is.null(input$branch_delete_name)) {
        withProgress(message = "Deleting branch", value = 0, style = "old", {
          system("git checkout master")
          for (ib in input$branch_delete_name) {
            system(paste("git branch -D", ib))
          }
        })
      }
    })

    output$ui_branch_merge_branches <- renderUI({
      br <- c("master", branches()) %>% .[!grepl("origin/", .)]
      if (length(br) == 1) {
        HTML("<label>No branches available to merge</label>")
      } else {
        tagList(
          fillRow(height = "70px", width = "300px",
            selectInput("branch_merge_from", "From:", choices = br, selected = br[2]),
            selectInput("branch_merge_into", "Into:", choices = br, selected = br[1])
          )
        )
      }
    })

    rbranches <- reactive({
      input$sync; input$sync_unlink; input$branch_link; input$branch_unlink
      input$branch_create; input$branch_checkout; input$branch_delete;
      input$branch_merge; input$collect_fetch; input$collect

      br <- system("git branch --all", intern = TRUE)
      brs <- attr(br, "status")
      ## need both conditions because output on windows and mac differs
      if (length(br) == 0 || (!is.null(brs) && brs == 128)) {
        c()
      } else {
        br %>% {unique(c(.[grepl("\\* ",.)],.))} %>%
        gsub("[\\* ]+", "", .) %>%
        {.[!grepl("(^remotes/origin/master$)|(^remotes/origin/HEAD)",.)]}
      }
    })

    output$ui_branch_checkout_name <- renderUI({
      input$branch_create
      br <- rbranches()
      if (length(br) == 0) {
        invisible()
      } else {
        selectInput("branch_checkout_name", NULL, choices = br)
      }
    })

    observeEvent(input$branch_checkout, {
      if (!is.null(input$branch_checkout_name)) {
        ## based on solution #1 http://stackoverflow.com/a/29828320/1974918
        withProgress(message = "Checkout branch", value = 0, style = "old", {
          system(paste0("git checkout ", sub("remotes/origin/","",input$branch_checkout_name)))
        })
      }
    })

    output$ui_branch_delete_name <- renderUI({
      resp <- branches() %>% .[!grepl("^remotes/origin", .)]

      if (length(resp) == 0) {
        HTML("<label>No branches available to delete</label>")
      } else {
        selectizeInput("branch_delete_name",
          label = NULL,
          selected = resp[1],
          choices = resp,
          multiple = TRUE,
          options = list(placeholder = "Select branch(es) to delete", plugins = list("remove_button"))
        )
      }
    })

    remote_info <- function() {
      input$sync; input$sync_unlink; input$branch_link; input$branch_unlink
      cat("Overview of remotes:\n\n")
      cat(
        paste0(system("git remote -v", intern = TRUE), collapse = "\n") %>%
          gsub("(\t)|(  ) "," ",.)
      )
    }

    assignment_name <- function() {
      assignment <- capture.output(remote_info())[-1:-2]
      if (any(grepl("(https://github.com)|(git@github.com)", assignment))) {
        message("GitGadget does not (yet) support assignment management on GitHub.com")
        ""
      } else{
        if (any(grepl("https://gitlab.com", assignment))) {
          server <- "https://gitlab.com"
          gsub(paste0("^.*", server, "/(.*).git.*"), "\\1", assignment) %>% unique()
        } else {
          server <- "git@gitlab.com"
          gsub(paste0("^.*", server, ":(.*).git.*"), "\\1", assignment) %>% unique()
        }
      }
    }

    upstream_info <- function() {
      input$sync; input$sync_unlink
      system("git remote -v", intern = TRUE) %>%
        .[grepl("^upstream",.)] %>%
        gsub("^upstream\\s+","", .) %>%
        gsub(" \\(fetch\\)$","", .)
    }

    observeEvent(input$sync_commit, {
      cmess <- input$sync_commit_message
      if (is_empty(cmess))
        cmess <- paste0("Updates: ", Sys.time())
      else
        cmess <- gsub("\"", "'", cmess)
      withProgress(message = "Committing all changes locally", value = 0, style = "old", {
        system("git add .")
        system(paste0("git commit -m \"", cmess, "\""))
        message("\nCommit attempt completed. Check the console for messages\n")
      })
      updateTextInput(session = session, "sync_commit_message", value = "")
    })

    ## Show reset modal when button is clicked.
    observeEvent(input$sync_undo_commit_show, {
      ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
      showModal(
        modalDialog(title = "Undo latest local commit",
          span("Are you sure you want to undo the latest local commit? This will
               leave the latest changes un-staged (see Rstudio Git tab) so you can
               edit them or revert the changes"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("sync_undo_commit", "Undo", class = "btn-danger", title = "Undo the latest local commit\n\nGit command:\ngit reset HEAD~")
          )
        )
      )
    })

    observeEvent(input$sync_undo_commit, {
      withProgress(message = "Undo the latest local commit", value = 0, style = "old", {
        system("git reset HEAD~")
        message("\nPrevious changes are now unstaged. Check the Git tab in Rstudio\n")
      })
      removeModal()
      updateTextInput(
        session = session,
        "sync_commit_message",
        value = system("git log -1 --pretty=%B", intern = TRUE)
      )
    })

    observeEvent(input$sync_pull, {
      withProgress(message = "Pull changes from remote", value = 0, style = "old", {
        system("git pull")
        message("\nPull attempt completed. Check the console for messages\n")
      })
    })

    observeEvent(input$sync_push, {
      withProgress(message = "Pushing changes to remote", value = 0, style = "old", {
        system("git push")
        message("\nPush attempt completed. Check the console for messages\n")
      })
    })

    ## Show reset modal when button is clicked.
    observeEvent(input$sync_reset_show, {
      ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
      showModal(
        modalDialog(title = "Reset local repo",
          span("Are you sure you want to reset the local repo? Only use this
               option if you want to destroy the current state of the local repo
               and make a copy of the remote repo!"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("sync_reset", "Reset", class = "btn-danger", title = "Completely reset local repo to remote master branch\n\nGit commands:\ngit --fetch all\ngit reset --hard origin/master")
          )
        )
      )
    })

    observeEvent(input$sync_reset, {
      removeModal()
      withProgress(message = "Resetting local repo to remote master branch", value = 0, style = "old", {
        ## Add confirmation dialog
        system("git fetch --all")
        system("git reset --hard origin/master")
        message("\nReset attempt completed. Check the console for messages\n")
      })
    })

    observeEvent(input$sync, {
      if (!is_empty(input$sync_from)) {
        withProgress(message = "Syncing repo", value = 0, style = "old", {
          if (is_empty(upstream_info()))
            system(paste("git remote add upstream", input$sync_from))
          system("git fetch upstream")
        })
      }
    })

    observeEvent(input$sync_merge, {
      withProgress(message = "Merging synced repo", value = 0, style = "old", {
        system("git checkout master")
        system("git merge upstream/master")
      })
    })

    observeEvent(input$synch_abort, {
      system("git merge --abort")
    })

    observeEvent(input$sync_unlink, {
      system("git remote remove upstream")
    })

    output$ui_sync_from <- renderUI({
      init <- upstream_info()
      # textInput("sync_from","Sync repo with remote it was forked from:", value = ifelse(length(init) == 0, "", init[1]), placeholder = "Provide https link to original remote repo")
      textInput("sync_from","Sync repo with remote it was forked from:", value = ifelse(length(init) == 0, "", init[1]), placeholder = "Provide https or ssh link to original remote repo")
    })

    output$sync_output <- renderPrint({
      remote_info()
    })

    get_assignments <- eventReactive(input$collect_list, {

      token <- input$collect_token
      group <- input$collect_group
      server <- input$collect_server
      if (is_empty(token) || is_empty(server)) {
        message("Please specify all required inputs to retrieve available assignments")
        return(invisible())
      }

      proj <- get_allprojects(token, server = "https://gitlab.com/api/v4/", everything = TRUE)$repos
      proj <- proj[proj$namespace$name == group,]

      if (length(proj) == 0) {
        message("No assignments found for specified groupname")
        return(invisible())
      } else {
        proj[["name"]]
      }
    })

    output$ui_collect_assignment <- renderUI({

      resp <- assignment_name()
      if (length(resp) == 0) {
        HTML("<label>No assignments available for specified input values</label>")
      } else {
        selectInput("collect_assignment","Assignment name:", choices = resp)
      }

    })

   collect_file_find <- shinyFiles::shinyFileChoose(
      input = input,
      id = "collect_file_find",
      roots = gg_volumes,
      session = session,
      filetype = "csv"
    )

    ## https://gitlab.com/gitlab-org/gitlab-ce/blob/master/doc/workflow/merge_requests.md#checkout-merge-requests-locally
    ## setup a branch switcher so you can easily do "git checkout origin/merge-requests/1" for each PR
    ## can you push back tot the PR as well?
    output$ui_collect_user_file <- renderUI({
      init <- Sys.getenv("git.userfile")
      if (!is.integer(input$collect_file_find)) {
        chosen <- shinyFiles::parseFilePaths(gg_volumes, input$collect_file_find)
        if (nrow(chosen) > 0) {
          init <- chosen$datapath
        }
      }
      textInput("collect_user_file", "Upload file with student tokens:", value = init, placeholder = "Open student CSV file")
    })

    collect <- eventReactive(input$collect, {
      req(input$collect_token, input$collect_server, input$collect_user_file)

      cat("Generating merge requests ...\n")

      ## pre not used when called from the gadget interface because the full
      ## assignment name is retrieved from gitlab
      # input$collect_token, input$collect_group,
      withProgress(message = "Generating merge requests", value = 0, style = "old", {
        collect_work(
          input$collect_token, input$collect_assignment,
          input$collect_user_file,
          type = input$collect_type, server = input$collect_server
        )
      })

      message("\nGenerating merge requests complete. Check the console for messages. Click the 'Fetch' button to review the merge requests locally or view and comment on gitlab")
    })

    collect_fetch <- eventReactive(input$collect_fetch, {
      remote_fetch <- system("git config --get-all remote.origin.fetch", intern = TRUE)
      if (!"+refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*" %in% remote_fetch) {
        system("git config --add remote.origin.fetch +refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*")
      }


      # input$collect_token, input$collect_group,
      ## pre not used when called from the gadget interface because the full
      ## assignment name is retrieved from gitlab
      withProgress(message = "Fetching merge requests", value = 0, style = "old", {
        fetch_work(
          input$collect_token, input$collect_assignment,
          server = input$collect_server
        )
      })

      message("\nUse the Git tab in R-studio (click refresh first) to switch between different student assignment submissions\n")
    })

    output$collect_output <- renderPrint({
      if (is_empty(input$collect_assignment) || is_empty(input$collect_user_file)) {
       cat("Provide GitLab token and load the user file with GitLab tokens. You should be in the\nRstudio project used to create and for the assignment repo (i.e., check is the\nAssignment name shown is correct). Then press the Collect button to generate Merge\nRequests. Click the Fetch button to review the Merge Requests locally") } else {
        if (pressed(input$collect))
          ret <- collect()
        if (pressed(input$collect_fetch)) {
          cat("Fetching merge requests ...\n")
          ret <- collect_fetch()
        }
        if (not_pressed(input$collect) && not_pressed(input$collect_fetch))
          cat("Specify GitLab token and the user file with GitLab tokens. Then press the Collect button")
      }
    })

    ## Show remove_git modal when button is clicked.
    observeEvent(input$help, {
      ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
      showModal(
        modalDialog(title = "GitGadget Help",
          markdown::markdownToHTML(
            file.path(system.file(package = "gitgadget"), "app/help.md"),
            fragment.only = TRUE,
            options = "",
            stylesheet = ""
          ) %>%
          gsub("<table>","<table class='table table-condensed table-hover'>",.) %>%
          HTML,
          footer = tagList(modalButton("OK")),
          easyClose = TRUE
        )
      )
    })

    observeEvent(input$done, {
      stopApp(cat("Stopped GitGadget"))
    })
  }

  # runGadget(shinyApp(ui, server), port = port, viewer = shiny::paneViewer(minHeight = "maximize"))
  runGadget(shinyApp(ui, server), port = port, viewer = shiny::paneViewer(minHeight = 725))
}

#' Launch gitgadget in a separate process
#'
#' @details Using the \code{callr} package to launch gitgadget in a separate process so
#'   the console is not blocked. Rstudio viewer is used if available. See
#'   \url{https://github.com/vnijs/gitgadget} for documentation
#'
#'  @importFrom callr r_bg
#'
#' @export
gitgadget_callr <- function() {
  port <- get_port()
  callr::r_bg(function(port) { gitgadget::gitgadget(port = port)}, args = list(port), user_profile = TRUE)
  Sys.sleep(1)
  getOption("viewer")(paste0("http://localhost:", port, "/"))
}
