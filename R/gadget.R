#' Launch gitgadget in Rstudio viewer if available
#'
#' @details See \url{https://github.com/vnijs/gitgadget} for documentation
#'
#' @export
gitgadget <- function() {

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
  rprofdir <- Sys.getenv("HOME")
  projdir <- basedir <- NULL

  if (rstudioapi::isAvailable())
    projdir <- basedir <- rstudioapi::getActiveProject()

  if (length(projdir) == 0)
    projdir <- basedir <- file.path(getOption("git.home", default = normalizePath(file.path(getwd(), ".."), winslash = "/")))

  # choose.dir <- function(...) {
  #   os_type <- Sys.info()["sysname"]
  #   if (os_type == "Windows") {
  #     utils::choose.dir(...)
  #   } else if (os_type == "Darwin") {
  #     pth <- file.path(system.file(package = "gitgadget"), "app/www/choose.dir.scpt")
  #     dpath <- suppressWarnings(
  #       system(paste0("osascript -l JavaScript ", pth), intern = TRUE)
  #     )
  #     if (length(dpath) > 0) {
  #       gsub("Path\\(\"(.*)\"\\)", "\\1", dpath)
  #     } else {
  #       character(0)
  #     }
  #   } else {
  #     dirname(file.choose())
  #   }
  # }

  ## Reset confirmation model
  # help <- function() HTML("<i title='View documentation' class='fa fa-question action-button shiny-bound-input' href='https://github.com/vnijs/gitgadget' id='gg_help'></i>")
  # help <- HTML("<button id='help' type='button' class='btn btn-default btn-sm action-button'>Help</button>")

  ui <- miniPage(
    # gadgetTitleBar(paste0("GITGADGET (", packageVersion("gitgadget"), ")"), left = miniTitleBarButton("help", "Help", primary = FALSE)),
    # gadgetTitleBar(paste0("GITGADGET (", packageVersion("gitgadget"), ")")),
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
          textInput("intro_user_name","User name:", value = getOption("git.user", ""), placeholder = "Provide GitLab/GitHub user name"),
          textInput("intro_user_email","User email:", value = getOption("git.email", ""), placeholder = "Provide GitLab/GitHub user email"),
          passwordInput("intro_token","Token:", value = getOption("git.token", "")),
          radioButtons("intro_user_type", "User type:", c("student","faculty"), getOption("git.user.type", "student"), inline = TRUE),
          fillRow(height = "70px", width = "475px",
            uiOutput("ui_intro_git_home"),
            actionButton("intro_git_home_open", "Open", title = "Browse and select a local directory", style = "margin-top: 25px;")
          ),
          uiOutput("ui_intro_buttons"),
          hr(),
          verbatimTextOutput("introduce_output")
        )
      ),
      miniTabPanel("Create", value = "create", icon = icon("git"),
        miniContentPanel(
          HTML("<h2>Create a repo on GitLab</h2>"),
          fillRow(height = "70px", width = "300px",
            textInput("create_user_name","User name:", value = getOption("git.user", "")),
            passwordInput("create_token","Token:", value = getOption("git.token", ""))
          ),
          fillRow(height = "70px", width = "300px",
            textInput("create_group","Group name:", value = getOption("git.group", "")),
            uiOutput("ui_create_pre")
          ),
          fillRow(height = "70px", width = "475px",
            uiOutput("ui_create_directory"),
            actionButton("create_directory_find", "Open", title = "Browse and select a local repo directory")
          ),
          textInput("create_server","API server:", value = getOption("git.server", "https://gitlab.com/api/v4/")),
          conditionalPanel("input.intro_user_type == 'faculty'",
            fillRow(height = "70px", width = "475px",
                uiOutput("ui_create_user_file"),
                actionButton("create_file_find", "Open", title = "Browse and select a CSV file with student id and token information. Used for assignment management by instructors")
            ),
            conditionalPanel("input.create_user_file != ''",
              radioButtons("create_type", "Assignment type:", c("individual","team"), "individual", inline = TRUE)
            )
          ),
          HTML("<h4>Remove previous version of .git and repo on Gitlab</h4>"),
          actionButton("remove_git_show", "Remove .git", title = "Remove previous .git directory if present", class = "btn-danger"),
          actionButton("remove_gitlab_show", "Remove remote", title = "Remove previous remote repo if present", class = "btn-danger"),
          HTML("<h4>Create local .git and remote repo on Gitlab</h4>"),
          actionButton("create", "Create", title = "Create a new repo using the gitlab API"),
          hr(),
          verbatimTextOutput("create_output")
        )
      ),
      miniTabPanel("Clone", value = "clone", icon = icon("clone"),
        miniContentPanel(
          HTML("<h2>Clone a repo</h2>"),
          textInput("clone_from","Repo to clone from remote git server:", placeholder = "Provide https link to repo", value = ""),
          # textInput("clone_into","Base directory to clone repo into:", value = getOption("git.home", basedir), placeholder = "Choose directory to store repo"),
          fillRow(height = "70px", width = "475px",
            # uiOutput("ui_intro_git_home"),
            uiOutput("ui_clone_into"),
            actionButton("clone_into_open", "Open", title = "Browse and select a local directory", style = "margin-top: 25px;")
          ),

          textInput("clone_to","Custom directory to clone repo into:", placeholder = "Use for custom directory only", value = ""),
          radioButtons("clone_proj", "Open project in:", c("current session" = "curr", "new session" = "new"), "curr", inline = TRUE),
          actionButton("clone", "Clone", title = "Clone a repo from, e.g., github or gitlab over HTTPS. By default, the name of the remote repo and the local clone will be the same. To change the name for the local repo to create, provide an alternative in the 'Custom directory' input\n\nGit command:\ngit clone <remote url>\n\nNote: To activate a credential helper the first time you clone a (private) repo from, e.g., github or gitlab, run 'git clone <remote url>' from the command line"),
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
          HTML("<h2>Sync with remote</h2>"),
          actionButton("sync_pull", "Pull", title = "Pull updates from remote repo\n\nGit command: git pull"),
          actionButton("sync_push", "Push", title = "Push all commited updates to the remote repo\n\nGit command: git push"),
          actionButton("sync_reset_show", "Reset", class = "btn-danger", title = "Completely reset local repo to remote master branch\n\nGit commands:\ngit --fetch all\ngit reset --hard origin/master"),
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
            # fillRow(height = "70px", width = "300px",
              # textInput("collect_user_name","User name:", value = getOption("git.user", ""), placeholder = "Provide GitLab/GitHub user name"),
              passwordInput("collect_token","Token:", value = getOption("git.token", "")),
            # ),
            fillRow(height = "70px", width = "500px",
              textInput("collect_group","Group name:", value = getOption("git.group", ""), placeholder = "Enter group name on GitLab"),
              actionButton("collect_list", "List", title = "Collect the list of assignments associated with the specified group. Used for assignment management by instructors")
            ),
            uiOutput("ui_collect_assignment"),
            conditionalPanel("input.collect_assignment != undefined && input.collect_assignment != null &&
                              input.collect_assignment.length > 0",
              fillRow(height = "70px", width = "475px",
                uiOutput("ui_collect_user_file"),
                actionButton("collect_file_find", "Open", title = "Browse and select a CSV file with student id and token information. Used for assignment management by instructors")
              ),
              textInput("collect_server","API server:", value = getOption("git.server", "https://gitlab.com/api/v4/")),
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

    observeEvent(input$intro_git, {

      if (!is_empty(input$intro_user_name)) {
        cmd <- paste("git config --global --replace-all user.name", input$intro_user_name)
        resp <- system(cmd, intern = TRUE)
        cat("Used:", cmd, "\n")

        rprof <- file.path(rprofdir, ".Rprofile")
        if (file.exists(rprof)) {
          readLines(rprof) %>%
            .[!grepl("options\\(git.user\\s*=",.)] %>%
            paste0(collapse = "\n") %>%
            paste0(., "\noptions(git.user = \"", input$intro_user_name, "\")\n") %>%
            cat(file = rprof)
        } else {
          paste0("options(git.user = \"", input$intro_user_name, "\")\n") %>% cat(file = rprof)
        }
      }

      if (!is_empty(input$intro_user_email)) {
        cmd <- paste("git config --global --replace-all user.email", input$intro_user_email)
        resp <- system(cmd, intern = TRUE)
        cat("Used:", cmd, "\n")

        rprof <- file.path(rprofdir, ".Rprofile")
        if (file.exists(rprof)) {
          readLines(rprof) %>%
            .[!grepl("options\\(git.email\\s*=",.)] %>%
            paste0(collapse = "\n") %>%
            paste0(., "\noptions(git.email = \"", input$intro_user_email, "\")\n") %>%
            cat(file = rprof)
        } else {
          paste0("options(git.email = \"", input$intro_user_email, "\")\n") %>% cat(file = rprof)
        }
      }

      if (!is_empty(input$intro_token)) {
        rprof <- file.path(rprofdir, ".Rprofile")
        if (file.exists(rprof)) {
          readLines(rprof) %>%
            .[!grepl("options\\(git.token\\s*=",.)] %>%
            paste0(collapse = "\n") %>%
            paste0(., "\noptions(git.token = \"", input$intro_token, "\")\n") %>%
            cat(file = rprof)
        } else {
          paste0("options(git.user = \"", input$intro_token, "\")\n") %>% cat(file = rprof)
        }
      }

      ## set git.home option
      git_home <- gsub("^\\s+|\\s+$", "", input$intro_git_home)
      if (!is_empty(git_home) && git_home != getOption("git.home", "")) {
        if (!dir.exists(git_home)) dir.create(git_home, recursive = TRUE)
        rprof <- file.path(rprofdir, ".Rprofile")
        if (file.exists(rprof)) {
          readLines(rprof) %>%
            .[!grepl("options\\(git.home\\s*=",.)] %>%
            paste0(collapse = "\n") %>%
            paste0(., "\noptions(git.home = \"", git_home, "\")\n") %>%
            cat(file = rprof)
        } else {
          paste0("options(git.home = \"", git_home, "\")\n") %>% cat(file = rprof)
        }
        cat("Updated git home in .Rprofile. Restart Rstudio to see the changes\n")
      }

      ## set git.user.type option
      git_user_type <- input$intro_user_type
      # input <- list(intro_user_name = "me", intro_user_type = "student")
      if (!is_empty(git_user_type) && git_user_type != getOption("git.user.type", "student")) {
        rprof <- file.path(rprofdir, ".Rprofile")
        if (file.exists(rprof)) {
          readLines(rprof) %>%
            .[!grepl("options\\(git.user.type\\s*=",.)] %>%
            paste0(collapse = "\n") %>%
            paste0(., "\noptions(git.user.type = \"", git_user_type, "\")\n") %>%
            cat(file = rprof)
        } else {
          paste0("options(git.user.type = \"", git_user_type, "\")\n") %>% cat(file = rprof)
        }
        cat("Updated user type in .Rprofile. Restart Rstudio to see the changes\n")
      }

      ## On Windows Rstudio doesn't look for information in the Documents
      ## directory for some reason
      if (!file.exists(file.path(homedir, ".gitconfig")) &&
          file.exists(file.path(homedir, "Documents/.gitconfig"))) {

        file.copy(
          file.path(homedir, "Documents/.gitconfig"),
          file.path(homedir, ".gitconfig")
        )
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

    intro_git_home <- reactive({
      if (pressed(input$intro_git_home_open))
        rstudioapi::selectDirectory()
      else
        return(c())
    })

    output$ui_intro_git_home <- renderUI({
      init <- intro_git_home()
      init <- ifelse(length(init) == 0, getOption("git.home", basedir), init)
      textInput("intro_git_home","Base directory to clone repos into:",
        value = init,
        placeholder = "Choose directory to store repos"
      )
    })


    output$ui_intro_buttons <- renderUI({
      ## intend to remove SSH functionality below
      # tagList(
      #   fillRow(height = "70px", width = "300px",
      #     textInput("intro_keyname","Key name:", value = "id_rsa"),
      #     textInput("intro_passphrase","Pass-phrase:", value = "")
      #   ),
      #   actionButton("intro_git", "Introduce", title = "Introduce yourself to git\n\nGit commands:\ngit config --global --replace-all user.name <username>\ngit config --global --replace-all user.email <useremail>\ngit config --global credential.helper <credential helper>"),
      #   actionButton("intro_ssh", "SSH key", title = "Create an SSH key and copy the public-key to the clipboard")
      # )
      actionButton("intro_git", "Introduce", title = "Introduce yourself to git\n\nGit commands:\ngit config --global --replace-all user.name <username>\ngit config --global --replace-all user.email <useremail>\ngit config --global credential.helper <credential helper>")
    })

    intro_ssh <- eventReactive(input$intro_ssh, {
      if (os_type == "Darwin") {
        email <- system("git config --global --list", intern = TRUE) %>%
          .[grepl("^user.email",.)] %>%
          gsub("user.email=","",.)

        if (length(email) == 0) {
          cat("Make sure you have an email address and user name set before generating the SSH key")
          return(invisible())
        }

        if (is_empty(.ssh_exists())) {
          if (!dir.exists("~/.ssh")) dir.create("~/.ssh")
          keyname <- ifelse(is_empty(input$intro_keyname), "id_rsa", input$intro_keyname)
          paste0("ssh-keygen -t rsa -b 4096 -C \"", email, "\" -f ~/.ssh/", keyname," -N '", input$intro_passphrase, "'") %>%
           system(.)

          key <- readLines(paste0("~/.ssh/",keyname,".pub"))
          out <- pipe("pbcopy")
          cat(key, file = out)
          close(out)
          cat("\nYour new public SSH key has been copied to the clipboard. Navigate to https://gitlab.com/profile/keys in your browser, paste the key into the 'Key' text input on gitlab, and click 'Add key'\n")

        } else {
          key <- readLines(.ssh_exists())
          out <- pipe("pbcopy")
          cat(key, file = out)
          close(out)
          cat("\nYour public SSH key has been copied to the clipboard. Navigate to https://gitlab.com/profile/keys in your browser, paste the key into the 'Key' text input on gitlab, and click 'Add key'\n")
        }
      } else if (os_type == "Windows") {
        if (!is_empty(.ssh_exists())) {
          key <- readLines(.ssh_exists())
          cat(key, file = "clipboard")
          cat("\nYour public SSH key has been copied to the clipboard. Navigate to https://gitlab.com/profile/keys in your browser, paste the key into the 'Key' text input on gitlab, and click 'Add key'\n")
        } else {
          cat("\nSSH keys cannot be generated from GIT gadget on Windows. In RStudio go to Tools > Global Options and select Git/SVN. Click 'Create RSA Key' and then 'View public key'. Copy the key to the clipboard, navigate to https://gitlab.com/profile/keys in your browser, paste the key into the 'Key' text input on gitlab, and click 'Add key'\n")
        }
      }
    })

    output$introduce_output <- renderPrint({
      input$intro_git
      ret <- system("git config --global --list", intern = TRUE) %>%
        .[grepl("^user",.)]
      if (length(ret) == 0) {
        cat("No user information set. Enter a user name and email and click the 'Introduce' button\n\nSet user.name : git config --global user.name 'Your Name'\nSet user.email: git config --global user.email 'myemail@gmail.com'\n")
        return(invisible())
      } else {

        ptext <- if(not_pressed(input$intro_git)) "Checking credentials" else "Working on introduction"
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
          paste0("git.home=", getOption("git.home", "<restart Rstudio to view updates>"),"\n") %>%
          cat
      }

      # if (pressed(input$intro_ssh)) {
      #   intro_ssh()
      # } else {
      #   if (!is_empty(.ssh_exists()) ) {
      #     cat("\nSSH keys seem to exist on your system. Click the 'SSH key' button to copy them to the clipboard")
      #   } else {
      #     cat("\nNo SSH keys seem to exist on your system. Click the 'SSH key' button to generate them")
      #   }
      # }
    })

    output$ui_create_pre <- renderUI({
      req(input$create_group)
      textInput("create_pre","Prefix:", value = getOption("git.prefix", paste0(input$create_group,"-")))
    })

    create_directory_find <- reactive({
      if(pressed(input$create_directory_find))
        rstudioapi::selectDirectory()
      else
        return(c())
    })

    output$ui_create_directory <- renderUI({
      init <- create_directory_find() %>% {ifelse (length(.) == 0, projdir, .)}
      textInput("create_directory","Local directory:", value = init, placeholder = "Base directory for the git repo")
    })

    create_file_find <- reactive({
      if (pressed(input$create_file_find))
        rstudioapi::selectFile(filter = "All files (*)")
      else
        return(c())
    })

    output$ui_create_user_file <- renderUI({
      init <- getOption("git.userfile", default = "")
      init <- create_file_find() %>% {ifelse(length(.) == 0, init, .)}
      textInput("create_user_file","Upload file with student tokens:", value = init, placeholder = "Open student CSV file")
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
    observeEvent(input$remove_gitlab_show, {
      ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
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
    })

    remove_gitlab <- observeEvent(input$remove_gitlab, {
      removeModal()
      # if (is_empty(input$create_user_name) || is_empty(input$create_password)) {
      if (is_empty(input$create_token)) {
        # cat("User name and password are required to remove remote repo")
        cat("Token required to remove remote repo")
        return(invisible())
      }

      if (!dir.exists(input$create_directory)) {
        cat("The specified directory does not exist. Create the directory and try again")
        return(invisible())
      }

      withProgress(message = "Removing remote repo", value = 0, style = "old", {
        create_group_lc <- tolower(input$create_group)
        create_pre_lc <- tolower(input$create_pre)
        repo <- basename(input$create_directory)

        cat("Removing remote repo ...\n")
        # token <- connect(input$create_user_name, input$create_password, input$create_server)$token
        # token <- connect(token = input$create_token)$token
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
      # if (is_empty(input$create_user_name) || is_empty(input$create_password)) {
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
            message(paste0("\nProject ", id$project_id, " fork removed for ", students[i, "userid"], " in ", students[i, "team"]))
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

      withProgress(message = "Creating and forking repo", value = 0, style = "old", {

        create_group_lc <- tolower(input$create_group)
        create_pre_lc <- tolower(input$create_pre)

        if (create_group_lc != "" && create_group_lc != getOption("git.user", "")) {
          cat("Creating group ...\n")
          create_group(
            input$create_token, create_group_lc, input$create_user_file,
            permission = 20, server = input$create_server
          )
        }

        repo <- basename(input$create_directory)

        if (grepl("[^A-z0-9_\\.\\-]", repo)) {
          cat("The repo name cannot contain spaces or symbols. Please change the name and try again")
          return(invisible())
        }

        directory <- dirname(input$create_directory)
        cat("Creating repo ...\n")

        create_repo(
          input$create_user_name, input$create_token, create_group_lc, repo, directory,
          pre = create_pre_lc, server = input$create_server
        )
        if (!is_empty(input$create_user_file)) {
          cat("Assigning work ...\n")
          assign_work(
            input$create_token, create_group_lc, repo,
            input$create_user_file, type = input$create_type, pre = create_pre_lc,
            server = input$create_server
          )
        }

        message("\nCreate process complete. Check the console for messages\n")
      })
    })

    output$create_output <- renderPrint({
      input$create  ## creating a dependency
      ret <- create()
    })

    clone <- eventReactive(input$clone, {
      if (input$clone_from != "") {
        if (input$clone_into != "") {
          owd <- setwd(input$clone_into)
          on.exit(setwd(owd))
        }
        clone_from <- cmd_from <- input$clone_from
        cmd <- paste("git clone", clone_from)
        cmdclean <- paste("git clone", input$clone_from)

        cloneto <- input$clone_to
        if (!is_empty(cloneto)) {
          cmd <- paste(cmd, cloneto)
          cmdclean <- paste(cmdclean, cloneto)
        }
        cat("Used:", cmdclean, "\n\n")

        withProgress(message = "Cloning repo", value = 0, style = "old", {
          system(cmd)
        })
      }
    })

    clone_into_home <- reactive({
      if (pressed(input$clone_into_open))
        rstudioapi::selectDirectory()
      else
        return(c())
    })

    output$ui_clone_into <- renderUI({
      init <- clone_into_home()
      init <- ifelse(length(init) == 0, getOption("git.home", basedir), init)
      textInput("clone_into","Base directory to clone repo into:",
        value = init,
        placeholder = "Choose directory to store repo"
      )
    })

    output$clone_output <- renderPrint({
      input$clone
      ret <- clone()
      isolate({
        if (length(ret) == 0) {
          cat("Nothing was returned. Make sure you specified a repo to clone from")
        } else if (ret == 0) {
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
        # paste0("git branch --set-upstream-to origin ", input$branch_create_name) %>%
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
        # selectInput("branch_delete_name", NULL, choices = resp)
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
      textInput("sync_from","Sync repo with remote it was forked from:", value = ifelse (length(init) == 0, "", init[1]), placeholder = "Provide https link to original remote repo")
    })

    output$sync_output <- renderPrint({
      remote_info()
    })

    get_assignments <- eventReactive(input$collect_list, {

      username <- input$collect_user_name
      token <- input$collect_token
      group <- input$collect_group
      server <- input$collect_server
      if (is_empty(username) || is_empty(token) || is_empty(group) ||
          is_empty(server)) {
        message("Specify all required inputs to retrieve available assignments")
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

      withProgress(message = "Generating list of available assignments", value = 0, style = "old", {
        resp <- get_assignments()
      })

      if (length(resp) == 0) {
        HTML("<label>No assignments available for specified input values</label>")
      } else {
        selectInput("collect_assignment","Assignment name:", choices = resp)
      }

    })

    collect_file_find <- reactive({
      if (input$collect_file_find == 0) return(c())
      rstudioapi::selectFile(filter = "All files (*)")
    })

    ## https://gitlab.com/gitlab-org/gitlab-ce/blob/master/doc/workflow/merge_requests.md#checkout-merge-requests-locally
    ## setup a branch switcher so you can easily do "git checkout origin/merge-requests/1" for each PR
    ## can you push back tot the PR as well?
    output$ui_collect_user_file <- renderUI({
      init <- getOption("git.userfile", default = "")
      init <- collect_file_find() %>% {ifelse(length(.) == 0, init, .)}
      textInput("collect_user_file","Upload file with student tokens:", value = init, placeholder = "Open student CSV file")
    })

    collect <- eventReactive(input$collect, {
      req(
        input$collect_token, input$collect_group,
        input$collect_server, input$collect_user_file
      )

      cat("Generating merge requests ...\n")

      ## pre not used when called from the gadget interface because the full
      ## assignment name is retrieved from gitlab
      withProgress(message = "Generating merge requests", value = 0, style = "old", {
        collect_work(
          input$collect_token, input$collect_group,
          input$collect_assignment, input$collect_user_file,
          type = input$collect_type, pre = "", server = input$collect_server
        )
      })

      message("\nGenerating merge requests complete. Check the console for messages. Click the 'Fetch' button to review the merge requests locally or view and comment on gitlab")
    })

    collect_fetch <- eventReactive(input$collect_fetch, {
      remote_fetch <- system("git config --get-all remote.origin.fetch", intern = TRUE)
      if (!"+refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*" %in% remote_fetch) {
        system("git config --add remote.origin.fetch +refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*")
      }

      ## pre not used when called from the gadget interface because the full
      ## assignment name is retrieved from gitlab
      withProgress(message = "Fetching merge requests", value = 0, style = "old", {
        fetch_work(
          input$collect_token, input$collect_group,
          input$collect_assignment, pre = "", server = input$collect_server
        )
      })

      message("\nUse the Git tab in R-studio (click refresh first) to switch between different student assignment submissions\n")
    })

    output$collect_output <- renderPrint({
      if (is_empty(input$collect_assignment) || is_empty(input$collect_user_file)) {
       cat("Provide GitLab token and the group name and then click the List button to show available assignments. Load the user file with GitLab tokens and press the Collect button to generate Merge Requests. Click the Fetch button to review the Merge Requests locally")
      } else {
        if (pressed(input$collect))
          ret <- collect()
        if (pressed(input$collect_fetch)) {
          cat("Fetching merge requests ...\n")
          ret <- collect_fetch()
        }
        if (not_pressed(input$collect) && not_pressed(input$collect_fetch))
          cat("Specify all required inputs and then press the Collect button")
      }
    })

    # observe({
    #   cat(input$tabs)
    # })

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

  resp <- runGadget(shinyApp(ui, server), viewer = paneViewer())

  # Launch Shiny app in another process, without blocking
  # https://github.com/rstudio/rstudioapi/issues/17#issuecomment-321958223
  # callr::r_bg(function() {
  #   shiny::shinyApp(
  #     ui = ui,
  #     server = server,
  #     options = list(port = 3131, launch.browser = FALSE)
  #   )
  # })
  # # # Give Shiny a second to start
  # Sys.sleep(1)
  # # # Launch viewer
  # getOption("viewer")("http://localhost:3131/")

  ## attempt to run in separate process
  # callr::r_bg(function() {
  #   resp <- runApp(shinyApp(ui, server), launch.browser = FALSE, port = 3131)
  # })
}

## test section
main_gadget__ <- FALSE
# main_gadget__ <- TRUE
if (main_gadget__) {

  library(shiny)
  library(miniUI)
  library(rstudioapi)
  library(curl)
  library(jsonlite)
  library(dplyr)

  source("R/git.R", local = TRUE)

  gitgadget()
  # Sys.sleep(1)
  # getOption("viewer")("http://localhost:3131/")
}
