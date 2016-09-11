#' export
gitgadget <- function() {

  ## points to gitgadget project unfortunately
  # if (rstudioapi::isAvailable()) {
  #   projdir <- rstudioapi::getActiveProject()
  #   basedir <- normalizePath(file.path(projdir, ".."))
  # } else {
  #   projdir <- getwd()
  #   basedir <- normalizePath(file.path(projdir,".."))
  # }

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
  projdir <- basedir <- file.path(getOption("git.home", default = normalizePath(file.path(getwd(), ".."), winslash = "/")))
  # help <- function() HTML("<i title='View documentation' class='fa fa-question action-button shiny-bound-input' href='https://github.com/vnijs/gitgadget' id='gg_help'></i>")
  # help <- HTML("<button id='help' type='button' class='btn btn-default btn-sm action-button'>Help</button>")

  ui <- miniPage(
    # gadgetTitleBar(paste0("GITGADGET (", packageVersion("gitgadget"), ")"), left = miniTitleBarButton("help", "Help", primary = FALSE)),
    gadgetTitleBar(paste0("GITGADGET (", packageVersion("gitgadget"), ")")),
    includeCSS(file.path(system.file("app", package = "gitgadget"), "www/style.css")),
    miniTabstripPanel(
      miniTabPanel("Introduce", icon = icon("hand-paper-o"),
        miniContentPanel(
          HTML("<h2>Introduce yourself to git</h2>"),
          textInput("intro_user_name","User name:", value = getOption("git.user", "")),
          textInput("intro_user_email","User email:", value = getOption("git.email", "")),
          textInput("intro_git_home","Git directory:", value = getOption("git.home", basedir)),
          uiOutput("ui_intro_buttons"),
          hr(),
          verbatimTextOutput("introduce_output")
        )
      ),
      miniTabPanel("Create", icon = icon("git"),
        miniContentPanel(
          HTML("<h2>Create a repo on gitlab</h2>"),
          fillRow(height = "70px", width = "300px",
            textInput("create_user_name","User name:", value = getOption("git.user", "")),
            passwordInput("create_password","Password:", value = getOption("git.password", ""))
          ),
          fillRow(height = "70px", width = "300px",
            textInput("create_group","Group name:", value = getOption("git.group", "")),
            textInput("create_pre","Prefix:", value = getOption("git.prefix", ""))
          ),
          fillRow(height = "70px", width = "475px",
            uiOutput("ui_create_directory"),
            actionButton("create_directory_find", "Open", title = "Browse and select a file inside the local repo directory")
          ),
          textInput("create_server","API server:", value = getOption("git.server", "https://gitlab.com/api/v3/")),
          fillRow(height = "70px", width = "475px",
              uiOutput("ui_create_user_file"),
              actionButton("create_file_find", "Open", title = "Browse and select a CSV file with student id and token information. Used for assignment management by instructors")
          ),
          conditionalPanel("input.create_user_file != ''",
            radioButtons("create_type", "Assignment type:", c("individual","team"), "individual", inline = TRUE)
          ),
          actionButton("create", "Create", title = "Create a new repo using the gitlab API"),
          hr(),
          verbatimTextOutput("create_output")
        )
      ),
      miniTabPanel("Clone", icon = icon("clone"),
        miniContentPanel(
          HTML("<h2>Clone a repo</h2>"),
          textInput("clone_from","Clone from:", value = ""),
          textInput("clone_into","Clone into:", value = getOption("git.home", basedir)),
          textInput("clone_to","Clone to:", value = ""),
          actionButton("clone", "Clone", title = "Clone a repo from, e.g., github or gitlab over HTTPS. By default, the name of the remote repo and the local clone will be the same. To change the name of the local repo, provide an alternative in the 'Clone to' input\n\nGit command:\ngit clone <remote url>\n\nNote: To activate a credential helper the first time you clone a (private) repo from, e.g., github or gitlab, run 'git clone <remote url>' from the command line"),
          hr(),
          verbatimTextOutput("clone_output")
        )
      ),
      miniTabPanel("Branch", icon = icon("code-fork"),
        miniContentPanel(
          br(),
          HTML("<h4>Create a new branch</h4>"),
          textInput("branch_create_name", NULL, value = ""),
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
          HTML("<h4>Delete an existing branch</h4>"),
          uiOutput("ui_branch_delete_name"),
          actionButton("branch_unlink", "Unlink remote", title = "Unlink the local and the remote branch. The remote branch will not be deleted\n\nGit command:\ngit branch -d -r origin/<branch>"),
          actionButton("branch_delete", "Delete local", title = "Remove the local branch\n\nGit commands:\ngit checkout master\ngit branch -D <branch>"),
          br(), br()
        )
      ),
      miniTabPanel("Sync", icon = icon("refresh"),
        miniContentPanel(
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
      miniTabPanel("Collect", icon = icon("cloud-download"),
        miniContentPanel(
          HTML("<h2>Collect assignments</h2>"),
          fillRow(height = "70px", width = "300px",
            textInput("collect_user_name","User name:", value = getOption("git.user", "")),
            passwordInput("collect_password","Password:", value = getOption("git.password", ""))
          ),
          fillRow(height = "70px", width = "500px",
            textInput("collect_group","Group name:", value = getOption("git.group", "")),
            actionButton("collect_list", "List", title = "Collect the list of assignments associated with the specified group. Used for assignment management by instructors")
          ),
          uiOutput("ui_collect_assignment"),
          fillRow(height = "70px", width = "475px",
            uiOutput("ui_collect_user_file"),
            actionButton("collect_file_find", "Open", title = "Browse and select a CSV file with student id and token information. Used for assignment management by instructors")
          ),
          textInput("collect_server","API server:", value = getOption("git.server", "https://gitlab.com/api/v3/")),
          radioButtons("collect_type", "Assignment type:", c("individual","team"), "individual", inline = TRUE),
          actionButton("collect", "Collect", title = "Create merge requests from all student forks using the gitlab API. Used for assignment management by instructors"),
          actionButton("collect_fetch", "Fetch", title = "Create local branches from all merge requests and link them to (new) remote branches. Used for assignment management by instructors"),
          hr(),
          verbatimTextOutput("collect_output")
        )
      )
    )
  )

  server <- function(input, output, session) {

    observeEvent(input$help, {
      viewer <- getOption("viewer", default = browseURL)
      viewer("https://github.com/vnijs/gitgadget")
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
          paste0("\noptions(git.user = \"", input$intro_user_name, "\")\n") %>% cat(file = rprof)
        }
      }

      if (!is_empty(input$intro_user_email)) {
        cmd <- paste("git config --global --replace-all user.email", input$intro_user_email)
        resp <- system(cmd, intern = TRUE)
        cat("Used:", cmd, "\n")
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
          paste0("\noptions(git.home = \"", git_home, "\")\n") %>% cat(file = rprof)
        }
        cat("Updated .Rprofile. Restart Rstudio to see the changes\n")
      }

      ## Rstudio doesn't look for information in the Documents directory
      if (
        !file.exists(file.path(homedir, ".gitconfig")) &&
        file.exists(file.path(homedir, "Documents/.gitconfig"))
      ) {
        file.copy(
          file.path(homedir, "Documents/.gitconfig"),
          file.path(homedir, ".gitconfig")
        )
      }
    })

    .ssh_exists <- reactive({
      ## update after pressing the intro_ssh button
      input$intro_ssh
      input$intro_keyname

      keyname <- ifelse (is_empty(input$intro_keyname), "id_rsa", input$intro_keyname)
      .ssh_path <- file.path(homedir, ".ssh", paste0(keyname, ".pub"))
      if (file.exists(.ssh_path)) .ssh_path else ""
    })

    output$ui_intro_buttons <- renderUI({
      tagList(
        fillRow(height = "70px", width = "300px",
          textInput("intro_keyname","Key name:", value = "id_rsa"),
          textInput("intro_passphrase","Pass-phrase:", value = "")
        ),
        actionButton("intro_git", "Introduce", title = "Introduce yourself to git\n\nGit commands:\ngit config --global --replace-all user.name <username>\ngit config --global --replace-all user.email <useremail>\ngit config --global credential.helper <credential helper>"),
        actionButton("intro_ssh", "SSH key", title = "Create an SSH key and copy the public-key to the clipboard")
      )
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
          keyname <- ifelse (is_empty(input$intro_keyname), "id_rsa", input$intro_keyname)
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

    create_directory_find <- reactive({
      if(not_pressed(input$create_directory_find)) return(c())
      ## R doesn't have a cross platform dir.choose option
      ## user must select a file in the directory they want
      dirname(file.choose())
    })

    output$ui_create_directory <- renderUI({
      init <- create_directory_find() %>% {ifelse (length(.) == 0, projdir, .)}
      textInput("create_directory","Local directory:", value = init)
    })

    create_file_find <- reactive({
      if (not_pressed(input$create_file_find)) return(c())
      file.choose()
    })

    output$ui_create_user_file <- renderUI({
      init <- getOption("git.userfile", default = "")
      init <- create_file_find() %>% {ifelse (length(.) == 0, init, .)}
      textInput("create_user_file","User file:", value = init)
    })

    create <- eventReactive(input$create, {

      if (is_empty(input$create_user_name) || is_empty(input$create_password)) {
        cat("User name and password are required to create a new repo")
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
            input$create_user_name, input$create_password, create_group_lc, input$create_user_file,
            permission = 20, pre = create_pre_lc, server = input$create_server
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
          input$create_user_name, input$create_password, create_group_lc, repo, directory,
          pre = create_pre_lc, server = input$create_server
        )
        if (!is_empty(input$create_user_file)) {
          cat("Assigning work ...\n")
          assign_work(
            input$create_user_name, input$create_password, create_group_lc, repo,
            input$create_user_file, type = input$create_type, pre = create_pre_lc,
            server = input$create_server
          )
        }

        cat("\nCreate process complete. Check the console for messages")
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

    output$clone_output <- renderPrint({
      input$clone
      ret <- clone()
      isolate({
        if (length(ret) == 0) {
          cat("Nothing was returned. Make sure you specified a repo to clone from")
        } else if (ret == 0) {
          if (input$clone_to == "") {
            dir <- file.path(input$clone_into, gsub(".git", "", basename(input$clone_from)))
          } else {
            dir <- file.path(input$clone_into, input$clone_to)
          }

          rproj <- list.files(path = dir, pattern = "*.Rproj")
          if (length(rproj) == 0) {
            "Version: 1.0\n\nRestoreWorkspace: No\nSaveWorkspace: No\nAlwaysSaveHistory: Default\n\nEnableCodeIndexing: Yes\nEncoding: UTF-8\n\nAutoAppendNewline: Yes\nStripTrailingWhitespace: Yes\n\nBuildType: Package\nPackageUseDevtools: Yes\nPackageInstallArgs: --no-multiarch --with-keep.source\nPackageRoxygenize: rd,collate,namespace" %>%
              cat(file = file.path(dir, paste0(basename(dir),".Rproj")))
          }

          gitignore <- list.files(path = dir, pattern = ".gitignore")
          if (length(gitignore) == 0)
            cat(".Rproj.user\n.Rhistory\n.RData\n.Ruserdata\n", file = file.path(dir, ".gitignore"))

          cat("Repo was sucessfully cloned into", dir)
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
      if (!is_empty(branch)) system(paste0("git branch -d -r origin/", branch))
    })

    observeEvent(input$branch_delete, {
      if (!is.null(input$branch_delete_name)) {
        withProgress(message = "Deleting branch", value = 0, style = "old", {
          system("git checkout master")
          paste("git branch -D", input$branch_delete_name) %>%
            system(.)
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
      input$branch_create; input$branch_checkout; input$branch_delete; input$branch_merge

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
        selectInput("branch_delete_name", NULL, choices = resp)
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
      textInput("sync_from","Sync from:", value = ifelse (length(init) == 0, "", init[1]))
    })

    output$sync_output <- renderPrint({
      remote_info()
    })

    get_assignments <- eventReactive(input$collect_list, {

      username <- input$collect_user_name
      password <- input$collect_password
      group <- input$collect_group
      server <- input$collect_server
      if (is_empty(username) || is_empty(password) || is_empty(group) ||
          is_empty(server)) {
        message("Specify all required inputs to retrieve available assignments")
        return(invisible())
      }

      h <- new_handle()
      handle_setopt(h, customrequest = "POST")
      murl <- paste0(server, "session?login=", username, "&password=", password)
      resp <- curl_fetch_memory(murl, h)

      if (checkerr(resp$status_code) == FALSE)
        message("Server Error: ", fromJSON(rawToChar(resp$content))$message)

      token <- fromJSON(rawToChar(resp$content))$private_token
      handle_setopt(h, customrequest = "GET")
      handle_setheaders(h, "PRIVATE-TOKEN" = token)
      resp <- curl_fetch_memory(paste0(server, "projects"), h)

      if (checkerr(resp$status_code) == FALSE)
        message("Server Error: ", fromJSON(rawToChar(resp$content))$message)

      proj <- fromJSON(rawToChar(resp$content))
      proj <- proj[proj$namespace$name == group,]

      if (length(proj) == 0) {
        message("No assignments found for specified groupname")
        return(invisible())
      } else {
        proj[["name"]]
      }
    })

    output$ui_collect_assignment <- renderUI({
      resp <- get_assignments()
      if (length(resp) == 0) {
        HTML("<label>No assignments available for specified input values</label>")
      } else {
        selectInput("collect_assignment","Assignment name:", choices = resp)
      }
    })

    collect_file_find <- reactive({
      if (input$collect_file_find == 0) return(c())
      file.choose()
    })

    ## https://gitlab.com/gitlab-org/gitlab-ce/blob/master/doc/workflow/merge_requests.md#checkout-merge-requests-locally
    ## setup a branch switcher so you can easily do "git checkout origin/merge-requests/1" for each PR
    ## can you push back tot the PR as well?
    output$ui_collect_user_file <- renderUI({
      init <- getOption("git.userfile", default = "")
      init <- collect_file_find() %>% {ifelse(length(.) == 0, init, .)}
      textInput("collect_user_file","User file:", value = init)
    })

    collect <- eventReactive(input$collect, {
      req(
        input$collect_user_name, input$collect_password, input$collect_group,
        input$collect_server, input$collect_user_file
      )

      cat("Generating merge requests ...\n")

      withProgress(message = "Generating merge requests", value = 0, style = "old", {
        collect_work(
          input$collect_user_name, input$collect_password, input$collect_group,
          input$collect_assignment, input$collect_user_file,
          type = input$collect_type, pre = "", server = input$collect_server
        )
      })

      cat("\nGenerating merge requests complete. Check the console for messages. Click the 'Fetch' button to review the merge requests locally or view and comment on gitlab")
    })

    collect_fetch <- eventReactive(input$collect_fetch, {
      remote_fetch <- system("git config remote.origin.fetch", intern = TRUE)
      if (!"+refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*" %in% remote_fetch) {
        cat("Your working directory is not set to the assignment directory or this repo was not created using gitgadget. Please navigate to the assignment directory or add \"fetch = +refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*\" to the remote origin section in .git/config file")
      } else {


        withProgress(message = "Fetching merge requests", value = 0, style = "old", {
          fetch_work(
            input$collect_user_name, input$collect_password, input$collect_group,
            input$collect_assignment, pre = "", server = input$collect_server
          )
        })

        cat("Use the Git tab in Rstudio (click refresh first) to switch between different assignments")
      }
    })

    output$collect_output <- renderPrint({
      if (is_empty(input$collect_assignment)) {
       cat("Specify all required inputs to generate the list of available assignments. Then press the Collect button")
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

    observeEvent(input$done, {
      stopApp(TRUE)
    })
  }

  resp <- runGadget(shinyApp(ui, server), viewer = paneViewer())
}

## test section
main_gadget__ <- FALSE
if (main_gadget__) {

  library(shiny)
  library(miniUI)
  library(rstudioapi)
  library(curl)
  library(jsonlite)
  library(dplyr)

  source("R/git.R", local = TRUE)

  gitgadget()
}
