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

  find_home <- function() {
    if (os_type == "Windows") {
      normalizePath(file.path(Sys.getenv("HOMEDRIVE"),Sys.getenv("HOMEPATH")), winslash = "/")
    } else if (os_type == "Darwin") {
      Sys.getenv("HOME")
    }
  }

  homedir <- find_home()
  projdir <- basedir <- file.path(getOption("git.home", default = normalizePath(file.path(getwd(),".."))))

  ui <- miniPage(
    gadgetTitleBar("GITGADGET"),
    includeCSS(file.path(system.file("app", package = "gitgadget"), "www/style.css")),
    miniTabstripPanel(
      miniTabPanel("Introduce", icon = icon("hand-paper-o"),
        miniContentPanel(
          HTML("<h2>Introduce yourself to git</h2>"),
          textInput("intro_user_name","User name:", value = getOption("git.user", "")),
          textInput("intro_user_email","User email:", value = getOption("git.email", "")),
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
            actionButton("create_directory_find", "Open")
          ),
          textInput("create_server","API server:", value = getOption("git.server", "https://gitlab.com/api/v3/")),
          fillRow(height = "70px", width = "475px",
              uiOutput("ui_create_user_file"),
              actionButton("create_file_find", "Open")
          ),
          conditionalPanel("input.create_user_file != ''",
            radioButtons("create_type", "Assignment type:", c("individual","team"), "individual", inline = TRUE)
          ),
          actionButton("create", "Create"),
          hr(),
          verbatimTextOutput("create_output")
        )
      ),
      miniTabPanel("Clone", icon = icon("clone"),
        miniContentPanel(
          HTML("<h2>Clone a repo</h2>"),
          fillRow(height = "70px", width = "300px",
            textInput("clone_user_name","User name:", value = getOption("git.user", "")),
            passwordInput("clone_password","Password:", value = getOption("git.password", ""))
          ),
          textInput("clone_from","Clone from:", value = ""),
          textInput("clone_into","Clone into:", value = getOption("git.home", basedir)),
          textInput("clone_to","Clone to:", value = ""),
          actionButton("clone", "Clone"),
          hr(),
          verbatimTextOutput("clone_output")
        )
      ),
      miniTabPanel("Branch", icon = icon("code-fork"),
        miniContentPanel(
          HTML("<h2>Create a new branch</h2>"),
          textInput("branch_create_name","Branch name:", value = ""),
          actionButton("branch_create", "Create local"),
          actionButton("branch_link_remote", "Link remote"),
          HTML("<h2>Merge branch with master</h2>"),
          uiOutput("ui_branch_merge_name"),
          actionButton("branch_merge", "Merge"),
          HTML("<h2>Delete an existing branch</h2>"),
          uiOutput("ui_branch_delete_name"),
          actionButton("branch_unlink_remote", "Unlink remote"),
          actionButton("branch_delete", "Delete local"),
          hr(),
          verbatimTextOutput("branch_output")
        )
      ),
      miniTabPanel("Collect", icon = icon("cloud-download"),
        miniContentPanel(
          HTML("<h2>Collect assignments</h2>"),
          textInput("collect_assignment_name","Assignment name:", value = ""),
          fillRow(height = "70px", width = "475px",
            uiOutput("ui_collect_user_file"),
            actionButton("collect_file_find", "Open")
          ),
          actionButton("collect", "Collect")
        )
      )
    )
  )

  server <- function(input, output, session) {

    observeEvent(input$intro_git, {

      if (!is_empty(input$intro_user_name)) {
        cmd <- paste("git config --global --replace-all user.name", input$intro_user_name)
        resp <- system(cmd, intern = TRUE)
        cat("Used:", cmd, "\n")
      }

      if (!is_empty(input$intro_user_email)) {
        cmd <- paste("git config --global --replace-all user.email", input$intro_user_email)
        resp <- system(cmd, intern = TRUE)
        cat("Used:", cmd, "\n")
      }

      ## Rstudio doesn't look for information in the Documents directory
      if (file.exists(file.path(homedir, "Documents", ".gitconfig"))) {
        file.copy(
          file.path(homedir, "Documents", ".gitconfig"),
          file.path(homedir, ".gitconfig"),
          overwrite = TRUE
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
        actionButton("intro_git", "Introduce"),
        actionButton("intro_ssh", "SSH key")
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
        crh <- system("git config --global --list", intern = TRUE) %>%
          .[grepl("^credential.help",.)]

        if (length(crh) == 0) {
          if (os_type == "Darwin") {
            system("git config --global credential.helper osxkeychain")
          } else if (os_type == "Windows") {
            system("git config --global credential.helper wincred")
          }

          crh <- system("git config --global --list", intern = TRUE) %>%
            .[grepl("^credential.help",.)]

        }

        if (length(crh) == 0) {
          cat("\nSetting up credential help failed. Go to http://happygitwithr.com/credential-caching.html in your browser for additional suggestions\n")
          crh <- ""
        }

        paste(c(ret, crh), collapse = "\n") %>%
          paste0("Show settings: git config --global --list\n\n", ., "\n") %>%
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
      init <- create_file_find() %>% {ifelse (length(.) == 0, "", .)}
      textInput("create_user_file","User file:", value = init)
    })

    create <- eventReactive(input$create, {

      if (!dir.exists(input$create_directory)) {
        cat("The specified directory does not exist. Create the directory and try again")
        return(invisible())
      }

      if (input$create_group != "" && input$create_group != getOption("git.user", "")) {
        cat("Creating group ...\n")
        create_group(
          input$create_user_name, input$create_password, input$create_group, input$create_user_file,
          permission = 20, pre = input$create_pre, server = input$create_server
        )
      }

      repo <- basename(input$create_directory)
      directory <- dirname(input$create_directory)
      cat("Creating repo ...\n")

      create_repo(
        input$create_user_name, input$create_password, input$create_group, repo, directory,
        pre = input$create_pre, server = input$create_server
      )
      if (!is_empty(input$create_user_file)) {
        cat("Assigning work ...\n")
        assign_work(
          input$create_user_name, input$create_password, input$create_group, repo,
          input$create_user_file, type = input$create_type, pre = input$create_pre,
          server = input$create_server
        )
      }

      cat("\nCreate process complete. Check the console for messages")
    })

    output$create_output <- renderPrint({
      input$create
      ret <- create()
    })

    clone <- eventReactive(input$clone, {
      if (input$clone_from != "") {
        if (input$clone_into != "") {
          owd <- setwd(input$clone_into)
          on.exit(setwd(owd))
        }
        clone_from <- cmd_from <- input$clone_from
        if (grepl("^https", clone_from) && !is_empty(input$clone_user_name) && !is_empty(input$clone_password)) {
          clone_from <- gsub("https://",paste0("https://", input$clone_user_name,":", input$clone_password, "@"), clone_from)
        }

        cmd <- paste("git clone", clone_from)
        cmdclean <- paste("git clone", input$clone_from)

        cloneto <- input$clone_to
        if (!is_empty(cloneto)) {
          cmd <- paste(cmd, cloneto)
          cmdclean <- paste(cmdclean, cloneto)
        }
        cat("Used:", cmdclean, "\n\n")
        system(cmd)
      }
    })

    output$clone_output <- renderPrint({
      input$clone
      req(!is.null(input$clone_user_name))
      req(!is.null(input$clone_password))
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
          cat("Repo was sucessfully cloned into", dir)
        } else {
          cat("There was an error cloning the repo. Check the R console for output")
        }
      })
    })

    output$ui_branch_merge_name <- renderUI({
      input$branch_delete
      input$branch_create
      resp <- system("git branch", intern = TRUE) %>%
        gsub("[\\* ]+", "", .) %>%
        {.[!grepl("master",.)]}

      if (length(resp) == 0) {
        HTML("<label>No branches available to merge</label>")
      } else {
        selectInput("branch_merge_name","Branch name:", choices = resp)
      }
    })

    output$ui_branch_delete_name <- renderUI({
      input$branch_delete
      input$branch_create
      resp <- system("git branch", intern = TRUE) %>%
        gsub("[\\* ]+", "", .) %>%
        {.[!grepl("master",.)]}

      if (length(resp) == 0) {
        HTML("<label>No branches available to delete</label>")
      } else {
        selectInput("branch_delete_name","Branch name:", choices = resp)
      }
    })

    observeEvent(input$branch_create, {
      if (input$branch_create_name != "") {
        paste("git checkout -b", input$branch_create_name) %>%
          system(.)
      }
    })

    observeEvent(input$branch_merge, {
      if (!is.null(input$branch_merge_name)) {
        system("git checkout master")
        paste("git merge ", input$branch_merge_name) %>%
          system(.)
      }
    })

    observeEvent(input$branch_link_remote, {
      if (input$branch_create_name != "") {
        ## would prefer to do this without 'push' -- however then I can't unlink for some reason
        # paste0("git branch --set-upstream-to origin ", input$branch_create_name) %>%
        paste("git push --set-upstream origin", input$branch_create_name) %>%
          system(.)
      }
    })

    observeEvent(input$branch_unlink_remote, {
      branch <- input$branch_delete_name
      if (is_empty(branch)) input$branch_create_name
      if (!is_empty(branch)) {
        # paste0("git branch --unset-upstream ", branch) %>%
        paste0("git branch -d -r origin/", branch) %>%
          system(.)
      }
    })

    observeEvent(input$branch_delete, {
      if (!is.null(input$branch_delete_name)) {
        system("git checkout master")
        paste("git branch -d", input$branch_delete_name) %>%
          system(.)
      }
    })

    output$branch_output <- renderPrint({
      cat("Overview of remotes:\n\n")
      cat(paste0(system("git remote -v", intern = TRUE), collapse = "\n"))
    })

    collect_file_find <- reactive({
      if (input$collect_file_find == 0) return(c())
      file.choose()
    })

    output$ui_collect_user_file <- renderUI({
      init <- collect_file_find() %>% {ifelse(length(.) == 0, "", .)}
      textInput("collect_user_file","User file:", value = init)
    })

    observeEvent(input$done, {
      stopApp(TRUE)
    })
  }

  resp <- runGadget(shinyApp(ui, server), viewer = paneViewer())
}

## test section
main_gadget__ <- FALSE
# main_gadget__ <- TRUE
if (main_gadget__) {

  # setwd("~/gh/gitgadget")
  library(shiny)
  library(miniUI)
  library(rstudioapi)
  library(curl)
  library(jsonlite)
  library(dplyr)
  # input <- list()

  source("~/gh/gitgadget/R/git.R", local = TRUE)

  gitgadget()
}
