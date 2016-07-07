## adding postback to path on Windows
# shell(paste0("setx PATH \"", Sys.getenv("RS_RPOSTBACK_PATH") %>% gsub("rpostback","postback",.), "\""))
# shell("PATH")

if (rstudioapi::isAvailable()) {
  basedir <- normalizePath(file.path(rstudioapi::getActiveProject(), ".."))
} else {
  basedir <- normalizePath(file.path(getwd(),".."))
}

#' export
gitgadget <- function() {
  ui <- miniPage(
    gadgetTitleBar("GIT gadget"),
    includeCSS(file.path(system.file("app", package = "gitgadget"), "www/style.css")),
    miniTabstripPanel(
      miniTabPanel("Introduce", icon = icon("hand-paper-o"),
        miniContentPanel(
          HTML("<h2>Introduce yourself to git</h2>"),
          textInput("intro_user_name","User name:", value = getOption("git.user", "")),
          textInput("intro_user_email","User email:", value = getOption("git.email", "")),
          uiOutput("ui_intro_buttons"),
          hr(),
          verbatimTextOutput("introduce")
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
            textInput("create_group","Group name:", value = ""),
            textInput("create_pre","Prefix:", value = getOption("git.prefix", ""))
          ),
          textInput("create_repo","Repo name:", value = ""),
          uiOutput("ui_create_directory"),
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
          verbatimTextOutput("createoutput")
        )
      ),
      miniTabPanel("Clone", icon = icon("clone"),
        miniContentPanel(
          HTML("<h2>Clone a repo</h2>"),
          textInput("clone_from","Clone from:", value = ""),
          textInput("clone_into","Clone into:", value = getOption("git.home", basedir)),
          textInput("clone_to","Clone to:", value = ""),
          actionButton("clone", "Clone"),
          hr(),
          verbatimTextOutput("cloneoutput")
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
          actionButton("branch_delete", "Delete local")
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
      if (input$intro_user_name != "") {
        cmd <- paste("git config --global --replace-all user.name", input$intro_user_name)
        resp <- system(cmd, intern = TRUE)
        cat("Used:", cmd, "\n")
      }

      if (input$intro_user_email != "") {
        cmd <- paste("git config --global --replace-all user.email", input$intro_user_email)
        resp <- system(cmd, intern = TRUE)
        cat("Used:", cmd, "\n")
      }
    })

    .ssh_exists <- reactive({
      os_type <- Sys.info()["sysname"]
      if (os_type == "Windows") {
        home <- file.path(Sys.getenv("HOMEDRIVE"),Sys.getenv("HOMEPATH"))
      } else if (os_type == "Darwin") {
        home <- Sys.getenv("HOME")
      }
      .ssh_path <- file.path(home, ".ssh/id_rsa.pub")
      if (file.exists(.ssh_path)) .ssh_path else ""
    })

    output$ui_intro_buttons <- renderUI({
      if (.ssh_exists() != "") {
        actionButton("intro_git", "Introduce")
      } else {
        tagList(
          actionButton("intro_git", "Introduce"),
          actionButton("intro_ssh", "SSH key")
        )
      }
    })

    intro_ssh <- eventReactive(input$intro_ssh, {
      os_type <- Sys.info()["sysname"]
      if (os_type == "Darwin") {
        email <- system("git config --global --list", intern = TRUE) %>%
          .[grepl("^user.email",.)] %>%
          gsub("user.email=","",.)

        if (length(email) == 0) {
          cat("Make sure you have an email address and user name set before generating the SSH key")
          return(invisible())
        }

        if (!dir.exists("~/.ssh")) dir.create("~/.ssh")

        paste0("ssh-keygen -t rsa -b 4096 -C \"", email, "\" -f ~/.ssh/id_rsa -P ''") %>%
          system(.)

        key <- readLines("~/.ssh/id_rsa.pub")
        out <- pipe("pbcopy")
        cat(key, file = out)
        close(out)
      } else if (os_type == "Windows") {

        if (.ssh_exists() != "") {
          key <- readLines(.ssh_exists())
          cat(key, file = "clipboard")
        } else {
          cat("\nSSH keys cannot be generated from GIT gadget on Windows. In RStudio go to Tools > Global Options and select Git/SVN. Click 'Create RSA Key' and then 'View public key'. Copy the key to the clipboard, navigate to https://gitlab.com/profile/keys in your browser, paste the key into the 'Key' text input on gitlab, and click 'Add key'\n")
        }
      }
    })

    output$introduce <- renderPrint({
      input$intro_git
      ret <- system("git config --global --list", intern = TRUE) %>%
        .[grepl("^user",.)]
      if (length(ret) == 0) {
        cat("No user information set. Enter a user name and email and click the 'Introduce' button\n\nSet user.name : git config --global user.name 'Your Name'\nSet user.email: git config --global user.email 'myemail@gmail.com'\n")
      } else {
        paste(ret, collapse = "\n") %>%
          paste0("Show settings: git config --global --list\n\n", ., "\n") %>%
          cat
      }

      if (!is.null(input$intro_ssh) && input$intro_ssh != 0) intro_ssh()

      os_type <- Sys.info()["sysname"]
      if (os_type == "Windows") {
        home <- file.path(Sys.getenv("HOMEDRIVE"),Sys.getenv("HOMEPATH"))
      } else if (os_type == "Darwin") {
        home <- Sys.getenv("HOME")
      }

      # system2("unset", "SSH_ASKPASS")
      # system2("unsetenv", "SSH_ASKPASS")

      if (.ssh_exists() != "") {
        key <- readLines(.ssh_exists())
        os_type <- Sys.info()["sysname"]
        if (os_type == "Windows") {
          cat(key, file = "clipboard")
        } else if (os_type == "Darwin") {
          out <- pipe("pbcopy")
          cat(key, file = out)
          close(out)
        }

        if (os_type %in% c("Windows","Darwin"))
          cat("\nYour public SSH key has been copied to the clipboard. Navigate to https://gitlab.com/profile/keys in your browser, paste the key into the 'Key' text input on gitlab, and click 'Add key'")
      } else {
        cat("\nNo SSH keys seem to exist on your system. Click the 'SSH key' button to generate them")
      }
    })

    # create_directory_find <- eventReactive(input$create_directory_find, {
    #   ## can't use choose here -- mac doesn't have a choose.dir function
    #   file.choose()
    # })

    output$ui_create_directory <- renderUI({
      init = file.path(getOption("git.home", basedir), input$create_repo)
      textInput("create_directory","Local directory:", value = init)
    })

    create_file_find <- reactive({
      if (input$create_file_find == 0) return(c())
      file.choose()
    })

    output$ui_create_user_file <- renderUI({
      init <- create_file_find() %>% {ifelse(length(.) == 0, "", .)}
      textInput("create_user_file","User file:", value = init)
    })

    create <- eventReactive(input$create, {
      if (input$create_group != "" && input$create_group != getOption("git.user", "")) {
        create_group(
          input$create_user_name, input$create_password, input$create_group, input$create_user_file, input$create_pre, input$create_server
        )
      }
      create_repo(
        input$create_user_name, input$create_password, input$create_group, input$create_repo, input$create_directory, input$create_pre, input$create_server
      )
    })

    output$createoutput <- renderPrint({
      input$create
      ret <- create()
    })

    clone <- eventReactive(input$clone, {
      if (input$clone_from != "") {
        if (input$clone_into != "") {
          owd <- setwd(input$clone_into)
          on.exit(setwd(owd))
        }
        cloneto <- input$clone_to
        if (cloneto == "") {
          cmd <- paste("git clone", input$clone_from)
        } else {
          cmd <- paste("git clone", input$clone_from, cloneto)
        }
        cat("Used:", cmd, "\n\n")
        # system(cmd, intern = TRUE)
        system(cmd)
      }
    })

    output$cloneoutput <- renderPrint({
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
      if (branch == "") input$branch_create_name
      if (branch != "") {
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
