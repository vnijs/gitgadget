# observeEvent(input$intro_token_gl_get, {
#   utils::browseURL("https://gitlab.com/-/profile/personal_access_tokens")
# })

# observeEvent(input$intro_token_gh_get, {
#   ## based on usethis::browse_github_pat
#   utils::browseURL("https://github.com/settings/tokens/new?scopes=repo,gist&description=R:GITHUB_PAT")
# })

observeEvent(input$intro_git, {

  # set default branch to 'main' and set rebase to false ... for now
  cmd <- "git config --global init.defaultBranch 'main'\ngit config --global pull.rebase false"
  resp <- system(cmd, intern = TRUE)
  cat("Used:", cmd, "\n")

  if (!is_empty(input$intro_user_name)) {
    cmd <- paste("git config --global --replace-all user.name", input$intro_user_name)
    resp <- system(cmd, intern = TRUE)
    cat("Used:", cmd, "\n")

    renvir <- file.path(renvirdir, ".Renviron")
    if (file.exists(renvir)) {
      renv <- readLines(renvir, warn = FALSE) %>%
        .[!grepl("^\\s*git.user\\s*=", .)] %>%
        paste0(collapse = "\n") %>%
        paste0(., "\ngit.user = \"", input$intro_user_name, "\"\n")
      cat(renv, file = renvir)
    } else {
      renv <- paste0("git.user = \"", input$intro_user_name, "\"\n")
      cat(renv, file = renvir)
    }
  }

  if (!is_empty(input$intro_user_email)) {
    cmd <- paste("git config --global --replace-all user.email", input$intro_user_email)
    resp <- system(cmd, intern = TRUE)
    cat("Used:", cmd, "\n")

    renvir <- file.path(renvirdir, ".Renviron")
    if (file.exists(renvir)) {
      renv <- readLines(renvir, warn = FALSE) %>%
        .[!grepl("^\\s*git.email\\s*=", .)] %>%
        paste0(collapse = "\n") %>%
        paste0(., "\ngit.email = \"", input$intro_user_email, "\"\n")
      cat(renv, file = renvir)
    } else {
      renv <- paste0("git.email = \"", input$intro_user_email, "\"\n")
      cat(renv, file = renvir)
    }
  }

  if (!is_empty(input$intro_server)) {
    renvir <- file.path(renvirdir, ".Renviron")
    if (file.exists(renvir)) {
      renv <- readLines(renvir, warn = FALSE) %>%
        .[!grepl("^\\s*git.server\\s*=", .)] %>%
        paste0(collapse = "\n") %>%
        paste0(., "\ngit.server = \"", input$intro_server, "\"\n")
      cat(renv, file = renvir)
    }
  }

  if (!is_empty(input$intro_token_gl)) {
    renvir <- file.path(renvirdir, ".Renviron")
    if (file.exists(renvir)) {
      envir <- readLines(renvir, warn = FALSE)
      token_ind <- which(grepl("^\\s*git.token\\s*=", envir))
      if (length(token_ind) > 0) {
        envir[token_ind] <- paste0("# ", envir[token_ind])
      }
      renv <- paste0(envir, collapse = "\n") %>%
        paste0(., "\ngit.token = \"", input$intro_token_gl, "\"\n")
      cat(renv, file = renvir)
    }
  }

  if (!is_empty(input$intro_token_gh)) {
    renvir <- file.path(renvirdir, ".Renviron")
    if (file.exists(renvir)) {
      renv <- readLines(renvir, warn = FALSE) %>%
        .[!grepl("^\\s*GITHUB_PAT\\s*=", .)] %>%
        paste0(collapse = "\n") %>%
        paste0(., "\nGITHUB_PAT = \"", input$intro_token_gh, "\"\n")
      cat(renv, file = renvir)
    }
  }

  ## set git.home option
  git_home <- gsub("^\\s+|\\s+$", "", input$intro_git_home)
  if (!is_empty(git_home) && git_home != Sys.getenv("git.home")) {
    if (!dir.exists(git_home)) dir.create(git_home, recursive = TRUE)
    renvir <- file.path(renvirdir, ".Renviron")
    if (file.exists(renvir)) {
      renv <- readLines(renvir, warn = FALSE) %>%
        .[!grepl("^\\s*git.home\\s*=", .)] %>%
        paste0(collapse = "\n") %>%
        paste0(., "\ngit.home = \"", git_home, "\"\n")
      cat(renv, file = renvir)
    } else {
      renv <- paste0("git.home = \"", git_home, "\")\n")
      cat(renv, file = renvir)
    }
    cat("Updated git home in .Renviron. Restart Rstudio to see the changes\n")
  }

  ## set git.user.type option
  git_user_type <- input$intro_user_type
  if (!is_empty(git_user_type) && git_user_type != Sys.getenv("git.user.type", "student")) {
    renvir <- file.path(renvirdir, ".Renviron")
    if (file.exists(renvir)) {
      renv <- readLines(renvir, warn = FALSE) %>%
        .[!grepl("^\\s*git.user.type\\s*=", .)] %>%
        paste0(collapse = "\n") %>%
        paste0(., "\ngit.user.type = \"", git_user_type, "\"\n")
      cat(renv, file = renvir)
    } else {
      paste0("git.user.type = \"", git_user_type, "\"\n") %>% cat(file = renvir)
    }
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

server_url <- reactive({
  init <- input$intro_server
  if (is_empty(init)) init <- Sys.getenv("git.server", "https://gitlab.com/api/v4/")
  sub("\\s*(https://|http://)?([^/]+).*", "\\1\\2", init)
})

output$ui_intro_get_token <- renderUI({
  url <- server_url()
  actionButton(
    "intro_token_gl_get", "Create",
    title = "Browse to git server to get a PAT",
    style = "margin-top: 25px;",
    onclick = paste0("window.open('", url, "/-/profile/personal_access_tokens', '_blank')")
  )
})

output$ui_intro_git_home <- renderUI({
  init <- Sys.getenv("git.home", basedir)
  if (!is.integer(input$intro_git_home_open)) {
    init <- shinyFiles::parseDirPath(gg_volumes, input$intro_git_home_open)
  }

  textInput("intro_git_home", "Base directory to clone repos into:",
    value = init,
    placeholder = "Choose directory to store repos"
  )
})

output$ui_intro_buttons <- renderUI({
  url <- server_url()
  tagList(
    fillRow(
      height = "70px", width = "300px",
      textInput("intro_keyname", "Key name:", value = "id_rsa"),
      textInput("intro_passphrase", "Pass-phrase:", value = "")
    ),
    actionButton(
      "intro_git", "Introduce",
      title = "Introduce yourself to git\n\nGit commands:\ngit config --global --replace-all user.name <username>\ngit config --global --replace-all user.email <useremail>\ngit config --global credential.helper <credential helper>\ngit config --global init.defaultBranch 'main'"
    ),
    actionButton(
      "intro_ssh", "SSH key",
      title = "Create an SSH key and copy the public-key to the clipboard",
      onclick = paste0("window.open('", url, "/-/profile/keys', '_blank')")
    ),
    actionButton("intro_restart", "Restart", title = "Restart GitGadget"),
    actionButton("intro_check", "Check", title = "Check settings")
  )
})

intro_ssh <- eventReactive(input$intro_ssh, {
  url <- server_url()
  url_keys <- paste0(url, "/-/profile/keys")
  url_short <- sub("\\s*(https://|http://)?([^/]+).*", "\\2", url)
  if (os_type != "Windows") {
    email <- system("git config --global --list", intern = TRUE) %>%
      .[grepl("^user.email", .)] %>%
      gsub("user.email=", "", .)

    if (length(email) == 0) {
      cat("Make sure you have an email address and user name set before generating the SSH key")
      return(invisible())
    }

    keyname <- ifelse(is_empty(input$intro_keyname), "id_rsa", input$intro_keyname)
    ssh_dir <- file.path(homedir, ".ssh")
    if (is_empty(.ssh_exists())) {
      if (!dir.exists(ssh_dir)) dir.create(ssh_dir)
      paste0("ssh-keygen -t rsa -b 4096 -C \"", email, "\" -f ", ssh_dir, "/", keyname, " -N '", input$intro_passphrase, "'") %>%
        system(.)

      key <- readLines(paste0(ssh_dir, "/", keyname, ".pub"), warn = FALSE)

      if (os_type == "Darwin") {
        out <- pipe("pbcopy")
        cat(key, file = out)
        close(out)
        cat(paste0("\nYour new public SSH key has been copied to the clipboard. Navigate to ", url_keys, " in your browser, paste the key into the 'Key' text input on the site, and click 'Add key'\n"))
      } else {
        cat(paste0("\n", key))
        cat(paste0("\n\nCopy the new public SSH key to ", url_keys, ". Paste the key into the 'Key' text input on the site, and click 'Add key'\n"))
      }
    } else {
      key <- readLines(.ssh_exists(), warn = FALSE)
      if (os_type == "Darwin") {
        out <- pipe("pbcopy")
        cat(key, file = out)
        close(out)
        cat(paste0("\nYour public SSH key has been copied to the clipboard. Navigate to ", url_keys, " in your browser, paste the key into the 'Key' text input on the site, and click 'Add key'\n"))
      } else {
        cat(paste0("\n", key))
        cat(paste0("\n\nCopy the public SSH key to ", url_keys, ". Paste the key into the 'Key' text input on the site, and click 'Add key'\n"))
      }
    }

    ## set environment variable
    Sys.setenv(GIT_SSH_COMMAND = paste0("'ssh -i '", ssh_dir, "/", keyname))

    if (file.exists(file.path(ssh_dir, "known_hosts"))) {
      if (!any(grepl(url_short, readLines(file.path(ssh_dir, "known_hosts"), warn = FALSE)))) {
        system(paste0("ssh-keyscan -t rsa,dsa ", url_short, " >> ", ssh_dir, "/known_hosts"))
      }
    } else {
      system(paste0("ssh-keyscan -t rsa,dsa ", url_short, " >> ", ssh_dir, "/known_hosts"))
    }

    if (keyname != "id_rsa") {
      cat("\nYou will also need to add the lines below to ~/.ssh/config\n")
      cat(paste0("\nHost ", url_short, "\n"))
      cat(paste0("    IdentityFile ~/.ssh/", keyname))
      cat("", file = "~/.ssh/config", append = TRUE)

      if (rstudioapi::isAvailable()) {
        rstudioapi::navigateToFile("~/.ssh/config", line = 1000L)
      } else {
        utils::file.edit("~/.ssh/config")
      }
    }
  } else {
    if (!is_empty(.ssh_exists())) {
      ssh_dir <- file.path(homedir, ".ssh")

      if (file.exists(file.path(ssh_dir, "known_hosts"))) {
        if (!any(grepl(url_short, readLines(file.path(ssh_dir, "known_hosts"), warn = FALSE)))) {
          system(paste0("ssh-keyscan -t rsa,dsa ", url_short, " >> ", ssh_dir, "/known_hosts"))
        }
      } else {
        system(paste0("ssh-keyscan -t rsa,dsa ", url_short, " >> ", ssh_dir, "/known_hosts"))
      }

      key <- readLines(.ssh_exists(), warn = FALSE)
      cat(key, file = "clipboard")
      cat(paste0("\nYour public SSH key has been copied to the clipboard. Navigate to ", url_keys, " in your browser, paste the key into the 'Key' text input on the site, and click 'Add key'\n"))
    } else {
      cat(paste0("\nSSH keys cannot be generated from Git Gadget on Windows. In RStudio go to Tools > Global Options and select Git/SVN. Click 'Create RSA Key' and then 'View public key'. Copy the key to the clipboard, navigate to ", url_keys, " in your browser, paste the key into the 'Key' text input on the site, and click 'Add key'\n"))
    }
  }
})

observeEvent(input$intro_restart, {
  mess <- "To restart the app with updated settings press the 'Done' button on the top-right. Then refresh your browser and restart Git Gadget"
  show_in_modal <- function(mess = mess) {
    showModal(
      modalDialog(
        title = "Restart Git Gadget",
        span(mess),
        easyClose = TRUE
      )
    )
  }
  if (getOption("gitgadget.jupyter", default = FALSE)) {
    show_in_modal(mess)
    # } else  if ((exists("launch.browser") && is.logical(launch.browser) FALSE)) {
  } else if ((exists("launch.browser") && is.logical(launch.browser) && !launch.browser)) {
    show_in_modal(mess)
  } else if (Sys.getenv("SHINY_PORT") == "") {
    stopApp(cat("\nAfter restarting Git Gadget your settings will have been updated\nand Git Gadget will be ready to clone, create, etc. repos\n\n"))
    rstudioapi::executeCommand("restartR")
    ## https://github.com/rstudio/rstudioapi/issues/111
    # rstudioapi::restartSession("gitgadget:::gitgadget()")
  } else {
    mess <- "To restart the app with updated settings press the 'Done' button on the top-right. Then restart R and restart Git Gadget"
    show_in_modal(mess)
  }
})

observeEvent(input$intro_check, {
  show_in_modal <- function() {
    path <- usethis:::scoped_path_r("user", ".Renviron", envvar = "R_ENVIRON_USER")
    Renv <- readLines(path)
    showModal(
      modalDialog(
        title = "Content of .Renviron",
        span(HTML(paste0(Renv, collapse = "</br>"))),
        size = "l",
        easyClose = TRUE
      )
    )
  }
  if (getOption("gitgadget.jupyter", default = FALSE)) {
    show_in_modal()
  } else if ((exists("launch.browser") && is.logical(launch.browser) && !launch.browser)) {
    show_in_modal()
  } else if (Sys.getenv("SHINY_PORT") == "") {
    usethis::edit_r_environ()
  } else {
    show_in_modal()
  }
})

output$introduce_output <- renderPrint({
  input$intro_git
  if (file.exists(file.path(find_home(), ".gitconfig")) ||
    file.exists(file.path(find_home(), "Documents/.gitconfig"))) {
    ret <- system("git config --global --list", intern = TRUE) %>%
      .[grepl("^user", .)]
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
        .[grepl("^credential.helper", .)]

      if (length(crh) == 0) {
        if (os_type == "Darwin") {
          system("git config --global credential.helper osxkeychain")
        } else if (os_type == "Windows") {
          system("git config --global credential.helper wincred")
        } else {
          system("git config --global credential.helper 'cache --timeout=32000000'")
        }

        crh <- system("git config --global --list", intern = TRUE) %>%
          .[grepl("^credential.helper", .)]
      }
    })

    if (length(crh) == 0) {
      cat("\nSetting up credential help failed. Go to https://happygitwithr.com/credential-caching.html in your browser for additional suggestions\n")
      crh <- ""
    }

    paste(c(ret, crh), collapse = "\n") %>%
      paste0("Show settings: git config --global --list\n\n", ., "\n") %>%
      paste0("git.home=", Sys.getenv("git.home", "<restart Rstudio to view updates>"), "\n") %>%
      cat()
  }

  if (pressed(input$intro_ssh)) {
    intro_ssh()
  } else {
    if (!is_empty(.ssh_exists())) {
      cat("\nSSH keys seem to exist on your system. Click the 'SSH key' button to copy them to the clipboard")
    } else {
      cat("\nNo SSH keys seem to exist on your system. Click the 'SSH key' button to generate them")
    }
  }
})