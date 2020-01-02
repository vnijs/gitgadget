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
      clone_to <- file.path(getwd(), clone_to)
    } else {
      clone_to <- file.path(getwd(), basename(clone_from) %>% tools::file_path_sans_ext())
    }

    if (dir.exists(clone_to)) {
      showModal(
        modalDialog(
          title = "Directory already exists",
          span("The directory you are trying to clone to already exists")
        )
      )
      return(invisible())
    }

    cat("Used:", cmdclean, "\n\n")

    withProgress(message = "Cloning repo", value = 0, style = "old", {
      if (os_type == "Windows") {
        ret <- suppressWarnings(system(cmd, intern = TRUE))
      } else {
        ret <- suppressWarnings(system(paste(cmd, "2>&1"), intern = TRUE))
      }
      if (any(grepl("rpostback-askpass", ret)) || any(grepl("could not read Username", ret))) {
        if (rstudioapi::isAvailable()) {
          rstudioapi::terminalActivate()
          tid <- c()
          slp <- 1
          while (length(tid) == 0) {
            Sys.sleep(1)
            tid <- rstudioapi::terminalVisible()
            if  (slp > 10) break
            slp <- slp + 1
          }
          if (slp > 10) {
            cat("\nUnable to send commands to terminal. Please try again\n")
          } else {
            rstudioapi::terminalSend(tid, paste("git clone", clone_from, clone_to, "\n"))
            showModal(
              modalDialog(
                title = "Provide user name and password",
                span("Provide user name and password in Rstudio > Terminal to clone from GitLab (GitHub)")
              )
            )
          }
        }
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
  if (!is.integer(input$clone_into_open)) {
    init <- shinyFiles::parseDirPath(gg_volumes, input$clone_into_open)
  } else {
    init <- Sys.getenv("git.home", basedir)
  }
  textInput("clone_into", "Base directory to clone repo into:",
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
          cat(file = file.path(dir, paste0(basename(dir), ".Rproj")))
      }

      vscode <- list.files(path = dir, pattern = "*.code-workspace")
      if (length(vscode) == 0) {
        '{"folders": [{"path": "."}], "settings": {}}'
          cat(file = file.path(dir, paste0(basename(dir), ".code-workspace")))
      }

      gitignore <- list.files(path = dir, all.files = TRUE, pattern = ".gitignore")
      if (length(gitignore) == 0) {
        cat(".Rproj.user\n.Rhistory\n.RData\n.Ruserdata\n.DS_Store\n.ipynb_checkpoints\n.mypy_cache\n.vscode\n", file = ".gitignore")
      }

      cat("Repo was sucessfully cloned into", dir)

      if (length(rproj) == 0) {
        rproj <- list.files(path = dir, pattern = "*.Rproj", full.names = TRUE)[1]
      } else {
        rproj <- file.path(dir, rproj[1])
      }
      if (rstudioapi::isAvailable()) {
        rstudioapi::openProject(rproj, newSession = input$clone_proj == "new")
      }
    } else {
      cat("There was an error cloning the repo. Check the R console for output")
    }
  })
})
