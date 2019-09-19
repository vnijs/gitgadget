## code for adding and removing users from a group
## not currently used

add_users_group <- function(user_ids, group_id, token, permission, server) {
  resp <- lapply(user_ids, function(user_id) {
    add_user_group(user_id, group_id, token, permission, server)$status
  })
  if (resp[[1]] != "OKAY") stop("\nThere was an error adding users\n")
}

add_user_group <- function(user_id, group_id, token, permission, server) {
  h <- new_handle()
  handle_setopt(h, customrequest = "POST")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  murl <- paste0(server, "groups/", group_id, "/members?user_id=", user_id, "&access_level=", permission)
  resp <- curl_fetch_memory(murl,h)
  if (checkerr(resp$status_code) == FALSE) {
    mess <- fromJSON(rawToChar(resp$content))$message
    if (length(mess) > 0 && grepl("already exists", mess, ignore.case = TRUE)) {
      return(list(status = "OKAY", message = mess))
    } else {
      message("There was an error adding user", user_id, "to group", group_id, ":", mess, "\n")
      return(list(status = "SERVER_ERROR", message = mess))
    }
  }

  resp$content <- fromJSON(rawToChar(resp$content))
  list(status = "OKAY")
}

remove_members_group <- function(token, groupname = "", userfile = "", server = "https://gitlab.com/api/v4/") {

  resp <- connect(token, server)

  if (resp$status != "OKAY")
    stop("Error connecting to server: check token/server")

  token <- resp$token
  resp <- groupID(groupname, groupname, token, server)

  if (resp$status == "NOSUCHGROUP") {
    message("No group found: ", resp$message)
    return(invisible())
  }

  ## must give users permission in order to fork repo for them
  if (!is_empty(userfile)) {
    course_id <- resp$group_id
    udat <- read_ufile(userfile)
    uids <- userIDs(udat$userid, token, server)
    remove_users_group(uids, course_id, token, server)
  }
}

remove_users_group <- function(user_ids, group_id, token, server) {
  resp <- lapply(user_ids, function(user_id) {
    remove_user_group(user_id, group_id, token, server)$status
  })
}

remove_user_group <- function(user_id, group_id, token, server) {
  h <- new_handle()
  handle_setopt(h, customrequest = "DELETE")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  murl <- paste0(server, "groups/", group_id, "/members/", user_id)
  resp <- curl_fetch_memory(murl,h)
  if (checkerr(resp$status_code) == FALSE) {
    message("User ", user_id, " was not a member of group ", group_id, "\n")
    list(status = "SERVER_ERROR")
  } else {
    list(status = "OKAY")
  }
}
