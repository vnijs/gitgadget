library(methods)
library(curl)
library(jsonlite)


printf <- function(...) { cat(paste(sprintf(...),'\n',sep='')) }

checkerr <- function(code) {floor(code/100)==2}

connect <- function(username,password) {
    h <- new_handle()
    handle_setopt(h, customrequest = "POST")
    murl <- paste(SERVER,"session?login=",username,'&password=',password,sep="")
    resp <- curl_fetch_memory(murl,h)
    if (checkerr(resp$status_code) == FALSE) {
        return(list(status='SERVER_ERROR',message=fromJSON(rawToChar(resp$content))$message))
    }
    token <- fromJSON(rawToChar(resp$content))$private_token
    return(list(status='OKAY',token=token))
}

groupID <- function(groupname,path,token) {
    h <- new_handle()
    handle_setheaders(h,"PRIVATE-TOKEN" = token)
    murl <- paste(SERVER,'groups',sep='')
    resp <- curl_fetch_memory(murl,h)
    if (checkerr(resp$status_code) == FALSE) {
        return(list(status='SERVER_ERROR'))
    }
    resp$content <- fromJSON(rawToChar(resp$content))
    groups <- resp$content$id[resp$content$name==groupname & resp$content$path==path];
    if (length(groups)!=1) {
        return(list(status='NO_SUCH_GROUP'))
    }
    return(list(status='OKAY',group_id=groups[1]))
}


userIDs <- function(usernames,token) {
    sapply(usernames,function(username) {
        resp <- userID(username,token)
        if (resp$status=='OKAY') {
            return(resp$user_id[1])
        } else {
            return(NA)
        }
    })
}

userID <- function(username,token) {
    h <- new_handle()
    handle_setopt(h, customrequest = "GET")
    handle_setheaders(h,"PRIVATE-TOKEN" = token)
    resp <- curl_fetch_memory(paste(SERVER,'users?username=',username,sep=""),h)
    if (checkerr(resp$status_code) == FALSE) {
        return(list(status='SERVER_ERROR',message=rawToChar(resp$content)))
    }
    uid <- fromJSON(rawToChar(resp$content))$id
    if (length(uid)!=1) {
        return(list(status='NO_SUCH_USER'))
    }
    return(list(status='OKAY',user_id=uid[1]))
}

create_group <- function(group_name,path,token) {
    h <- new_handle()
    handle_setopt(h, customrequest = "POST")
    handle_setheaders(h,"PRIVATE-TOKEN" = token)
    murl <- paste(SERVER,"groups?name=",group_name,"&path=",path,"&visibility_level=0",sep="")
    resp <- curl_fetch_memory(murl,h)
    if (checkerr(resp$status_code) == FALSE) {
        return(list(status='SERVER_ERROR',message=fromJSON(rawToChar(resp$content))$message))
    }
    group_id <- fromJSON(rawToChar(resp$content))$id
    return(list(status='OKAY',group_id=group_id))
}

add_users <- function(user_ids,group_id,token,permission=20) {
    sapply(user_ids,function(user_id) {
        resp <- add_user(user_id,group_id,token,permission)
        return (resp$status)
    })
}

add_user <- function(user_id,group_id,token,permission=20) {

    h <- new_handle()
    handle_setopt(h, customrequest = "POST")
    handle_setheaders(h,"PRIVATE-TOKEN" = token)
    murl <- paste(SERVER,"groups/",group_id,"/members?user_id=",user_id,"&access_level=",permission,sep="")
    resp <- curl_fetch_memory(murl,h)
    if (checkerr(resp$status_code) == FALSE) {
        mesg <- fromJSON(rawToChar(resp$content))$message
        if (mesg != 'Already exists') {
            return(list(status='SERVER_ERROR',message=mesg))
        } else {
            return(list(status='OKAY'))
        }
    }
    resp$content <- fromJSON(rawToChar(resp$content))
    return(list(status='OKAY'))
}



update_course <- function(username,password,course_name,student_file) {

    token <- ''
    course_name <- paste('rady-msba-',course_name,sep='')

    resp <- connect(username=username,password=password)
    if (resp$status != 'OKAY') {
        printf("Error connecting to server: check username/password/server")
        return(FALSE)
    }
    token <- resp$token


    resp <- groupID(course_name,course_name,token)
    if (resp$status == 'NO_SUCH_GROUP') {
        resp <- create_group(course_name,course_name,token)
    }
    if (resp$status != 'OKAY') {
        printf("unable to create or get group: %s",resp$message)
    }
    course_id <- resp$group_id

    userfile <- read.csv(student_file,header=TRUE)

    user_ids <- userIDs(as.character(userfile$userid),token)
    add_users(user_ids,course_id,token)
}
#####################

get_allprojects <- function(token,everything=FALSE) {
    h <- new_handle()
    handle_setopt(h, customrequest = "GET")
    handle_setheaders(h,"PRIVATE-TOKEN" = token)
    resp <- curl_fetch_memory(paste(SERVER,"projects",sep=''),h)
    if (checkerr(resp$status_code) == FALSE) {
        printf("SERVER_ERROR: Problem getting projects\n")
        return(list(status="SERVER_ERROR",message=fromJSON(rawToChar(resp$content))$message))
    }
    mainproj <- fromJSON(rawToChar(resp$content))
    if (everything==FALSE) {
        mainproj <- mainproj[,names(mainproj) %in% c('id','name','path_with_namespace','forked_from_project')]
    }
    return(list(status="OKAY",repos=mainproj))
}

projID <- function(path_with_namespace,token) {

    resp <- get_allprojects(token)
    if (resp$status!='OKAY') {
        return(resp)
    }

    mainproj <- resp$repos
    mainproj <- mainproj[mainproj$path_with_namespace == path_with_namespace,]
    if (length(mainproj$id)==0) {
        printf("No such repo: %s: status code %s\n",path_with_namespace,mainproj$message)
        return(list(status="NO_SUCH_REPO",message='No such repo'))
    }
    return(list(status="OKAY",project_id=mainproj$id[1]))
}

forkRepo <- function(proj_id,token) {
    h <- new_handle()
    handle_setopt(h, customrequest = "POST")
    handle_setheaders(h,"PRIVATE-TOKEN" = token)
    murl <- paste(SERVER,"projects/fork/",proj_id,sep="")
    resp <- curl_fetch_memory(murl,h)
    if (checkerr(resp$status_code) == FALSE) {
        printf("Problem forking\n")
        return(list(status="SERVER_ERROR",message=fromJSON(rawToChar(resp$content))$message))
    } else {
        content <- fromJSON(rawToChar(resp$content))
        return(list(status="OKAY",content=content))
    }
}

renameRepo <- function(proj_id,token,newname) {
    h <- new_handle()
    handle_setopt(h, customrequest = "PUT")
    handle_setheaders(h,"PRIVATE-TOKEN" = token)
    murl <- paste(SERVER,"projects/",proj_id,'?name=',newname,sep="")
    resp <- curl_fetch_memory(murl,h)
    if (checkerr(resp$status_code) == FALSE) {
        printf("Problem changing the name\n")
        return(list(status='SERVER_ERROR',message=fromJSON(rawToChar(resp$content))$message))
    }
    return(list(status='OKAY',name=fromJSON(rawToChar(resp$content))$name))
}

setupteam <- function(leader_token,other_user_ids,assignment_id,course,team_name) {
    ##fork if needed for leader
    resp <- get_allprojects(leader_token,everything=TRUE)
    if (is.null(resp$repos$forked_from_project) | !(assignment_id %in% resp$repos$forked_from_project$id)) {
        printf("fork needs to be created for leader")
        resp <- forkRepo(assignment_id,leader_token)
        if (resp$status != 'OKAY') {
            printf("Error forking for leader");
            return(FALSE)
        }
        leader_project_id <- resp$content$id
        leader_project_name <- resp$content$name
        upstream_name <- resp$content$name
    } else {
        printf("Leader has already forked the assignment")
        leader_project_id <- resp$repos$id[!is.na(resp$repos$forked_from_project$id==assignment_id)]
        leader_project_name <- resp$repos$name[!is.na(resp$repos$forked_from_project$id==assignment_id)]
        upstream_name <- resp$repos$forked_from_project$name[!is.na(resp$repos$forked_from_project$id==assignment_id)]
    }

    ##rename leader's repo if needed.
    if (upstream_name == leader_project_name) {
        course <- gsub('rady-msba-(.*)','\\1',course)
        leader_project_name <- paste(course,'-',team_name,'-',upstream_name,sep='')
        resp <- renameRepo(leader_project_id,leader_token,leader_project_name)
        if (resp$status=='OKAY') {
            printf("Renamed leader's repo to %s",leader_project_name)
        } else {
            printf("Error renaming leader's repo to %s",leader_project_name)
        }
    } else {
        printf("Leader repo has already been renamed to %s",leader_project_name)
    }


    #add other_user_ids as users
    if (length(other_user_ids)>0) {
        printf("Adding team members to leader's repo")
        add_team(leader_project_id,leader_token,other_user_ids)
    } else {
        printf("No one to add to team")
    }
    return(TRUE)

}

add_team <- function(proj_id,token,team_mates) {
    h <- new_handle()
    handle_setopt(h, customrequest = "POST")
    handle_setheaders(h,"PRIVATE-TOKEN" = token)

    sapply(team_mates,function(otherid) {
        murl <- paste(SERVER,"projects/",proj_id,'/members?user_id=',otherid,'&access_level=40',sep="")
        resp <- curl_fetch_memory(murl,h)
        if (checkerr(resp$status_code) == TRUE) {
            content <- fromJSON(rawToChar(resp$content))
            printf("\tadding user %s to team\n",content$name)
            return(list(status='OKAY',content=content))
        } else {
            printf("\tError adding %s to team: server code %d\n",otherid,resp$status_code)
            content <- fromJSON(rawToChar(resp$content))
            return(list(status='SERVER_ERROR',content=content))
        }
    })
}



assign_work <- function(username,password,course_name,assignment,student_file) {

    token <- ''
    course_name <- paste('rady-msba-',course_name,sep='')

    resp <- connect(username=username,password=password)
    if (resp$status != 'OKAY') {
        printf("Error connecting to server: check username/password/server")
        return(FALSE)
    }
    token <- resp$token
    upstream_name <- paste(course_name,'/',assignment,sep='')
    resp <- projID(upstream_name,token)

    if (resp$status!="OKAY") {
        printf("Error getting assignment %s",upstream_name)
        return(FALSE)
    }
    assignment_id <- resp$project_id

    student_data <- read.csv(student_file,header=TRUE)
    student_data$userid <- as.character(student_data$userid)
    student_data$token <- as.character(student_data$token)

    student_data$user_id <- userIDs(student_data$userid,token)

    student_data$rownum <- c(1:length(student_data$user_id))

    aggregate(student_data$rownum,by=list(team=student_data$team),function(rownum) {
        leader <- min(rownum)
        if (length(rownum)!=1) {
            others <- rownum[!(rownum %in% leader)]
            printf("Setting up team:%s : Leader: %s Others: %s",student_data$team[leader],student_data$userid[leader],
                   paste(student_data$userid[others],sep=','))
        } else {
            others <- c()
            printf("Setting up team:%s : Leader: %s Others: -",student_data$team[leader],student_data$userid[leader])
        }
        teamname <- student_data$team[leader]
        setupteam(leader_token=student_data$token[leader],
                  other_user_ids=student_data$user_id[others],
                  assignment_id=assignment_id,
                  course=course_name,
                  team_name=teamname)
    })
    return(NULL)
}

#####################

make_repo <- function(repo_name,namespace,token) {

    if (namespace!='OWN') {
        resp <- groupID(namespace,namespace,token)
        if (resp$status!="OKAY") {
            printf("No such namespace to create repo")
            return(FALSE)
        }
        namespace_id <- resp$group_id
    } else {
        namespace_id <- ''
    }

    h <- new_handle()
    handle_setopt(h, customrequest = "POST")
    handle_setheaders(h,"PRIVATE-TOKEN" = token)

    murl <- paste(SERVER,"projects?",'name=',repo_name,'&namespace_id=',namespace_id,sep="")
    resp <- curl_fetch_memory(murl,h)
    content <- fromJSON(rawToChar(resp$content))
    if (checkerr(resp$status_code) == TRUE) {
        printf("Created repo %s",repo_name)
        return(list(status='OKAY',repo_id=content$id))
    } else {
        printf("Error creating repo")
        return(list(status='SERVER_ERROR',content=content))
    }
}



upload_file <- function(token,filename,directory,repo_id) {
    printf("Uploading file %s",filename)
    h <- new_handle()
    handle_setopt(h, customrequest = "POST")

    postdata <- paste("file_path=",filename,sep="")
    postdata <- paste(postdata,"branch_name=master",sep='&')
    postdata <- paste(postdata,"commit_message=uploaded_from_R_script",sep='&')
    postdata <- paste(postdata,"encoding=base64",sep='&')
    postdata <- paste(postdata,"content=",sep='&')
    postdata <- paste(postdata,URLencode(base64encode(paste(directory,'/',filename,sep=''))),sep='')
    handle_setopt(h, postfields = postdata)

    #handle_setopt(h, copypostfields = "branch_name=master")
    #handle_setopt(h, copypostfields = "commit_message=uploaded_from_R_script")
    #handle_setopt(h, copypostfields = "content=Actual Test here")

    handle_setheaders(h,"PRIVATE-TOKEN" = token)

    murl <- paste(SERVER,"projects/",repo_id,"/repository/files",sep='')
    resp <- curl_fetch_memory(murl,h)
    resp$content <- fromJSON(rawToChar(resp$content))
    return(resp$content)
}

upload_repo <- function(username,password,course_name,assignment,directory) {

    resp <- connect(username,password);
    if (resp$status != 'OKAY') {
        printf("Error connecting to server: check username/password/server")
        return(FALSE)
    }
    token <- resp$token
    printf("Making the repo %s %s\n",assignment,course_name)
    if (course_name == username) {
        make_repo(assignment,'OWN',token)
    } else {
        course_name <- paste('rady-msba-',course_name,sep='')
        make_repo(assignment,course_name,token)
    }

    setwd(directory)
    system2("git",c("init"))
    system2("unset","SSH_ASKPASS")
    system2("unsetenv", "SSH_ASKPASS")
    murl <- paste('https://',username,':',password,'@gitlab.com/',course_name,'/',assignment,'.git',sep='')
    print(murl)
    system2("git",c("remote","add","origin",murl))
    system2("git",c("add","."))
    system2("git",c("commit","-m",'"Auto Uploaded"'))
    system2("git",c("push","-u","origin","master"))

}

##############
SERVER <- 'https://gitlab.com/api/v3/'
faculty_username <- 'sanjiverat'
faculty_password <- Sys.getenv('GITPASSWORD')

course_name <- 'scm-2016'
##important - name the assignment something unique. because they will all reside in the student's namespce
##so if we dont name it appropriately, two faculty might have assignment1 which would create conflict.
##and we wont be able to fork.
##stupid problem because fork needs unique name
##and only after fork can we rename.
assignment <- 'scm-assignment1'
student_file <- 'mgba-students.csv'
directory <- '/home/rady/mg460v/mg460v/PERT'


update_course(faculty_username,faculty_password,course_name,student_file)

#upload_repo(faculty_username,faculty_password,course_name,assignment,directory)

#assign_work(faculty_username,faculty_password,course_name,assignment,student_file)

