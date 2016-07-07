##COURSE is the namespace where the repo would be created
##the repo name would be the name of current directory.
##all the files will be recursively uploaded into the repo
##give password as the first argument
##we need sed, grep, awk, find, base64, and curl. 


USERNAME=sanjiverat
COURSE=sanjiverat
PASSWD=$1


TOKEN=`curl -s -X POST "https://gitlab.com/api/v3/session?login=$USERNAME&password=$PASSWD" | sed 's/^.*private_token"://' | sed 's/"//g' | sed 's/}//'`

COURSEID=`curl -s --header "PRIVATE-TOKEN: $TOKEN" "https://gitlab.com/api/v3/namespaces" | sed 's/}/}\n/' | sed 's/{//g' | sed 's/}//g' | sed 's/,/ /g' | sed 's/\[//g' | sed 's/\]//g' | awk '{print $1 " " $2}' | sed 's/"id"://g' | sed \
's/"path"://g' | sed 's/"//g' | awk '{if ($2 == "'"$COURSE"'") print $1}'`



ASSIGNMENT=${PWD##*/}

FULLNAME="$COURSE/$ASSIGNMENT"


REPOID=`curl -s --header "PRIVATE-TOKEN: $TOKEN" -X GET "https://gitlab.com/api/v3/projects?search=$ASSIGNMENT" | sed 's/owner\":{.*}/owner\"/g' | grep -o "\"id\":[0-9]*" | sed 's/"id"://g'`

if [[ -z $REPOID ]]; then
    echo "Creating repo named $ASSIGNMENT for course $COURSE"
    REPOID=`curl -s --header "PRIVATE-TOKEN: $TOKEN" -X POST "https://gitlab.com/api/v3/projects?name=$ASSIGNMENT&namespace_id=$COURSEID" | sed 's/owner\":{.*}/owner\"/g' | grep -o "\"id\":[0-9]*" | sed 's/"id"://g'`
    echo "Starting to upload files"
    for file in $(find -type f -printf '%P\n')
    do
        (printf "content=" ; base64 $file) | curl "https://gitlab.com/api/v3/projects/$REPOID/repository/files" \
                                                  --header "PRIVATE-TOKEN: $TOKEN" \
                                                  --data "file_path=$file" \
                                                  --data "branch_name=master" \
                                                  --data "commit_message=uploaded_from_script" \
                                                  --data "encoding=base64" \
                                                  --data "@-" \
                                                  -X POST
    done
else
    echo "Repo $ASSIGNMENT already exists. Exiting without making any changes"
fi