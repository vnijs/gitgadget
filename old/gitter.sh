CONFIG_FILE=./.gitter.conf
SERVER=https://gitlab.com/api/v3

# read the options
TEMP=`getopt -o hp::s::c:: --long help,profile::,config_file::,server::,command:: -n 'gitter.sh' -- "$@"`
eval set -- "$TEMP"

function Usage() {
    echo "Usage: gitter --profile=<profile> --command=<command>"
    echo "Possible commands are"
    echo "ls/rm path_with_namespace"
    #echo "Usage: gitter --profile=<profile> --config_file=<> --server=<>"
}

while true ; do
    case "$1" in
	-h|--help)
	    Usage ; shift; exit;;
        -p|--profile)
            PROFILE=$2 ; shift 2 ;;
        --config_file)
            CONFIG_FILE=$2 ; shift 2 ;;
        -s|--server)
            SERVER=$2 ; shift 2 ;;
	-c|--command)
	    COMMAND=$2 ; shift 2 ;;
        --) shift ; break ;;
        *) echo "Internal error!" ; exit 1 ;;
    esac
done

echo $COMMAND

if [ -z "$COMMAND" ]; then Usage ; exit ; fi

function jsonq() {
    local sep=',' 
    arg=$(printf "${sep}\"%s\"" "${@}")
    arg=[${arg:${#sep}}]
    python -c 'import json,sys;obj=json.load(sys.stdin);obj=[obj] if (isinstance(obj,list)==False) else obj; print "\n".join(["\t".join([str(proj[key]) for key in '$arg']) for proj in obj])' 
}

function GetToken() {
    local url=$SERVER
    url+="/session?login=$1&password=$2"
    curl -s -X POST $url | jsonq private_token
}


CREDENTIALS=`cat $CONFIG_FILE | awk -v PROFILE=$PROFILE 'BEGIN {RS=""} { if ($1=="profile" && $2==PROFILE) {
for(i=1;i<=NF;) {
output[$i] = $(i+1);
i = i+2;
};
print output["username"], "|", output["password"], "|" , output["token"]
}}'`

IFS='| '
creds=($CREDENTIALS)
if [ -z "${creds[2]}" ]; then 
USERNAME="${creds[0]}"
PASSWORD="${creds[1]}"
if [[ -n "$USERNAME" && -n "$PASSWORD" ]]; then 
echo "Getting token for $USERNAME"
TOKEN=$(GetToken $USERNAME $PASSWORD)
else
echo "No username/password provided for the profile"
exit 1
fi
else
TOKEN="${creds[2]}";
fi

if [ -z "$TOKEN" ]; then
echo "Server did not return token"; exit 1
fi

function getprojects() {
    local tok
    tok=$1
    shift
    local arg
    local sep=',' 
    arg=$(printf "${sep}\"%s\"" "${@}")
    arg=[${arg:${#sep}}]
    local url=$SERVER
    url+="/projects"
    curl -s --header "PRIVATE-TOKEN: $tok" -X GET $url | jsonq $@ | awk '{ printf("%s",$1); for(i=2;i<=NF;i++){ printf("%40s",$i) } ; printf("\n")}'
}

function RemoveRepo() {
    local url=$SERVER
    local tok=$1
    local projID=$2
    url+="/projects/$projID"
    curl -s --header "PRIVATE-TOKEN: $tok" -X DELETE $url 
}

function projID() {
    local tok=$1
    local projname=$2
    getprojects $tok id path_with_namespace | awk -v name=$projname '{if ($2==name) {print($1)}}'
}


full_command=($COMMAND)
case "${full_command[0]}" in
    ls)
	getprojects $TOKEN id name "${full_command[@]:1}"
	#getprojects $TOKEN id path_with_namespace name
	;;
    rm)
	PROJID=$(projID $TOKEN "${full_command[1]}")
	echo Deleting repo "${full_command[1]}"
	RemoveRepo $TOKEN $PROJID
	;;
    *)
	echo "Unknown command"
esac

#echo "Getting repos"
#getprojects $TOKEN id path_with_namespace 


