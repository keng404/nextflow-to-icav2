suppressPackageStartupMessages(library("argparse"))
library(rlog)
library(rjson)
library(stringr)
library(httr)
# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 

parser$add_argument("-s", "--nextflow-script","--nextflow_script", default=NULL,
                    help="Main nf script for a pipeline")
parser$add_argument("-l", "--cwl-script","--cwl_script", default=NULL,
                     help="Main CWL  script for a pipeline")
parser$add_argument("-z","--storage-size","--storage_size", default="Small",
                    help = "default storage size to run analyses with this pipeline. [Small => 1.2 TB, Medium => 2.4 TB, and Large => 7.2 TB] are storage sizes")
parser$add_argument("-w","--workflow-language","--workflow_language", default="nextflow",
                    required=TRUE, help = "workflow language of pipeline. Currently supported workflow languages are cwl and nextflow")
parser$add_argument("-x","--parameters-xml","--parameters_xml", default=NULL,
                    required=TRUE, help = "parameters XML file")
parser$add_argument("-v","--pipeline-name","--pipeline_name",required = TRUE,
                    default=NULL, help = "pipeline name")
parser$add_argument("-g","--code-project-directory","--code-project_directory",
                    default=NULL, help = "directory with other files of interest")
parser$add_argument("-p","--ica-project-name","--ica_project_name",
                    default=NULL, help = "ICA project name")
parser$add_argument("-i","--ica-project-id","--ica_project_id",
                    default=NULL, help = "ICA project id")
parser$add_argument("-k","--api-key-file","--api_key_file", required = TRUE,
                    default=NULL, help = "ICA API key file i")
parser$add_argument("-m","--simple-mode","--simple_mode",action="store_true",
                    default=FALSE, help = "flag to indicate the creation of a simple pipeline => One workflow script + XML file")
parser$add_argument("-n","--nf-core-mode","--nf_core_mode",action="store_true",
                    default=FALSE, help = "flag to indicate nf-core pipeline")
parser$add_argument("--debug",action="store_true", default=FALSE, help = "flag for debug")
parser$add_argument("-d","--description", default= NULL, help = "pipeline description")
parser$add_argument("-c","--comments", default= NULL, help = "pipeline comments")
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
allowed_storage_sizes = c("Small","Medium","Large")
allowed_workflow_languages = c("cwl","nextflow")
pipeline_creation_request = list()
args <- parser$parse_args()
## API key file
api_key_file = args$api_key_file
api_key = read.delim(api_key_file,quote="",header=F)[,1]
## main script
## xml
xml_file = args$parameters_xml

storage_size = args$storage_size
if(!storage_size %in% allowed_storage_sizes){
  error_message = paste("Could not find",storage_size,"in allowable storage sizes")
  if(grepl(storage_size,allowed_storage_sizes,ignore.case = T)){
    potential_matches = allowed_storage_sizes[grepl(allowed_storage_sizes,storage_size,ignore.case = T)]
    error_message = paste(error_message,"\nDid you mean",paste(potential_matches,collapse="or "),"\n")
  }
  stop(error_message)
}
workflow_language = args$workflow_language
if(!workflow_language %in% allowed_workflow_languages){
  error_message = paste("Could not find",workflow_language,"in allowable workflow languages")
  if(grepl(workflow_language,allowed_workflow_languages,ignore.case = T)){
    potential_matches = allowed_workflow_languages[grepl(allowed_workflow_languages,workflow_language,ignore.case = T)]
    error_message = paste(error_message,"\nDid you mean",paste(potential_matches,collapse="or "),"\n")
  }
  stop(error_message)
}

if(workflow_language == "cwl"){
  main_script = args$cwl_script
} else if(workflow_language == "nextflow"){
  main_script = args$nextflow_script
}
#######################
additional_files = args$project_directory
files_to_add = list()
if(!is.null(additional_files)){
  file_list  = list.files(additional_files,full.names = T,recursive=T)
  dir_list = sort(unique(apply(t(file_list),2, function(x) dirname(x))))
### folders
  files_to_add[["folders"]] = dir_list
### files 
  files_to_add[["files"]] = file_list
} else{
  if(!args$simple_mode){
    rlog::log_info(paste("By default, LOOKING for additonal files to add here:",dirname(main_script)))
    file_list  = list.files(dirname(main_script),full.names = T,recursive=T)
    dir_list = sort(unique(apply(t(file_list),2, function(x) dirname(x))))
    ### folders
    files_to_add[["folders"]] = dir_list
    ### files 
    files_to_add[["files"]] = file_list
  } else{
    rlog::log_info(paste("Not adding additional files to pipeline"))
  }
}


comments = args$comments
description = args$description
is_nf_core = args$nf_core_mode

ica_project_name = args$ica_project_name
ica_project_id = args$ica_project_id
pipeline_name  = args$pipeline_name
# envelop pipeline name with double-quotes to prevent inadvertent parsing
if(length(strsplit(pipeline_name,"\\s+")[[1]]) >1){
  pipeline_name = paste("\"",pipeline_name,"\"", sep="")
}

### auto config ICA CLI ---- for Docker use
system(paste(paste("sed -e s","'/MY_API_KEY/",api_key,"/'",sep=""),"script.exp","> icav2_cli.auto_config.exp"))
system("expect icav2_cli.auto_config.exp")
#"-server-url ica.illumina.com")

if(is.null(ica_project_id) && is.null(ica_project_name)){
  stop(paste("Please provide an ICA project ID or ICA project name.\nExciting"))
} else if(is.null(ica_project_id)){
  rlog::log_info(paste("RUNNING: icav2 projects list -o json ","-s ica.illumina.com","-k",paste("'",api_key,"'",sep=""),"> tmp.json"))
  system(paste("icav2 projects list -o json ","-s ica.illumina.com","-k",paste("'",api_key,"'",sep=""),"> tmp.json"))
  ica_project_lookup = rjson::fromJSON(file="tmp.json")
  if(length(ica_project_lookup$items) < 1){
    stop(paste("Take a look at tmp.json"))
  }
  ica_project_lookup_to_add = ica_project_lookup
  while(!is.null(ica_project_lookup_to_add$nextPageToken)){
    system(paste("icav2 projects list -o json","-s ica.illumina.com","-k",paste("'",api_key,"'",sep=""),"--page-token",ica_project_lookup_to_add$nextPageToken,"> tmp.json"))
    ica_project_lookup_to_add = rjson::fromJSON(file="tmp.json")
    ica_project_lookup$items = append(ica_project_lookup$items, ica_project_lookup_to_add$items)
  }
  # delete last line of terminal output
  ica_project_lookup_table_subset = list()
  current_index = 1
  for(i in 1:length(ica_project_lookup$items)){
    project_name = ica_project_lookup$items[[i]]$name
    lookup_query = project_name== ica_project_name || grepl(ica_project_name,project_name,ignore.case = T)
    if(sum(lookup_query) >0){
      ica_project_lookup_table_subset[[current_index]] = ica_project_lookup$items[[i]]
      current_index = current_index +  1
    }
  }
  if(length(ica_project_lookup_table_subset) >0){
    # return the 1st result of the query
    ica_project_id = ica_project_lookup_table_subset[[1]]$id
    # throw warning if more than 1 potential match exists
    if(length(ica_project_lookup_table_subset) >1 ){
      rlog::log_warn(paste("Found more than 1 result that matches the ICA project name",ica_project_name))
      print(ica_project_lookup_table_subset)
    } else {
      rlog::log_info(paste("Found project id for ",ica_project_name,"project id is:",ica_project_id))
    }
  } else{
    stop(paste("Please provide a valid project name [",ica_project_name,"]\nFound",paste(ica_project_lookup_table[,1],collapse=", ")))
  }
}

if(is.null(comments) && is_nf_core){
  comments = paste("nf-core pipeline",basename(dirname(main_script)))
  #comments = paste(strsplit(comments,"[::punc::]+")[[1]],collapse = " ")
  comments = gsub("[\\(\\)\\[\\]]","  ",comments,perl=T)
} 


if(is_nf_core && is.null(pipeline_name)){
  pipeline_name = paste("nf-core pipeline",basename(dirname(main_script)),collapse="_")
}
pipeline_creation_request[["code"]] = pipeline_name
pipeline_creation_request[["parametersXmlFile"]] = xml_file
if(workflow_language == "nextflow"){
  pipeline_creation_request[["mainNextflowFile"]] = main_script
} else if(workflow_language == "cwl"){
  pipeline_creation_request[["workflowCwlFile"]] = main_script
}
base_ica_command = "icav2 projectpipelines create -s ica.illumina.com"
full_ica_command = paste(base_ica_command,"-k",paste("'",api_key,"'",sep=""),workflow_language,pipeline_name,"--project-id",ica_project_id,"--main",main_script,"--parameter",xml_file,"--storage-size",storage_size)

if(!is.null(comments)){
  full_ica_command = paste(full_ica_command,"--comment",paste("\"",comments,"\""))
  pipeline_creation_request[["versionComment"]] = comments
}
## add comments for pipeline if provided or give canned comment if this is an nf-core pipeline

### add documentation if README.md is found in same directory as the main NF script
### this is typical convention for most local instances GitHub repos
documentation = list.files(dirname(main_script),pattern="README.md",full.names = T)
if(!is.na(documentation)){
    
    description = read.delim(documentation,quote="",header=F)
    description_content = NULL
    lines_to_add = c()
    for(i in 1:nrow(description)){
     
      if(!grepl("\\!\\[",description[i,]) && !grepl("docker",description[i,],ignore.case=T) && (grepl("description",description[i,],ignore.case=T) || grepl("doc",description[i,],ignore.case=T))){
        lines_to_add = c(lines_to_add,description[i,])
      }
    }
    if(length(lines_to_add) >0){
      description_content = paste(lines_to_add,collapse = "\\n")
      description_content = gsub("[\\(\\)\\[\\]]"," ",description_content,perl=T)
      #description_content = paste(strsplit(description_content,"[::punc::]+")[[1]],collapse=" ")
      description_content = paste("\"",description_content,"\"")
    }
    if(is.null(description_content)){
      description = paste("\"",paste("Please refer to documentation found here:",documentation),"\"")
    } else{
      description = description_content
    }
}
if(!is.null(description)){
  full_ica_command = paste(full_ica_command,"--description",description)
  pipeline_creation_request[["description"]] = description
}

### add additional files for pipeline if there are additional files found in main NF script
### This has to be done in ICA for now
files_add_string = c()
file_list = list()
if(workflow_language == "nextflow"){
  if(length(names(files_to_add))>0){
    pipeline_creation_request[["otherNextflowFiles"]] = files_to_add[["files"]]
  }
} else if(workflow_language == "cwl"){
  if(length(names(files_to_add))>0){
    pipeline_creation_request[["toolCwlFiles"]] = files_to_add[["files"]]
  }
}

# return result to check if we're good
pipeline_creation_url = paste("https://ica.illumina.com/ica/rest/api/projects/",ica_project_id,"/pipelines:createNextflowPipeline",sep="")
if(workflow_language == "cwl"){
  pipeline_creation_url = paste("https://ica.illumina.com/ica/rest/api/projects/",ica_project_id,"/pipelines:createCwlPipeline",sep="")
}

system(paste("icav2 analysisstorages list -o json","-s ica.illumina.com","-k",paste("'",api_key,"'",sep=""),"> storages.json"))
storages_json = rjson::fromJSON(file="storages.json")
storage_id = NULL
for(i in 1:length(storages_json$items)){
  if(storages_json$items[[i]]$name == storage_size){
    storage_id = storages_json$items[[i]]$id
  }
}

if(is.null(storage_id)){
  stop(paste("Could not find the storage id associated with the storage size",storage_size))
}
#full_ica_command = paste(full_ica_command, "-o json"," > pipeline_create.json")
#rlog::log_info(paste("RUNNING_COMMAND:",full_ica_command))
#system(full_ica_command)
#json_response = rjson::fromJSON(file="pipeline_create.json")
#if(json_response$status_code != 201){
#  stop(paste("Check pipeline_create.json for error messages"))
#} else{
#  rlog::log_info(paste("Pipeline Created successfully in project [",ica_project_id,"]\nPipeline id is:",json_response$id))
#}


#### API implementation
# example POST request
#curl -X 'POST' \
#'https://ica.illumina.com/ica/rest/api/projects/adsfadsfas/pipelines:createNextflowPipeline' \
#-H 'accept: application/vnd.illumina.v3+json' \
#-H 'Content-Type: multipart/form-data' \
#-F 'otherNextflowFiles=@make_snapshot.sh;type=text/x-sh' \
#-F 'otherNextflowFiles=@nfcore_external_java_deps.jar;type=application/java-archive' \
#-F 'versionComment=FirstVersion' \
#-F 'parametersXmlFile=@sarek.nf-core.pipeline.xml;type=text/xml' \
#-F 'code=asdfdasfasdf' \
#-F 'htmlDocumentation=' \
#-F 'mainNextflowFile=@main.ica.dev.nf' \
#-F 'metadataModelFile=' \
#-F 'links=' \
#-F 'analysisStorageId=qassss' \
#-F 'categories=' \
#-F 'description=adsfassadf'

pipeline_creation_request[["analysisStorageId"]] = storage_id
pipeline_creation_request[["links"]] = "" 
pipeline_creation_request[["categories"]] = ""
pipeline_creation_request[["htmlDocumentation"]] = ""
pipeline_creation_request[["metadataModelFile"]] = "" 
###### ATTEMPT ____ MANUALLY CREATE ACTUAL CURL COMMAND TO ICA API rest server to  create pipeline
curl_command = paste("curl -X 'POST'",pipeline_creation_url)
files_sections = c("otherNextflowFiles","toolCwlFiles","mainNextflowFile","parametersXmlFile","workflowCwlFile")
#adding headers
curl_command = paste(curl_command,"-H 'accept: application/vnd.illumina.v3+json'")
curl_command = paste(curl_command,"-H 'X-API-Key:",api_key,"'")
curl_command = paste(curl_command,"-H 'Content-Type: multipart/form-data'")
for(i in 1:length(names(pipeline_creation_request))){
  if(!names(pipeline_creation_request)[i] %in% files_sections){
    string_to_add = paste("-F",paste("'",names(pipeline_creation_request)[i],"=",pipeline_creation_request[[names(pipeline_creation_request)[i]]],"'",sep=""))
    curl_command = paste(curl_command,string_to_add)
  } else{
    if(names(pipeline_creation_request)[i] == "otherNextflowFiles" || names(pipeline_creation_request)[i] == "toolCwlFiles"){
      base_path = paste(dirname(main_script),"/",sep="")
      other_files = c()
      if(workflow_language == "nextflow"){
        other_files = pipeline_creation_request[["otherNextflowFiles"]]
      } else if(workflow_language == "cwl"){
        other_files =  pipeline_creation_request[["toolCwlFiles"]]
      }
      rlog::log_info(paste("OTHER_FILES_TO_ADD",paste(other_files,collapse=", ")))
      for(fidx in 1:length(other_files)){
        current_file = other_files[fidx]
        type_str = paste(";type=",mime::guess_type(current_file),sep="")
        filepath_str = paste(";filename=",gsub(base_path,"",current_file),sep="")
        string_to_add = paste("-F",paste("'",names(pipeline_creation_request)[i],"=@",current_file,filepath_str,type_str,"'",sep=""))
        curl_command = paste(curl_command,string_to_add)
      }
    } else if(names(pipeline_creation_request)[i] == "parametersXmlFile"){
      string_to_add = paste("-F",paste("'",names(pipeline_creation_request)[i],"=@",pipeline_creation_request[[names(pipeline_creation_request)[i]]],";type=text/xml'",sep=""))
      curl_command = paste(curl_command,string_to_add)
    } else if(names(pipeline_creation_request)[i] == "mainNextflowFile" || names(pipeline_creation_request)[i] == "workflowCwlFile"){
      string_to_add = paste("-F",paste("'",names(pipeline_creation_request)[i],"=@",pipeline_creation_request[[names(pipeline_creation_request)[i]]],"'",sep=""))
      curl_command = paste(curl_command,string_to_add)
    } else{
      rlog::log_warn(paste("NOT SURE what to do with",names(pipeline_creation_request)[i],":",paste(pipeline_creation_request[[names(pipeline_creation_request)[i]]],collapse=", ")))
    }
  }
}
rlog::log_info(paste("RUNNING:",curl_command))
if(!args$debug){
  pipeline_creation_response = rjson::fromJSON(json_str=system(curl_command,intern=T))
  #pipeline_creation_response = httr::POST(pipeline_creation_url,config=httr::add_headers("X-API-Key"=api_key), httr::accept("application/vnd.illumina.v3+json"),httr::content_type("multipart/form-data"),body = pipeline_creation_request,encode="multipart", verbose())
  #pipeline_creation_response_list = str(content(pipeline_creation_response, "parsed"))
  if(!"pipeline" %in% names(pipeline_creation_response)){
    #print(pipeline_creation_response_list)
    print(pipeline_creation_response)
  } else{
    rlog::log_info(paste("Pipeline successfully created for project",ica_project_id,"\nPipeline Id is:",pipeline_creation_response$pipeline$id))
  }
}