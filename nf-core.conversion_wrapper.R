suppressPackageStartupMessages(library("argparse"))

# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 

parser$add_argument("-i", "--input", default=NULL,
                    help="input nf-core pipeline JSON")
parser$add_argument("-s", "--staging_directory", default=NULL,
                    help="staging_directory to stage nf-core pipelines")
parser$add_argument("-r", "--run_scripts", default=NULL,
                    help="run_script directory for this pipeline")
args <- parser$parse_args()

if(!is.null(args$input)){
  input_json = args$input
} else{
  stop(paste("Please define an input nf-core JSON of pipelines"))
}
if(!is.null(args$staging_directory)){
  staging_directory = args$staging_directory
} else{
  stop(paste("Please define a staging directory for nf-core pipelines"))
}
if(!is.null(args$run_scripts)){
  run_scripts = args$run_scripts
} else{
  stop(paste("Please define a directory with scripts for this pipeline"))
}
library(rjson)
input_json = "/Users/keng/nf-core.pipeline.json"
pipeline_metadata = fromJSON(file=input_json)

### grab metadata for NF pipelines
number_of_pipelines = length(pipeline_metadata$remote_workflows)
nf_pipelines_metadata = list()
for(i in 1:number_of_pipelines){
  if(length(pipeline_metadata$remote_workflows[[i]][["releases"]]) > 0 ){
    pipeline_name = pipeline_metadata$remote_workflows[[i]][["name"]]
    pipeline_github_link = pipeline_metadata$remote_workflows[[i]][["full_name"]]
    pipeline_branch = pipeline_metadata$remote_workflows[[i]][["releases"]][[1]]$tag_name
    pipeline_description =  pipeline_metadata$remote_workflows[[i]][["description"]]
    pipeline_tags  =  pipeline_metadata$remote_workflows[[i]][["topics"]]
    nf_pipeline_metadata = list()
    nf_pipeline_metadata[["name"]] = pipeline_name
    nf_pipeline_metadata[["github_link"]] = pipeline_github_link
    nf_pipeline_metadata[["release_branch"]] = pipeline_branch
    nf_pipeline_metadata[["description"]] = pipeline_description
    nf_pipeline_metadata[["tags"]] = pipeline_tags
    nf_pipelines_metadata[[pipeline_name]] = nf_pipeline_metadata
  }
}

### pull code from github
staging_directory = "/Users/keng/nf-core"
library(rlog)
rlog::log_info(paste("GRABBING nf-core pipelines from GitHub"))
for(j in 1:length(names(nf_pipelines_metadata))){
  nf_pipeline = nf_pipelines_metadata[[names(nf_pipelines_metadata)[j]]]
  setwd(staging_directory)
  clone_cmd = paste("git clone",paste("https://github.com/",nf_pipeline[["github_link"]],".git",sep=""))
  rlog::log_info(paste("Step1: git clone",nf_pipeline[["github_link"]]))
  system(clone_cmd)
  new_dir = paste(staging_directory,nf_pipeline[["name"]],sep="/")
  #setwd(new_dir)
  #rlog::log_info(paste("Step2: checking out tag",nf_pipeline[["release_branch"]]))
  #checkout_cmd = paste("git checkout",paste(nf_pipeline[["release_branch"]]))
  #system(checkout_cmd)
}
### generate parameter XML files for each pipeline in nf-core_pipelines metadata
schema_jsons = list.files(staging_directory,pattern="nextflow_schema.json",full.names=T,recursive = T)
for(k in 1:length(schema_jsons)){
  setwd("/Users/keng")
  run_cmd = paste("Rscript nf-core.json_to_params_xml.R --json",schema_jsons[k])
  rlog::log_info(paste("Running",run_cmd))
  system(run_cmd)
}
### generate updated NF files for each pipeline in nf-core_pipelines_metadata

