suppressPackageStartupMessages(library("argparse"))
library(stringr)
library(rjson)
library(rlog)
# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 

parser$add_argument("-i", "--input", default = NULL, required = TRUE,
                    help="input nf-core pipeline JSON")
parser$add_argument("-s","--staging-directory", "--staging_directory", default=NULL, 
                    required = TRUE, help="staging_directory to stage nf-core pipelines")
parser$add_argument("-r", "--run-scripts","--run_scripts", default=NULL,
                    help="run_script directory for this pipeline")
parser$add_argument("-d", "--no-dsl2-check","--no_dsl2_check", action="store_true",default=FALSE,
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
  stop(paste("Please define a scripts directory"))
}
pipeline_metadata = rjson::fromJSON(file=input_json)

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
rlog::log_info(paste("Generate parameter XML files"))
schema_jsons = list.files(staging_directory,pattern="nextflow_schema.json",full.names=T,recursive = T)
for(k in 1:length(schema_jsons)){
  setwd(run_scripts)
  run_cmd = paste("Rscript nf-core.json_to_params_xml.R --json",schema_jsons[k])
  rlog::log_info(paste("Running",run_cmd))
  system(run_cmd)
}
### generate updated NF files for each pipeline in nf-core_pipelines_metadata
### no dsl2 support currently
dsl2_enabled = function(nf_script){
  is_dsl2 = FALSE
  nf_script_dat = read.delim(nf_script,quote="",header=FALSE)
  for(i in 1:nrow(nf_script_dat)){
    skip_line = FALSE # --- line is a comment
    line_split = strsplit(nf_script_dat[i,],"\\s+")[[1]]
    clean_line = line_split
    for(t in 1:length(line_split)){
      sanitized_token = trimws(line_split[t])
      clean_line[t] = sanitized_token
    }
    clean_line = clean_line[clean_line!=""]
    if(grepl("/",clean_line[1])){
      skip_line = TRUE
    } else {
      if(length(clean_line) > 2){
        if(clean_line[1] == "nextflow.enable.dsl" && clean_line[3] == "2"){
          is_dsl2 = TRUE
          break
        }
      }
    }
  }
  if(is_dsl2){
    rlog::log_info(paste("SCRIPT",nf_script, "is DSL2 enabled"))
  }
  return(is_dsl2)
}
#################################
nextflow_scripts = list()
nextflow_configs = list()
configs_to_ignore_list = list()
if(length(schema_jsons) >0 ){
  for(k in 1:length(schema_jsons)){
   rlog::log_info(paste("LOOKING IN DIRECTORY:",dirname(schema_jsons[k])))
   nextflow_script = list.files(dirname(schema_jsons[k]),pattern="main.nf",full.names=T)
   nextflow_script = nextflow_script[!is.na(nextflow_script)]
   rlog::log_info(paste(schema_jsons[k],"Nextflow script",nextflow_script))
   main_config = list.files(dirname(schema_jsons[k]),pattern="nextflow.config",full.names=T)
   main_config = main_config[!is.na(main_config)]
   rlog::log_info(paste(schema_jsons[k],"Nextflow Config",main_config))
   configs_to_ignore = list.files(dirname(schema_jsons[k]),pattern="*config",full.names=T,recursive=T)
   if(length(configs_to_ignore) > 0){
     configs_ignore_bool = apply(t(configs_to_ignore),2,function(x) basename(x) == "genomes.config")
     if(sum(configs_ignore_bool) > 0 ){
       configs_to_ignore = configs_to_ignore[configs_ignore_bool]
       configs_to_ignore = configs_to_ignore[!is.na(configs_to_ignore)]
       rlog::log_info(paste(schema_jsons[k],"Nextflow Configs to Ignore",paste(configs_to_ignore,collapse=",")))
       configs_to_ignore_list[[schema_jsons[k]]] = configs_to_ignore
     } else{
       configs_to_ignore = NULL
     }
     } else{
       configs_to_ignore = NULL
   }
   if(length(nextflow_script) > 0 && length(main_config) > 0){
     if(!args$no_dsl2_check){
       if(!dsl2_enabled(nextflow_script)){
         nextflow_scripts[[schema_jsons[k]]] = nextflow_script
         nextflow_configs[[schema_jsons[k]]] = main_config
       } 
     } else{
       nextflow_scripts[[schema_jsons[k]]] = nextflow_script
       nextflow_configs[[schema_jsons[k]]] = main_config
     }
   } else{
     rlog::log_warn(paste("Could not find main script and config for",basename(dirname(schema_jsons))))
   }
  }
  
  if(length(names(nextflow_scripts)) > 0 ){
    all_nf_scripts = names(nextflow_scripts)
    scripts_to_update = all_nf_scripts[all_nf_scripts %in% names(nextflow_configs)]
    scripts_skipped = all_nf_scripts[!all_nf_scripts %in% scripts_to_update]
    rlog::log_warn(paste("Skipping updates for",paste(scripts_skipped,collapse=", ")))
    rlog::log_info(paste("Generating updates to NF scripts",paste(scripts_to_update,collapse=", ")))
    for(l in 1:length(scripts_to_update)){
      setwd(run_scripts)
      run_cmd = paste("Rscript nf-core.ica_mod_nf_script.R","--nf-script",nextflow_scripts[[scripts_to_update[l]]],"--config_file",nextflow_configs[[scripts_to_update[l]]])
      if(scripts_to_update[l] %in% names(configs_to_ignore_list)){
        configs_to_ignore = configs_to_ignore_list[[scripts_to_update[l]]]
        if(length(configs_to_ignore) > 0){
          run_cmd = run_cmd
            for(p in 1:length(configs_to_ignore)){
              run_cmd = paste(run_cmd,"--configs-to-ignore",configs_to_ignore[p])
            }
        }
      }
      rlog::log_info(paste("Running",run_cmd))
      system(run_cmd)
    }
    } else{
      rlog::log_info(paste("No NF scripts found\nCheck",paste(all_nf_scripts,collapse = ", ")))
    }
}
### Create our pipelines in ICA.