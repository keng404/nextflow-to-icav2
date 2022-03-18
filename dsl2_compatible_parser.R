library(rlog)
library(stringr)
#########
path_merge <- function(base_path,add_path){
  path_split = strsplit(add_path,"/")[[1]]
  if(length(path_split) == 1){
    return(file.path(base_path,add_path))
  } else if(path_split[1] == "." || path_split[1] == ".."){
    if(path_split[1] == "."){
      iters_to_remove = apply(t(path_split),2,function(x) x == ".")
      path_split = path_split[!iters_to_remove]
      return(file.path(base_path,paste(path_split[1:length(path_split)],collapse="/")))
    } else{
      iters_to_break = apply(t(path_split),2,function(x) x == "..")
      base_path_split = strsplit(base_path,"/")[[1]]
      iters_to_subtract = (length(base_path_split) + 1) - (1:length(path_split))[iters_to_break]
      base_path_iters_to_keep = (1:length(base_path_split) %in% iters_to_subtract)
      base_path_to_merge = paste(base_path_split[!base_path_iters_to_keep],collapse="/")
      add_path_to_merge = paste(path_split[!iters_to_break],collapse="/")
      return(file.path(base_path_to_merge,add_path_to_merge))
    }
  } else{
    return(file.path(base_path,add_path))
  }
}
#########
simplify_path <- function(path_of_interest){
  if(!is.character(path_of_interest)){
    rlog::log_error(paste("ERROR this is not a character:",path_of_interest))
    stop(paste("ERROR this is not a character:",path_of_interest))
  }
  path_split = strsplit(path_of_interest,"/")[[1]]
  if(sum("." %in% path_split) > 0){
    iters_to_remove = apply(t(path_split),2,function(x) x == ".")
    path_split = path_split[!iters_to_remove]
    simplified_path = paste(path_split[1:length(path_split)],collapse="/")
  } else if(sum(".." %in% path_split) > 0){
    path_components = strsplit(path_of_interest,"\\.\\./")[[1]]
    number_of_dirs_to_skip = sum(path_components == "")
    path_components = path_components[path_components != ""]
    for(s in 1:number_of_dirs_to_skip){
      path_components[1] = dirname(gsub("/$","",path_components[1]))
    }
    if(length(path_components) > 1){
      simplified_path = path_merge(path_components[1],paste(path_components[2],collapse="/"))
    } else{
      simplified_path = path_components[1]
    }
  } else{
    simplified_path = path_of_interest
  }
  return(simplified_path)
}
does_path_exist <- function(path_of_interest){
  if(!file.exists(path_of_interest)){
    rlog::log_error("Could not find file:",path_of_interest)
    return(FALSE)
  } else{
    return(TRUE)
  }
}
################
find_all_nf_scripts <- function(main_script){
  scripts_metadata = list()
  scripts_rename = list()
  scripts_to_look_at = c()
  skip_line = FALSE ### skip comment lines 
  dsl2_enabled = FALSE
  ### first pass to see if this is a DSL2 script
  nf_script_dat = read.delim(main_script,quote = "",header=F)
  for(i in 1:nrow(nf_script_dat)){
    skip_line = FALSE
    line_split = strsplit(nf_script_dat[i,],"\\s+")[[1]]
    clean_line = line_split
    for(j in 1:length(line_split)){
      clean_line[j] = trimws(line_split[j])
    }
    clean_line = clean_line[clean_line!=""]
    if(grepl("/",clean_line[1])){
      skip_line = TRUE
    } else{
      ### check if script is DSL2 enabled
      if(grepl("nextflow.enable.dsl",nf_script_dat[i,])){
        if(clean_line[3] == "2"){
          dsl2_enabled = TRUE
          rlog::log_info(paste("script",main_script,"is DSL2 enabled:",dsl2_enabled))
        }
      }
      if(!skip_line){
        if(clean_line[1] == "include"){
          result = str_extract(clean_line, "(?<=\\{)[^\\}]+")
          process = strsplit(result,"as")[[1]][1]
          location_to_add = gsub("'","",clean_line[length(clean_line)])
          initial_wd = getwd()
          setwd(dirname(main_script))
          if(!grepl(".nf$",location_to_add)){
            file_listing = normalizePath(paste(location_to_add,".nf",sep=""))
          } else{
            file_listing = normalizePath(location_to_add)
          }
          setwd(initial_wd)
          script_to_add = file_listing
          rlog::log_info(paste("Found_Scripts:",paste(script_to_add,collapse=", ")))
          ######### assumes we are writing a new file based on the original main_script
          final_relative_path = gsub(paste(main_script,"/",sep=""),"",script_to_add)
          new_file_name = gsub(".nf$",".def.nf",final_relative_path)
          new_file_path = file.path(dirname(location_to_add),basename(new_file_name))
          scripts_rename[[clean_line[length(clean_line)]]] = new_file_path
          #############################################################
          scripts_to_look_at = c(scripts_to_look_at,script_to_add)
        }
      }
    }
  }
  scripts_metadata[["scripts_rename"]] = scripts_rename
  scripts_metadata[["scripts_to_look_at"]] = scripts_to_look_at
  if(length(scripts_metadata[["scripts_rename"]]) == 0  || length(scripts_metadata[["scripts_to_look_at"]]) == 0){
    scripts_metadata = NULL
  }
  return(scripts_metadata)
}
###########################
getWorkflowEvents <- function(script){
  workflowEventMetadata = list()
  in_workflow_event_process = FALSE
  out_workflow_event_process = TRUE
  line_indent = ""
  in_expression = FALSE
  out_expression = TRUE
  in_closure = FALSE
  skip_line = FALSE ### skip comment lines 
  workflow_events = c("workflow.onComplete","workflow.onError")
  expression_directives = c("def","if","else","try","catch")
  ### workflow.onComplete, workflow.onError
  ### get process lines
  ### get line numbers in the script
  nf_script_dat = read.delim(script,quote = "",header=F)
  for(i in 1:nrow(nf_script_dat)){
    skip_line = FALSE
    line_split = strsplit(nf_script_dat[i,],"\\s+")[[1]]
    clean_line = line_split
    for(j in 1:length(line_split)){
      clean_line[j] = trimws(line_split[j])
    }
    clean_line = clean_line[clean_line!=""]
    if(grepl("/",clean_line[1])){
      skip_line = TRUE
      if(skip_line && in_workflow_event_process){
        rlog::log_info(paste("Adding comment line for workflow event:",workflow_event_name))
        rlog::log_info(paste("PROCESS_LINE:",nf_script_dat[i,]))
        process_lines = c(process_lines,nf_script_dat[i,])
        line_numbers = c(line_numbers,i)
      }
    } else{
      if(clean_line[1] %in% workflow_events || sum(workflow_events %in% clean_line[1]) > 0){
        in_workflow_event_process = TRUE
        out_workflow_event_process = FALSE
        in_expression = FALSE
        out_expression = TRUE
        workflow_event_name = gsub("\\{","",clean_line[1])
        rlog::log_info(paste("Found workflow event:",workflow_event_name))
        rlog::log_info(paste("PROCESS_LINE:",nf_script_dat[i,]))
        workflowEventMetadata[[workflow_event_name]] = list()
        process_lines = c(nf_script_dat[i,])
        line_numbers = c(i)
        in_closures = c()
      } else if(in_workflow_event_process && clean_line[1] != "}"){
        rlog::log_info(paste("PROCESS_LINE:",nf_script_dat[i,]))
        process_lines = c(process_lines,nf_script_dat[i,])
        line_numbers = c(line_numbers,i)
        if(sum(clean_line %in% expression_directives) > 0){
          if(clean_line[length(clean_line)] != "}" && "{"  %in% clean_line){
            in_expression = TRUE
            out_expression = FALSE
            in_closure = TRUE
            in_closures = c(in_closures,in_closure)
            rlog::log_info(paste("Found expression in workflow event:",nf_script_dat[i,]))
          }
        } 
      } else if(in_workflow_event_process && clean_line[1] == "}" && !out_expression){
        rlog::log_info(paste("PROCESS_LINE:",nf_script_dat[i,]))
        if(in_closure){
          if(! "{"  %in% clean_line){
            iter_to_change = length(in_closures) - sum(in_closures == FALSE)
            rlog::log_info(paste("updating in_closures:",iter_to_change,"number of in_closures",length(in_closures),nf_script_dat[i,]))
            in_closures[iter_to_change] = FALSE
          }
        } 
        if(sum(!in_closures) == length(in_closures)) {
          in_expression = FALSE
          out_expression = TRUE
          in_closures = c()
          rlog::log_info(paste("Exiting expression in workflow event:",nf_script_dat[i,]))
        }
        process_lines = c(process_lines,nf_script_dat[i,])
        line_numbers = c(line_numbers,i)
      } else if(in_workflow_event_process && clean_line[1] == "}" && out_expression){
        line_split1 = strsplit(nf_script_dat[i+1,],"\\s+")[[1]]
        clean_next_line = line_split1
        for(j in 1:length(line_split1)){
          clean_next_line[j] = trimws(line_split1[j])
        }
        clean_next_line = clean_next_line[clean_next_line!=""]
        clean_next_line_strsplit = strsplit(clean_next_line[length(clean_next_line)],"")[[1]]
        #################
        rlog::log_info(paste("PROCESS_LINE:",nf_script_dat[i,]))
        process_lines = c(process_lines,nf_script_dat[i,])
        line_numbers = c(line_numbers,i)
        rlog::log_info(paste("Updating workflow event:",workflow_event_name))
        workflowEventMetadata[[workflow_event_name]][["process_lines"]] = process_lines
        workflowEventMetadata[[workflow_event_name]][["line_numbers"]] = line_numbers
        workflowEventMetadata[[workflow_event_name]][["line_indent"]] = line_indent
        in_workflow_event_process = FALSE
        out_workflow_event_process = TRUE
        process_lines = c()
        line_numbers = c()
        in_expression = FALSE
        out_expression = TRUE
        in_closures = c()
      }
    }
  }
  return(workflowEventMetadata)
}
############
######## stub for getting actual path
#########################
## uncomment below to test
#main_script = "/Users/keng/nf-core/rnaseq/main.nf"
#scripts_to_check = find_all_nf_scripts(main_script=main_script)
#if(length(scripts_to_check) > 0){
#if(scripts_to_check == script_to_check){
  #workflow_script = list.files(path_merge(dirname(main_script),"workflows"),pattern="*.nf",full.names = T)
  #workflow_script = workflow_script[!is.na(workflow_script)]
#  workflow_script = scripts_to_check
#  scripts_to_add = workflow_script
#  scripts_to_parse = workflow_script
#  while(length(scripts_to_add) > 0){
#    if(length(scripts_to_add) > 0){
#      scripts_to_parse = c()
#      for(i in 1:length(scripts_to_add)){
#        rlog::log_info(paste("Looking through the following script:",paste(scripts_to_add[i],sep=", ")))
#        scripts_of_interest_metadata = find_all_nf_scripts(main_script=simplify_path(scripts_to_add[i]))
#        scripts_of_interest = scripts_of_interest_metadata[["scripts_to_look_at"]]
#        second_pass_scripts = scripts_of_interest[!scripts_of_interest %in% scripts_to_check]
#        if(length(second_pass_scripts) > 0){
#          path_exists_check  = apply(t(second_pass_scripts),2, function(x) does_path_exist(x))
#          if(sum(!path_exists_check) > 0){
#            fois = second_pass_scripts[!path_exists_check]
#            stop(paste("DOUBLE-CHECK if these scripts exist:",paste(fois,collapse=", ")))
#          }
#        }
#        scripts_to_parse = c(scripts_to_parse,scripts_of_interest[!scripts_of_interest %in% scripts_to_check])
#        scripts_to_check = c(scripts_to_check,scripts_of_interest[!scripts_of_interest %in% scripts_to_check])
#      }
#    }
#    scripts_to_add = scripts_to_parse
#  } 
#} else{
#  rlog::log_warn(paste("Did not find additional NF scripts to parse in addition to:",main_script))
#}
#scripts_to_check = apply(t(unique(scripts_to_check)),2,function(x) simplify_path(x))
#rlog::log_info(paste("Parsing through the following scripts:",paste(scripts_to_check,sep=", ")))


#####
## for each module/sub-module process
## update it and generate new file
## store those files and the original representations --- update the appropriate statements in the Main.nf and workflow.nf file
## create dummy submodule for copying intermediate files
## add this process to main script and call it
##########3
