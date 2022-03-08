suppressPackageStartupMessages(library("argparse"))

# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 

parser$add_argument("-s", "--nf-script","--nf_script", default=NULL, required=TRUE,
                    help="Main NF script")
parser$add_argument("-c","--config-file","--config_file", default=NULL,required=TRUE,
                    help = "main config file")
parser$add_argument("-x","--parameters-xml","--parameters_xml",
                    default=NULL, help = " parameters XML file output")
parser$add_argument("-g","--generate-parameters-xml","--generate_parameters_xml",
                    action="store_true",default=FALSE, help = "Generate parameters XML file")
parser$add_argument("-i","--configs_to_ignore", default=c(""),
                    action="append",help="config files to ignore")
parser$add_argument("-u","--instance-type-url","--instance_type_url", default="https://illumina.gitbook.io/ica/project/p-flow/f-pipelines#compute-types",
                    help = "URL that contains ICA instance type table")
parser$add_argument("-d","--default-instance","--default_instance", default = "himem-small",
                    help = "default instance value")
parser$add_argument("-n","--ica-instance-namespace","--ica_instance_namespace", default="scheduler.illumina.com/presetSize",
                    help = "ICA instance type namespace : will allow ICA scheduler to know  what instances to use")
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()
library(rlog)
library(stringr)

nf_script = args$nf_script
config_file = args$config_file
config_dat  = read.delim(config_file,quote="",header=F)
parameters_xml  = args$parameters_xml
configs_to_ignore = args$configs_to_ignore
configs_to_ignore = configs_to_ignore[configs_to_ignore!=""]
generate_parameters_xml = args$generate_parameters_xml
if(!is.null(parameter_xml)){
  generate_parameters_xml = TRUE
}
###########
ica_instance_namespace = args$ica_instance_namespace
default_instance = args$default_instance
instance_type_table_url = args$instance_type_url
######### NEED to add collection of process labels --- will help with update of 
########## corresponding functions
########## these labels will contain metadata for cpu/memory, error retry strategy,
########## and docker images used to run process
getParamsFromConfig <- function(conf_data){
  ## identify lines referring to params defined in config file
  lines_to_keep = c()
  line_skip = FALSE
  in_params_closure = FALSE
  in_closure = FALSE
  out_closure = TRUE
  out_params_closure = TRUE
  initial_nested_param = NA
  nested_param_key = NA
  
  for(i in 1:nrow(conf_data)){
    line_split = strsplit(conf_data[i,],"\\s+")[[1]]
    clean_line = line_split
    for(t in 1:length(line_split)){
      sanitized_token = trimws(line_split[t])
      clean_line[t] = sanitized_token
    }
    clean_line = clean_line[clean_line!=""]
    line_skip = FALSE
    if(grepl("/",clean_line[1])){
      line_skip = TRUE
    } 
    if(!line_skip && grepl("\\{",conf_data[i,]) && grepl("params",conf_data[i,]) && !grepl("def",conf_data[i,]) && !grepl("if",conf_data[i,]) && !grepl("else",conf_data[i,]) && !grepl("\\(params",conf_data[i,])  && !grepl("\\{params",conf_data[i,]) && !grepl("\\$\\{",conf_data[i,])){
      rlog::log_info(paste("ENTERING_PARAMS_ENCLOSURE:",conf_data[i,]))
      in_params_closure = TRUE
    } else if(!line_skip && grepl("\\{",conf_data[i,]) && !grepl("params",conf_data[i,])){
      in_closure = TRUE
      out_closure = FALSE
    } else if(!line_skip && grepl("\\}",conf_data[i,]) && !grepl("params",conf_data[i,]) && in_closure == TRUE){
      in_closure = FALSE
      out_closure = TRUE
    } else if(!line_skip && in_params_closure && grepl("\\}",conf_data[i,]) && !grepl("\\$\\{",conf_data[i,]) && in_closure ==FALSE ){
    ### initial check each line to see if we've exited the params closure
        out_params_closure = TRUE
        in_params_closure = FALSE
        rlog::log_info(paste("EXITED_PARAMS_ENCLOSURE:",conf_data[i,]))
    } else{
      rlog::log_info(paste("NOT_CHANGING_STATUS:",conf_data[i,]))
    }
    
    ### grab parameters in params enclosure
    if(!line_skip && in_params_closure == TRUE){
      if(!grepl("\\}",conf_data[i,]) && !grepl("\\{",conf_data[i,]) && grepl("\\=",conf_data[i,])){
        if(is.na(initial_nested_param) && is.na(nested_param_key)){
          rlog::log_info(paste("ADDING_LINE:",conf_data[i,]))
          lines_to_keep = c(lines_to_keep,conf_data[i,])
        }
      } else{
        rlog::log_info(paste("OTHER_LINE:",conf_data[i,]))
        if(!line_skip && !grepl("params",conf_data[i,])){
          if(!grepl("\'",conf_data[i,]) && grepl("\\{",conf_data[i,])){
            initial_nested_param = strsplit(conf_data[i,],"\\s+")[[1]]
            initial_nested_param = initial_nested_param[initial_nested_param!=""]
            initial_nested_param = initial_nested_param[!grepl("\\{",initial_nested_param)]
          } else{
            if( grepl("\\{",conf_data[i,])){
              nested_param_key = strsplit(conf_data[i,],"\\s+")[[1]]
              nested_param_key = nested_param_key[nested_param_key!=""]
              nested_param_key = nested_param_key[!grepl("\\{",nested_param_key)]
              rlog::log_info(paste("initial_nested_param:",initial_nested_param,"\t","nested_param_key:",nested_param_key))
            } else{
              rlog::log_info(paste("IGNORE_PARAM_LINE:",conf_data[i,]))
            }
          }
        }
      }
      rlog::log_info(paste("LINE_OF_INTEREST:",conf_data[i,],"PARAMS_ENCLOSURE:",in_params_closure))
      rlog::log_info(paste("initial_nested_param:",initial_nested_param,"\t","nested_param_key:",nested_param_key))
      if(!line_skip && !is.na(initial_nested_param) && !is.na(nested_param_key)){
        if(grepl("\\}",conf_data[i,]) && !grepl("\\$\\{",conf_data[i,])){
          rlog::log_info(paste("resetting nested key:",conf_data[i,]))
          nested_param_key = NA
        } else{
          if(!is.na(initial_nested_param) && !is.na(nested_param_key) && grepl("\\=",conf_data[i,])){
            modified_line = paste(paste(initial_nested_param,"[",nested_param_key,"]",".",sep=""),trimws(conf_data[i,]),sep="")
            rlog::log_info(paste("ADDING_MODIFIED_LINE:",modified_line))
            lines_to_keep = c(lines_to_keep,modified_line)
          }
        }
      } else if(!line_skip && !is.na(initial_nested_param) && is.na(nested_param_key)){
        if(grepl("\'",conf_data[i,]) && grepl("\\{",conf_data[i,])){
          rlog::log_info(paste("resetting nested key rule2:",conf_data[i,]))
          nested_param_key = strsplit(conf_data[i,],"\\s+")[[1]]
          nested_param_key = nested_param_key[nested_param_key!=""]
          nested_param_key = nested_param_key[!grepl("\\{",nested_param_key)]
        } else{
          rlog::log_info(paste("IGNORE_PARAM_LINE2:",conf_data[i,]))
          #if(grepl("\\}",conf_data[i,])){
          #  initial_nested_param = NA
          #}
        }
      } else if(!line_skip && (grepl("params\\.",conf_data[i,]) || grepl("\\$\\{",conf_data[i,])) && grepl("\\=",conf_data[i,])){
        if(!grepl("nextflow",conf_data[i,]) && !grepl("def",conf_data[i,])){
          rlog::log_info(paste("ADDING_LINE_THAT_MIGHT_BE_MISSED:",conf_data[i,]))
          lines_to_keep = c(lines_to_keep,conf_data[i,])
        }
      }
    } else if (!grepl("def",conf_data[i,]) && !grepl("if",conf_data[i,])  && !grepl("else",conf_data[i,])){
      if(grepl("params\\.",conf_data[i,]) && grepl("\\=",conf_data[i,]) && !grepl("\\(params",conf_data[i,])  && !grepl("\\{params",conf_data[i,]) ){
        modified_line = conf_data[i,]
        modified_line = gsub("params\\.","",conf_data[i,])
        rlog::log_info(paste("ADDING_LINE_STRIPPING_PARAMS:",modified_line))
        lines_to_keep = c(lines_to_keep,modified_line)
      }
    }
    
  } 
  rlog::log_info(paste("DONE_PARSING\n\n"))
  params = list()
  if(length(lines_to_keep) > 0){
    for(j in 1:length(lines_to_keep)){
      line_parsed = strsplit(lines_to_keep[j],"\\s+")[[1]]
      line_parsed = line_parsed[line_parsed != ""]
      params[[paste("params.",line_parsed[1],sep="")]] = line_parsed[3]
      # params[[paste("params.",line_parsed[1],sep="")]] = gsub("\'","",line_parsed[3])
    }
  }
  return(params)
}

z = getParamsFromConfig(conf_data=config_dat)

paramsFiller <- function(list_to_fill,params_list){
  params_list_updated = list_to_fill
  for(i in 1:length(list_to_fill)){
    result = str_extract(list_to_fill[i], "(?<=\\{)[^\\}]+")
    # maybe try a while loop?
    if(!is.na(result)){
      # fill in appropriate params vaiue
      updated_value = gsub(result,gsub("'","",params_list[[result]]),list_to_fill[i])
      # remove ${ and } from original string
      updated_value = gsub("\\$\\{","",updated_value)
      updated_value = gsub("\\}","",updated_value)
      updated_value = gsub("\'","",updated_value)
      updated_value = gsub("\"","",updated_value)
      params_list_updated[i] = updated_value
    }
  }
  return(params_list_updated)
}

findOtherConfigs <- function(conf_path,conf_data,defaultConfigs = NULL){
  ## identify lines in config that refer to additional config files
  lines_to_keep = c()
  # check if line contains if/else expression ()
  for(i in 1:nrow(conf_data)){
    if(grepl("includeConfig",conf_data[i,]) && !(grepl("test",conf_data[i,],ignore.case = T))){
      ### check if config is in if loop, we'll need to look at this expresssion to evaluate
      #### which configs to use
      lines_to_keep = c(lines_to_keep,conf_data[i,])
    }
  }
  configs = c()
  for(j in 1:length(lines_to_keep)){
    line_parsed = strsplit(lines_to_keep[j],"\\s+")[[1]]
    line_parsed = line_parsed[line_parsed != ""]
    line_parsed = line_parsed[!grepl("includeConfig",line_parsed)]
    if(!(grepl("\\{",line_parsed))){
      line_parsed = paste(dirname(conf_path),line_parsed,sep="/")
      configs = c(configs,gsub("\'","",line_parsed))
    } else{
      configs = c(configs,gsub("\'","",line_parsed))
    }
  }
  return(configs)
}

localConfigOrNot <- function(config_list){
  keep_array = c()
  for(i in 1:length(config_list)){
    keep_array[i] = !(grepl("http",config_list[i]))
  }
  return(config_list[keep_array])
}

z1 = findOtherConfigs(conf_path=config_file,conf_data=config_dat)
if(length(configs_to_ignore) > 0){
  z1 = z1[ !(z1 %in% configs_to_ignore)]
}
#paramsFiller(list_to_fill=z1,params_list=z)
final_config_list = localConfigOrNot(paramsFiller(list_to_fill=z1,params_list=z))

## STEP1: Grab nextflow config and propagate to all configs to grab the appropriate params
paramCollection = list()
for(config_idx in 1:length(final_config_list)){
  key_name = strsplit(basename(final_config_list[config_idx]),"\\.")[[1]][1]
  current_file = final_config_list[config_idx]
  rlog::log_info(paste("Reading in",current_file))
  parsedParams  = getParamsFromConfig(conf_data=read.delim(current_file,header=F))
  if(length(parsedParams) > 0) {
    paramCollection[[key_name]] = parsedParams
  }
}
compareConfigs <- function(defaultConfig, otherConfigs){
  updatedConfig = defaultConfig
  configs_to_check  = names(otherConfigs)
  default_config_params = names(defaultConfig)
  params_added = 0
  for(i in 1:length(configs_to_check)){
    optional_config_params = names(otherConfigs[[configs_to_check[i]]])
    params_to_add = optional_config_params[!(optional_config_params %in% default_config_params)]
    if(length(params_to_add) > 0){
      rlog::log_info(paste("Adding params",paste(params_to_add,collapse = ", ",sep=", ")))
      params_added = params_added + length(params_to_add)
      for(j in 1:length(params_to_add)){
        updatedConfig[[params_to_add[j]]] = otherConfigs[[configs_to_check[i]]][[params_to_add[j]]]
      }
    }
  }
  rlog::log_info(paste("Added",params_added,"params"))
  return(updatedConfig)
}
# compare param_collection with z
y = compareConfigs(defaultConfig = z,otherConfigs = paramCollection)
 #3 compare paramCollection with z and avoid deplication -- if param exists in z,
# skip the corresponding param in paramCollection

#####STEP2: Parse main.nf script to add params that are needed to run pipeline
### for DSL=2 NF scripts ... will need to look through other scripts
# check that param does not exist in main.nf
parse_nf_script <- function(nf_script){
  nf_lines = read.delim(nf_script,quote="",header=F)
  lines_of_interest = c()
  line_numbers = c()
  in_process_enclosure = FALSE
  out_process_enclosure = TRUE
  for(i in 1:nrow(nf_lines)){
    if(!in_process_enclosure){
      if(grepl("process",nf_lines[i,])){
        in_process_enclosure = TRUE
        out_process_enclosure = FALSE
      }
      if(grepl("params\\.",nf_lines[i,]) && !grepl("if",nf_lines[i,])  && !grepl("else",nf_lines[i,]) && !grepl("def",nf_lines[i,]) && !grepl("\\{params\\.",nf_lines[i,]) && !grepl("\\(params\\.",nf_lines[i,])){
        if(!grepl("task",nf_lines[i,])){
          rlog::log_info(paste("ADDING_NF_SCRIPT_LINE:",nf_lines[i,]))
          lines_of_interest = c(lines_of_interest,nf_lines[i,])
          line_numbers = c(line_numbers,i)
        }
      }
    } else{
      if(grepl("\\}",nf_lines[i,]) && !grepl("\\$",nf_lines[i,]) && !grepl("\\!",nf_lines[i,])){
        in_process_enclosure = FALSE
        out_process_enclosure = TRUE
      }
    }
  }
  param_list = c()
  for(j in 1:length(lines_of_interest)){
    tokenize_line = strsplit(lines_of_interest[j],"\\s+")[[1]]
    for(k in 1:1){
   # for(k in 1:length(tokenize_line)){
      sanitized_token = trimws(tokenize_line[k])
      if(grepl("^params\\.",sanitized_token)){
        split_check = strsplit(sanitized_token,"\\.")[[1]]
        param_names = c()
        ### remove additional chains done on the params to get the original param name
        for(l in 1:length(split_check)){
          if(!grepl("\\{",split_check[l]) && !grepl("\\(",split_check[l]) && !grepl("\\)",split_check[l])){
            punctuation_to_remove = str_extract(split_check[l],"(?![[\\]\\[\\_]])[[:punct:]]")
            punctuation_to_remove = punctuation_to_remove[!is.na(punctuation_to_remove)]
            punctuation_to_remove = punctuation_to_remove[punctuation_to_remove != "\""]
            punctuation_to_remove = punctuation_to_remove[punctuation_to_remove != "'"]
            if(length(punctuation_to_remove) >0){
              for(pidx in 1:length(punctuation_to_remove)){
                rlog::log_info(paste("REMOVING_PUNCTUATION:",punctuation_to_remove[pidx], "from",split_check[l]))
                split_check[l] = gsub(punctuation_to_remove[pidx],"",split_check[l])
              }
            }
            param_names = c(param_names,split_check[l])
          }
        }
        double_sanitized_token = paste(param_names,collapse=".")
        param_list = c(param_list,double_sanitized_token)
      }
    }
  }
  results = list()
  results[["lines_kept"]] = lines_of_interest
  results[["line_numbers"]] = line_numbers
  results[["params_found"]] = unique(param_list)
  return(results)
}
#nf_script = "/Users/keng/nf-core/sarek/main.nf"
nf_script_dat = read.delim(nf_script,quote="",header=F)
foi_result = parse_nf_script(nf_script =nf_script)
all_nf_edits = list()
##### STEP0: params then proess until we reach end of NF script
#### STEP1: compare foi_result[["params_found"]] with y ??? 
#### determines what params to add to our main NF script
##### STEP2:
##### scan all params to add and if it's a nested parameter --- parameter list, be sure
##### to initialize the parameter list. For Example, params.genome['myGenome'].bwa would 
##### need  params.genome =  [:]  and params.genome['myGenome'] = [:] before specifying the param
##### params.genome['myGenome'].bwa

### for each element in y determine if the parameter depends on an upstream parameter
params_to_check = names(y)
params_to_add_to_nf_script = params_to_check[!(params_to_check %in% foi_result[["params_found"]])]
#### try to figure out relative order that we should see these params in the main.nf script
#### known exception, this won't work well if your parameter references multiple parameters
#### 
param_order = list()
for( i in 1:length(params_to_check)){
  param_key_order = c()
  if(grepl("\\$\\{",y[[params_to_check[i]]])){
    rlog::log_info(paste("FOUND param to check:",params_to_check[i]))
    param_key_order = c(param_key_order,params_to_check[i])
    result = str_extract(y[[params_to_check[i]]], "(?<=\\{)[^\\}]+")
    param_key_order = c(param_key_order,result)
    if(!is.na(result)){
      param_check = TRUE
      while(param_check){
        if(!grepl("\\$\\{",y[[result]])){
          param_check = FALSE
        } else{
          result = str_extract(y[[result]], "(?<=\\{)[^\\}]+")
          if(!result %in% param_key_order){
            param_key_order = c(param_key_order,result)
          }
        }
      }
      rlog::log_info(paste(rev(param_key_order),collapse=" => "))
      param_order[[params_to_check[i]]] = rev(param_key_order)
    } else{
      rlog::log_info(paste("Are you sure about",params_to_check[i],"param to check:"))
    }
  } else{
    rlog::log_info(paste("NO need to check param:",params_to_check[i]))
    param_order[[params_to_check[i]]] = NULL
  }
}
#####
##### STEP0: params then proess until we reach end of NF script
# create appropriate data structures in main.nf (i.e. closures in configs -> lists in main.nf)
parameter_line_blocks  = split(foi_result[["line_numbers"]], cumsum( c(0, diff(foi_result[["line_numbers"]])>1) ) )
parameter_edits = list()
for( i in 1:length(params_to_add_to_nf_script)){
  if(params_to_add_to_nf_script[i] %in% names(param_order)){
      ### check to see if NF param is referenced in  script already,if so param needs to be defined after
    if(params_to_add_to_nf_script[i] %in% names(foi_result[["params_found"]])){
      line_number_of_interest = foi_result[["line_numbers"]][names(foi_result[["params_found"]]) == params_to_add_to_nf_script[i]]
      for(j in 1:length(names(parameter_line_blocks))){
        if(line_number_of_interest %in% parameter_line_blocks[[names(parameter_line_blocks[j])]]){
          parameter_edits[[params_to_add_to_nf_script[i]]] =  max(parameter_line_blocks[[names(parameter_line_blocks[j])]])
        }
      }
    } else{
      ### if not, find a parameter_line_block to start placing the params
      parameter_edits[[params_to_add_to_nf_script[i]]] = max(parameter_line_blocks[[1]])
    }
  } else{
    ### add anywhere in NF script
    parameter_edits[[params_to_add_to_nf_script[i]]] = max(parameter_line_blocks[[1]])
  }
}
##############

### fix bug parameter_edits and y is not equivalent
##################
complex_params_added = c()
for(i in 1:length(names(parameter_edits))){
  line_num = parameter_edits[[names(parameter_edits)[i]]]
  new_name = names(parameter_edits)[i]
  lines_to_add = c()
  if(grepl("\\[",new_name)){
    new_name = new_name
    new_name_tokens = strsplit( new_name , "\\.")[[1]]
    new_name_tokens_v2 = c()
    tokens_to_connect = c()
    connect_elements = FALSE
    for( t in 1:length(new_name_tokens)){
      if(grepl("\\[",new_name_tokens[t])){
        tokens_to_connect = c(tokens_to_connect,new_name_tokens[t])
        connect_elements = TRUE
      } else if(grepl("\\]",new_name_tokens[t])){
        tokens_to_connect = c(tokens_to_connect,new_name_tokens[t])
        new_name_tokens_v2 = c(new_name_tokens_v2,paste(tokens_to_connect,collapse="."))
        tokens_to_connect = c()
        connect_elements = FALSE
      } else if(connect_elements == TRUE){
        tokens_to_connect = c(tokens_to_connect,new_name_tokens[t])
        } else{
        new_name_tokens_v2 = c(new_name_tokens_v2,new_name_tokens[t])
      }
    }
    new_name_tokens = new_name_tokens_v2
    for(n in 1:length(new_name_tokens)){
      #test_split = strsplit(new_name_tokens[n],"[[:punct:]]")[[1]]
      test_split = strsplit(new_name_tokens[n],"\\[|]")[[1]]
      test_split = test_split[test_split!="" && !is.na(test_split)]
      rlog::log_info(paste("MY_SPLIT:",paste(test_split,collapse="---")))
      if(length(test_split) > 1){
        rlog::log_info(paste("INVESTIGATING_PARAM:",new_name))
        if(!(paste("params.",test_split[1],sep="") %in% complex_params_added)){
          line_to_add  = paste(paste("params.",test_split[1],sep=""),"=","[:]")
          rlog::log_info(paste("Initializing complex param:",paste("params.",test_split[1],sep="")))
          lines_to_add = c(lines_to_add,line_to_add)
          complex_params_added = c(complex_params_added,paste("params.",test_split[1],sep=""))
        }
        if(!(paste("params.",test_split[1],"[",test_split[2],"]",sep="") %in% complex_params_added)){
          line_to_add  = paste(paste("params.",test_split[1],"[",test_split[2],"]",sep=""),"=","[:]")
          lines_to_add = c(lines_to_add,line_to_add)
          rlog::log_info(paste("Initializing complex param:",paste("params.",test_split[1],"[",test_split[2],"]",sep="")))
          complex_params_added = c(complex_params_added,paste("params.",test_split[1],"[",test_split[2],"]",sep=""))
        }
      }
      
    }
    ### break down param name and check that the param is properly initialized
  } else{
    new_name = gsub("params\\.","",new_name)
  }
  expressions_to_add = c()
  if(grepl("false",y[[names(parameter_edits)[i]]])){
    y[[names(parameter_edits)[i]]] = gsub("\"","",y[[names(parameter_edits)[i]]])
  }
  logical_values = y[[names(parameter_edits)[i]]] != "null" && y[[names(parameter_edits)[i]]] != "true" && y[[names(parameter_edits)[i]]] != "false"
  other_conditions = grepl("/",y[[names(parameter_edits)[i]]])  
  must_have_condition = !grepl("'",y[[names(parameter_edits)[i]]])  
  dangling_single_quote  =str_extract_all(y[[names(parameter_edits)[i]]],"\"")[[1]]
  dangling_double_quote  =str_extract_all(y[[names(parameter_edits)[i]]],"'")[[1]]
  if(must_have_condition && (logical_values || other_conditions)){
    if(!grepl("/",y[[names(parameter_edits)[i]]])){
      if((length(dangling_single_quote) == 2  && length(dangling_double_quote) == 0)|| (length(dangling_single_quote) == 0 && length(dangling_double_quote) ==2)){
        expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
      } else if(length(dangling_single_quote) > 0 && length(dangling_single_quote) < 2){
        rlog::log_warn(paste("Working with",y[[names(parameter_edits)[i]]]))
        y[[names(parameter_edits)[i]]] = gsub("\"","",y[[names(parameter_edits)[i]]])
        expression_to_add = paste(new_name,"=",paste(y[[names(parameter_edits)[i]]],"'",sep=""))
      } else if(length(dangling_double_quote) > 0 && length(dangling_double_quote) < 2){
        y[[names(parameter_edits)[i]]] = gsub("'","",y[[names(parameter_edits)[i]]])
        expression_to_add = paste(new_name,"=",paste(y[[names(parameter_edits)[i]]],"\"",sep=""))
      } else if(!grepl("/",y[[names(parameter_edits)[i]]])){
        expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
      } else if(!grepl("\"",y[[names(parameter_edits)[i]]])){
        expression_to_add = paste(new_name,"=",paste("\"",y[[names(parameter_edits)[i]]],"\"",sep=""))
      } else{
        expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
      }
    } else{
      if((length(dangling_single_quote) == 2  && length(dangling_double_quote) == 0)|| (length(dangling_single_quote) == 0 && length(dangling_double_quote) ==2)){
        expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
      } else if(length(dangling_single_quote) > 0 && length(dangling_single_quote) < 2){
        rlog::log_warn(paste("Working with",y[[names(parameter_edits)[i]]]))
        y[[names(parameter_edits)[i]]] = gsub("\"","",y[[names(parameter_edits)[i]]])
        expression_to_add = paste(new_name,"=",paste(y[[names(parameter_edits)[i]]],"'",sep=""))
      } else if(length(dangling_double_quote) > 0 && length(dangling_double_quote) < 2){
        y[[names(parameter_edits)[i]]] = gsub("'","",y[[names(parameter_edits)[i]]])
        expression_to_add = paste(new_name,"=",paste(y[[names(parameter_edits)[i]]],"\"",sep=""))
      } else if(!grepl("/",y[[names(parameter_edits)[i]]])){
        expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
      } else if(!grepl("\"",y[[names(parameter_edits)[i]]])){
        expression_to_add = paste(new_name,"=",paste("\"",y[[names(parameter_edits)[i]]],"\"",sep=""))
      } else{
        expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
      }
    }
  } else{
    if((length(dangling_single_quote) == 2  && length(dangling_double_quote) == 0)|| (length(dangling_single_quote) == 0 && length(dangling_double_quote) ==2)){
      expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
    } else if(length(dangling_single_quote) > 0 && length(dangling_single_quote) < 2){
      rlog::log_warn(paste("Working with",y[[names(parameter_edits)[i]]]))
      y[[names(parameter_edits)[i]]] = gsub("\"","",y[[names(parameter_edits)[i]]])
      expression_to_add = paste(new_name,"=",paste(y[[names(parameter_edits)[i]]],"'",sep=""))
    } else if(length(dangling_double_quote) > 0 && length(dangling_double_quote) < 2){
      y[[names(parameter_edits)[i]]] = gsub("'","",y[[names(parameter_edits)[i]]])
      expression_to_add = paste(new_name,"=",paste(y[[names(parameter_edits)[i]]],"\"",sep=""))
    } else if(!grepl("/",y[[names(parameter_edits)[i]]])){
      expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
    } else if(!grepl("\"",y[[names(parameter_edits)[i]]])){
      expression_to_add = paste(new_name,"=",paste("\"",y[[names(parameter_edits)[i]]],"\"",sep=""))
    } else{
      expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
    }
  }
  
  if(!(paste(line_num) %in% names(all_nf_edits))){
    if(length(lines_to_add) > 0) {
      expressions_to_add = c(lines_to_add,expression_to_add)
    } else{
      expressions_to_add = c(expression_to_add)
    }
    rlog::log_info(paste("Initializing key:",paste(line_num)))
    all_nf_edits[[paste(line_num)]] = c()
    rlog::log_info(paste("Appending to key:",paste(line_num),"parameter:",new_name))
    all_nf_edits[[paste(line_num)]] = c(all_nf_edits[[paste(line_num)]],expressions_to_add)
  } else{
    rlog::log_info(paste("Appending to key:",paste(line_num),"parameter:",new_name))
    if(length(lines_to_add) > 0) {
      expressions_to_add = c(lines_to_add,expression_to_add)
    } else{
      expressions_to_add = c(expression_to_add)
    }
    all_nf_edits[[paste(line_num)]] = c(all_nf_edits[[paste(line_num)]],expressions_to_add)
  }
}

########################
#### update nf script with parameter updates 
updated_nf_file = gsub(".nf$",".ica.nf",nf_script)
new_nf_lines = c()
rlog::log_info(paste("ADDING UPDATED PARAMS to",updated_nf_file))
for(i in 1:nrow(nf_script_dat)){
  if(toString(i) %in% names(all_nf_edits)){
    new_nf_lines = c(new_nf_lines,nf_script_dat[i,])
    edits_to_add = all_nf_edits[[toString(i)]]
    for(j in 1:length(edits_to_add)){
      new_nf_lines = c(new_nf_lines,edits_to_add[j])
    }
  } else{
    new_nf_lines = c(new_nf_lines,nf_script_dat[i,])
  }
}
write.table(x=new_nf_lines,file=updated_nf_file,sep="\n",quote=F,row.names=F,col.names=F)
  
##########################################
classifyParameters <- function(paramsToXML){
  xmlSections = list()
  xmlSections[["dataInputs"]] = list()
  xmlSections[["parameterSettings"]] = list()
  xmlSections[["parameterSettings"]][["general"]] = list()
  for(i in 1:length(names(paramsToXML))){
    param_name = names(paramsToXML)[i]
    rlog::log_info(paste("LOOKING into",param_name))
    param_value = paramsToXML[[param_name]]
    param_final_name = gsub("params\\.","",param_name)
    if(grepl("\\$\\{",param_value)){
      #rlog::log_info(paste("LOOKING up",param_name))
      param_value = paramsFiller(list_to_fill=c(param_value),y)[1]
      #rlog::log_info(paste("VALUE is",param_value))
    }
    parameter_type = 'string'
    description = paste(parameter_type,"that defines",param_name)
    parameter_metadata = list()
    filename = basename(param_value)
    # initial algo will determine if parameter is string, int, boolean, or path
    if(grepl("/",param_value) || grepl("input",param_name)){
      simplfied_param_value = gsub("/","",param_value)
      rlog::log_info(paste("CHECKING if",param_name,"is file or folder. value:",param_name))
      if(simplfied_param_value != "" && !grepl("https://",param_value)  && !grepl("s3://",param_value) && !grepl("gs://",param_value) && !grepl("core.windows.net",param_value)){
        parameter_type = 'data'
        # if param value contains s3://, *.core.windows.net, or gs://, ignore, we won't put this in the XML
        # if path and contains file extension ('.') --- it's a file, if not it's a dir.
        #  or if the param contains 'dir' 
        data_type = "FOLDER"
        if(grepl("\\.",filename)){
          data_type = "FILE" 
        } 
        if(grepl("dir",param_name,ignore.case = TRUE)){
          data_type = "FOLDER"
        }
        if(!grepl("*",param_value) && !grepl("\\{",param_value) && !(grepl("\\[",param_name)) && !(grepl("\\$\\{",paramsToXML[[param_name]]))){
          rlog::log_info(paste("ADDING",param_name,"to dataInputs"))
          description = paste("Path",parameter_type,"that defines",data_type,param_name)
          parameter_metadata[["description"]] = description
          xmlSections[["dataInputs"]][[param_final_name]] = parameter_metadata
        } else{
          rlog::log_info(paste("ADDING",param_name,"to parameterSettings"))
          description = paste(parameter_type,"that defines",data_type,param_name)
          parameter_metadata[["description"]] = description
          parameter_metadata[["type"]] = parameter_type
          if(param_name != "params.help"){
            xmlSections[["parameterSettings"]][["general"]][[param_final_name]] = parameter_metadata
          }
        }
      }
      # all others will be put in options
      
    } else{
      if(!grepl("\\$\\{",param_value) && !(grepl("\\[",param_name))){
        if(!is.na(strtoi(param_value))){
          parameter_type = "integer"
          param_value = strtoi(param_value)
        }
        if(grepl("true",param_value,ignore.case=T) || grepl("false",param_value,ignore.case=T)){
          parameter_type = "boolean"
        }
        parameter_metadata[["description"]] = description
        parameter_metadata[["type"]] = parameter_type
        parameter_metadata[["default"]] = param_value
        if(param_name != "params.help"){
          xmlSections[["parameterSettings"]][["general"]][[param_final_name]] = parameter_metadata
        }
      }
    }
    if(parameter_type == "string"){
      if(!grepl("\\$\\{",param_value) && !(grepl("\\[",param_name))){
        if(!is.na(strtoi(param_value))){
          parameter_type = "integer"
          param_value = strtoi(param_value)
        }
        if(grepl("true",param_value,ignore.case=T) || grepl("false",param_value,ignore.case=T)){
          parameter_type = "boolean"
        }
        parameter_metadata[["description"]] = description
        parameter_metadata[["type"]] = parameter_type
        parameter_metadata[["default"]] = param_value
        if(param_name != "params.help"){
          xmlSections[["parameterSettings"]][["general"]][[param_final_name]] = parameter_metadata
        }
      }
    }
  }
  return(xmlSections)
}
#############
#getXMLSections = classifyParameters(paramsToXML = y)
#names(getXMLSections[["parameterSettings"]])
#names(getXMLSections[["dataInputs"]])
#################
### double-check or regenerate XML file ... which behavior to choose?

#parameters_xml = NULL
#generate_parameters_xml = TRUE
if(generate_parameters_xml){
  library(XML)
  getXMLSections = classifyParameters(paramsToXML = y)
  data_input_configurations = getXMLSections[["dataInputs"]]
  step_configurations = getXMLSections[["parameterSettings"]]
  rlog::log_info(paste("STEP3:Generating ICA XML based off of",nf_script))
  doc = newXMLDoc()
  #root = xmlRoot(doc)                                      # FIND ROOT
  #pipeline_node = newXMLNode("pipeline",parent=root)
  root = newXMLNode("pipeline",doc=doc)
  xmlAttrs(root) = c(code=paste(basename(dirname(nf_script)),"pipeline"),version="1.0",xmlns="xsd://www.illumina.com/ica/cp/pipelinedefinition")
  # add data inputs
  # create new sections for steps
  ## then add options for each section
  ############
  #code="left" format="FASTQ" type="FILE" required="true" multiValue="false"
  #<pd:dataInput code="ref" format="FASTA" type="FILE" required="true" multiValue="false">
  #  <pd:label>ref</pd:label>
  #  <pd:description>the value for File transcriptome</pd:description>
  #  </pd:dataInput>
  ############
  #############################
  #### add data inputs
  rlog::log_info(paste("STEP3a: Adding dataInputs"))
  dataInputsNode = newXMLNode("dataInputs",parent=root)
  dataInputNode = newXMLNode("dataInput",parent=dataInputsNode)
  if(length(data_input_configurations) >0){
    for(i in 1:length(names(data_input_configurations))){
      if(grepl("folder",data_input_configurations[[names(data_input_configurations)[i]]][["description"]],ignore.case=T)){
        xmlAttrs(dataInputNode) = c(code = names(data_input_configurations)[i] ,format = "UNKNOWN",type = "FOLDER",required = "true",multiValue = "true")   
      } else{
        xmlAttrs(dataInputNode) = c(code = names(data_input_configurations)[i] ,format = "UNKNOWN",type = "FILE",required = "true",multiValue = "true")   
      }
      newXMLNode("label", names(data_input_configurations)[i], parent=dataInputNode)
      newXMLNode("description", data_input_configurations[[names(data_input_configurations)[i]]][["description"]], parent=dataInputNode)
    }
  } else{
    rlog::log_info(paste("STEP3a: No dataInputs found"))
  }
  ############## add parameter options
  ## <pd:step execution="MANDATORY" code="General">
  #<pd:label>General</pd:label>
  #  <pd:description>General parameters</pd:description>
  #  <pd:tool code="generalparameters">
  #  <pd:label>generalparameters</pd:label>
  #  <pd:description>General Parameters</pd:description>
  
  #code="threads" minValues="1" maxValues="1" classification="USER"
  #
  #                <pd:parameter code="threads" minValues="1" maxValues="1" classification="USER">
  #                    <pd:label>threads</pd:label>
  #                    <pd:description>the value for threads</pd:description>
  #                    <pd:integerType/>
  #                    <pd:value>8</pd:value>
  #                </pd:parameter>
  ############  
  ########################
  rlog::log_info(paste("STEP3b: Adding parameter options"))
  stepsNode = newXMLNode("steps",parent=root)
  if(length(step_configurations)>0){
    for(i in 1:length(names(step_configurations))){
      initial_step_node = newXMLNode("step",parent=stepsNode)
      xmlAttrs(initial_step_node) = c(execution = "MANDATORY",code = names(step_configurations)[i])    
      step_label_node  = newXMLNode("label",names(step_configurations)[i],parent=initial_step_node)
      description_label_node  = newXMLNode("description",paste(names(step_configurations)[i],"parameters"),parent=initial_step_node)
      tool_description_node = newXMLNode("tool",parent=initial_step_node)
      xmlAttrs(tool_description_node) = c(code=paste(names(step_configurations)[i],"parameters"))
      nested_step_label_node  = newXMLNode("label",names(step_configurations)[i],parent=tool_description_node)
      nested_description_label_node  = newXMLNode("description",paste(names(step_configurations)[i],"parameters"),parent=tool_description_node)
      parameter_names = names(step_configurations[[names(step_configurations)[i]]])
      for(j in 1:length(parameter_names)){
        parameter_metadata = step_configurations[[names(step_configurations[i])]][[parameter_names[j]]]
        nested_parameter_node = newXMLNode("parameter",parent=tool_description_node)
        xmlAttrs(nested_parameter_node) = c(code = names(step_configurations[[names(step_configurations)[i]]])[j],minValues = "1",maxValues="1",classification="USER")
        newXMLNode("label",parameter_names[j],parent=nested_parameter_node)
        newXMLNode("description",parameter_metadata[["description"]],parent=nested_parameter_node)
        if(grepl("number",parameter_metadata[["type"]],ignore.case = T)){
          newXMLNode(paste("integer","Type",sep=""),parent=nested_parameter_node)
        } else{
          newXMLNode(paste(paste(parameter_metadata[["type"]],"Type",sep=""),sep=""),parent=nested_parameter_node)
        }
        if("default" %in% names(parameter_metadata)){
          newXMLNode("value",parameter_metadata[["default"]],parent=nested_parameter_node)
        } else{
          newXMLNode("value",parent=nested_parameter_node)
        }
      }
    }
  } else{
    rlog::log_info(paste("STEP3a: No parameters found"))
  }
  # VIEW XML
  #print(doc)
  
  # SAVE XML TO FILE
  outputFile = paste(basename(dirname(nf_script)), "pipeline","xml",sep=".")
  if(!is.null(parameters_xml)){
    outputPath = parameters_xml
  } else{
    outputPath = paste(dirname(nf_script),"/",outputFile,sep="")
  }
  rlog::log_info(paste("STEP4: Generating parameters XML to",outputPath))
  saveXML(doc, file=outputPath)
}


grabProcessMetadataFromConfigs <- function(conf_files,AllParams,default_config){
  processMetadata = list()
  processMetadata[["default"]] = list()
  processMetadata[["default"]][["errorStrategy"]] = "ignore"
  for(f in 1:length(conf_files)){
    conf_file = conf_files[f]
    conf_data = read.delim(conf_file,quote="",header=F)
    rlog::log_info(paste("READING IN:",conf_file))
    in_process_closure = FALSE
    out_process_closure = TRUE
    in_process_label = FALSE
    out_process_label = TRUE
    process_label_name = NULL
    ####################################
    for(l in 1:nrow(conf_data)){
      skip_line = FALSE # --- line is a comment
      line_split = strsplit(conf_data[l,],"\\s+")[[1]]
      clean_line = line_split
      for(t in 1:length(line_split)){
        sanitized_token = trimws(line_split[t])
        clean_line[t] = sanitized_token
      }
      clean_line = clean_line[clean_line!=""]
      if(grepl("/",clean_line[1])){
        skip_line = TRUE
      } else if(clean_line[1]=="process" && grepl("\\{",conf_data[l,])){
        in_process_closure = TRUE
        out_process_closure = FALSE
      } else if(grepl("withName",conf_data[l,]) || grepl("withLabel",conf_data[l,])){
        rlog::log_info(paste("PROCESS_LABEL_FOUND:",conf_data[l,]))
        rlog::log_info(paste("LINE_NUMBER:",l))
        in_process_label = TRUE
        out_process_label = FALSE
        process_label_name = strsplit(clean_line[1],"\\:")[[1]][2]
        processMetadata[[process_label_name]] = list()
        processMetadata[[process_label_name]][["errorStrategy"]] =  "ignore"
        rlog::log_info(paste("GETTING_METADATA_FOR_PROCESS_LABEL:",process_label_name))
      } else if(in_process_label && grepl("\\}",conf_data[l,]) && !grepl("=",conf_data[l,])){
        in_process_label = FALSE
        out_process_label = TRUE
        process_label_name = NULL
        rlog::log_info(paste("EXITING_PROCESS_LABEL_CLOSURE:",conf_data[l,]))
      } else if(in_process_closure && out_process_label  && grepl("\\}",conf_data[l,])  && !grepl("=",conf_data[l,]) ){
        in_process_closure = FALSE
        out_process_closure = TRUE
        rlog::log_info(paste("EXITING_PROCESS_CLOSURE:",conf_data[l,]))
      }
    ###########################
      if(in_process_closure && !skip_line){
        if(!in_process_label){
          rlog::log_info(paste("PROCESS_CLOSURE_LINE:",conf_data[l,]))
          if(grepl("\\=",conf_data[l,]) && (grepl("cpus",conf_data[l,]) || grepl("memory",conf_data[l,]) || grepl("container",conf_data[l,]))){
            ## fill in params if they are referenced in the expression
            line_to_evaluate = conf_data[l,]
            if(grepl("\\$\\{",line_to_evaluate)){
              line_to_evaluate = gsub("\\$","",line_to_evaluate)
              line_to_evaluate = gsub("\\{","",line_to_evaluate)
              line_to_evaluate = gsub("\\}","",line_to_evaluate)
            }
            for(k in 1:length(names(AllParams))){
              if(!grepl("\\'",names(AllParams)[k])){
                if(grepl(names(AllParams)[k],line_to_evaluate)){
                  rlog::log_info(paste("INITIAL_LINE:",line_to_evaluate))
                  #line_to_evaluate = paramsFiller(list_to_fill =  c(line_to_evaluate),params_list = AllParams)
                  line_to_evaluate = gsub(names(AllParams)[k],gsub("'","",AllParams[[names(AllParams)[k]]]),line_to_evaluate)
                  rlog::log_info(paste("TRANSLATED_LINE:",line_to_evaluate))
                }
              }
            }
            if(grepl("cpus",conf_data[l,])){
              cpus = str_extract(line_to_evaluate,"[[:digit:]]+")
              rlog::log_info(paste("CPUS_PARSED:",cpus))
              processMetadata[["default"]][["cpus"]] = cpus
            } else if(grepl("memory",conf_data[l,])){
              mem = str_extract(line_to_evaluate,"[[:digit:]]+.[[:alpha:]]+")
              rlog::log_info(paste("MEM_PARSED:",mem))
              processMetadata[["default"]][["mem"]] = mem
            } else if(grepl("container",conf_data[l,])){
              container = line_to_evaluate
              rlog::log_info(paste("CONTAINER_PARSED:",container))
              processMetadata[["default"]][["container"]] =  container
              } else{
              rlog::log_info(paste("NOT_PARSING_PROCESS_CLOSURE_LINE:",conf_data[l,]))
            }
          }
          
        } else{
          #print(processMetadata)
          rlog::log_info(paste("PROCESS_LABEL_NAME",process_label_name))
          print(process_label_name %in% names(processMetadata))
          rlog::log_info(paste("NAMES:",paste(names(processMetadata),collapse=", ")))
          #rlog::log_info(paste("PROCESS_LABEL_CLOSURE_LINE:",conf_data[l,]))
          if(grepl("\\=",conf_data[l,]) && (grepl("cpus",conf_data[l,]) || grepl("memory",conf_data[l,]) || grepl("container",conf_data[l,]))){
            ## fill in params if they are referenced in the expression
            line_to_evaluate = conf_data[l,]
            if(grepl("\\$\\{",line_to_evaluate)){
              line_to_evaluate = gsub("\\$","",line_to_evaluate)
              line_to_evaluate = gsub("\\{","",line_to_evaluate)
              line_to_evaluate = gsub("\\}","",line_to_evaluate)
            }
            for(k in 1:length(names(AllParams))){
              if(!grepl("\\'",names(AllParams)[k])){
                if(grepl(names(AllParams)[k],line_to_evaluate)){
                  rlog::log_info(paste("INITIAL_LINE:",line_to_evaluate))
                  #line_to_evaluate = paramsFiller(list_to_fill =  c(line_to_evaluate),params_list = AllParams)
                  line_to_evaluate = gsub(names(AllParams)[k],gsub("'","",AllParams[[names(AllParams)[k]]]),line_to_evaluate)
                  rlog::log_info(paste("TRANSLATED_LINE:",line_to_evaluate))
                }
              }
            }
            if(grepl("cpus",conf_data[l,])){
              cpus = str_extract(line_to_evaluate,"[[:digit:]]+")
              rlog::log_info(paste("PROCESS_LABEL_CPUS_PARSED:",cpus))
              processMetadata[[process_label_name]][["cpus"]] = cpus
            } else if(grepl("memory",conf_data[l,])){
              mem = str_extract(line_to_evaluate,"[[:digit:]]+.[[:alpha:]]+")
              rlog::log_info(paste("PROCESS_LABEL_MEM_PARSED:",mem))
              processMetadata[[process_label_name]][["mem"]] = mem
            } else if(grepl("container",conf_data[l,])){
              container = line_to_evaluate
              rlog::log_info(paste("PROCESS_LABEL_CONTAINER_PARSED:",container))
              processMetadata[[process_label_name]][["container"]] =  container
            } else{
              rlog::log_info(paste("NOT_PARSING_PROCESS_LABEL_CLOSURE_LINE:",conf_data[l,]))
            }
          }
        }
      }
    }
  }
  return(processMetadata)
}
nf_process_metadata = grabProcessMetadataFromConfigs(conf_file = final_config_list,AllParams= y)
print(nf_process_metadata)

### add DSL2 support for this process, adding different process metadata for ICA to 
### properly handle these workflows

getDefaultContainer <- function(config_file){
  defaultContainer = "null"
  config_dat = read.delim(config_file,quote="",header=F)
  for(l in 1:nrow(config_dat)){
    if(grepl("process.container",config_dat[l,]) && grepl("\\=",config_dat[l,])){
      line_split = strsplit(config_dat[l,],"\\s+")[[1]]
      clean_line = line_split
      for(j in 1:length(line_split)){
        clean_line[j] = trimws(line_split[j])
      }
      clean_line = clean_line[clean_line!=""]
      defaultContainer = clean_line[3]
    }
  }
  return(defaultContainer)
}

default_container = getDefaultContainer(config_file = config_file)
######## grabbing instance table info from the ICA GitBook
get_instance_type_table <- function(url){
  library(rvest)
  html = read_html(url)
  html_div_nodes = html %>% html_elements("div")
  nodes_that_have_table_data = html %>% html_elements("div") %>% html_attr("data-rnw-int-class")
  
  nodes_to_check = (1:length(html_div_nodes))[nodes_that_have_table_data == "table-row____"]
  nodes_to_check = nodes_to_check[!is.na(nodes_to_check)]
  
  computeTypes = FALSE
  lines_to_keep = c()
  for(i in 1:length(nodes_to_check)){
    text_of_interest = html_div_nodes[nodes_to_check[i]] %>% html_text2()
    cpuReference = FALSE
    if(grepl("Compute Type",text_of_interest)){
      computeTypes = TRUE
      
    } else if(grepl("cpu",text_of_interest) || grepl("small",text_of_interest) || grepl("medium",text_of_interest) || grepl("large",text_of_interest)){
      cpuReference = TRUE
    } else{
      computeTypes = FALSE
    }
    
    if(computeTypes || cpuReference){
      #print(html_attrs(html_div_nodes[nodes_to_check[i]]))
      #print(html_div_nodes[nodes_to_check[i]] %>% html_text2())
      lines_to_keep = c(lines_to_keep,text_of_interest)
    }
  }
  
  header_line = c()
  content_lines = c()
  for(line in 1:length(lines_to_keep)){
    if(line == 1){
      header_line = strsplit(lines_to_keep[line],"\n")[[1]]
    } else{
      content_lines = rbind(content_lines,strsplit(lines_to_keep[line],"\n")[[1]])
    }
  }
  colnames(content_lines) = header_line
  rownames(content_lines) = NULL
  return(data.frame(content_lines))
}
ica_instance_table = get_instance_type_table(url=instance_type_table_url)

################

###  need to figure out how to keep proper indentation when modifying a process

##STEP3: Add lines for each process that:
##  1) defines an instance-type --- use the Illumina GitBook for this  and  the configs to determine the appropriate cpu/ram settings 
## for each proess look for label, cpu, and or memory
## if for development, add a 'errorStrategy ignore' in our process
##  2) defines a docker image to run the instance on

getInstancePodAnnotation <- function(cpus,mem,container_name,ica_instance_table){
  pod_annotation_prefix = paste("pod annotation:", "'scheduler.illumina.com/presetSize'", "value:")
  pod_annotation = NULL
  pod_value = NA
  if(length(cpus) > 0 && length(mem) > 0){
    search_query = ica_instance_table$CPUs >= max(cpus) &&  ica_instance_table$`Mem..GB.` >= max(mem)
  } else if(length(cpus) > 0 && length(mem) == 0){
    search_query = ica_instance_table$CPUs >= max(cpus) 
  } else if(length(cpus) == 0 && length(mem) > 0){
    search_query = ica_instance_table$`Mem..GB.` >= max(mem)
  } else{
    pod_annotation = paste(pod_annotation_prefix,"'himem-small'")
    return(pod_annotation)
  }
  if(!is.null(container_name) && grepl("dragen",container_name)){
      pod_annotation = paste(pod_annotation_prefix,"'fpga-medium'")
    } else if(sum(search_query) > 0){
      pod_value = ica_instance_table[search_query,]$`Compute.Type`[1]
      pod_annotation = paste(pod_annotation_prefix,paste("'",pod_value,"'",sep=""))
      return(pod_annotation)
  } else{
    pod_annotation = paste(pod_annotation_prefix,"'himem-small'")
    return(pod_annotation)  
  }
}
######################################
parseProcessesInNextflowScript <- function(nf_script,nf_process_metadata,default_container,ica_instance_table){
  nf_script_process_lines = list()
  dsl2_enabled = FALSE
  in_process = FALSE
  out_process = TRUE
  process_has_input = FALSE
  process_is_labeled = FALSE 
  process_label = "default"
  process_name = NULL
  container_specified = FALSE
  process_container = default_container
  cpus = 1
  memory = 1
  line_indent = ""
  # include { UntarVegaReference } from './tools-vega.nf'
  skip_line = FALSE ### skip comment lines 
  ### first pass to see if this is a DSL2 script
  nf_script_dat = read.delim(nf_script,quote = "",header=F)
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
      if(grepl("nextflow.enable.dsl",nf_script_dat[i,])){
        if(clean_line[3] == "2"){
          dsl2_enabled = TRUE
        }
      }
    }
  }
  rlog::log_info(paste("script",nf_script,"is DSL2 enabled:",dsl2_enabled))
  #### now we will parse the nf script for processes, will collect line #s so that we know how to perform edits
  process_labels = c()
  line_numbers = c()
  process_cpus = c(cpus)
  process_memory = c(memory)
  process_lines = c()
  for(i in 1:nrow(nf_script_dat)){
    skip_line = FALSE       ### skip comment lines 
    line_split = strsplit(nf_script_dat[i,],"\\s+")[[1]]
    clean_line = line_split
    for(j in 1:length(line_split)){
      clean_line[j] = trimws(line_split[j])
    }
    clean_line = clean_line[clean_line!=""]
    clean_line_length = length(clean_line)
    if(grepl("/",clean_line[1])){
      skip_line = TRUE
    } 
    if(!skip_line){
      ### skip blank lines 
      if(length(clean_line)>0){
        line_numbers = c(line_numbers,i)
        if(clean_line[1] == "process"){
          in_process = TRUE
          out_process = FALSE
          process_name = clean_line[2]
          rlog::log_info(paste("FOUND_PROCESS_NAME:",process_name))
          nf_script_process_lines[[process_name]] = list()
          process_label = NULL
          container_specified = FALSE
          process_has_input = FALSE
          process_labels = c()
          line_numbers = c(i)
          process_cpus = c()
          process_memory = c()
          process_lines = c()
          process_container = default_container
          in_expression = FALSE
        } else if(in_process && (length(clean_line)>1) && ( clean_line[length(clean_line)] == "{" || grepl("if",nf_script_dat[i,]) || grepl("else",nf_script_dat[i,]) || grepl("def",nf_script_dat[i,]))){
          in_expression = TRUE
          rlog::log_info(paste("EXPRESSION_LINE:",nf_script_dat[i,]))
          #process_lines = c(process_lines,nf_script_dat[i,])
        } else if(in_process &&  (clean_line[1] == '"""'|| clean_line[1] == "}") && in_expression){
          in_expression = FALSE
          rlog::log_info(paste("EXPRESSION_LINE:",nf_script_dat[i,]))
          #process_lines = c(process_lines,nf_script_dat[i,])
        } else if(in_process && clean_line[1] == "}" && !in_expression){
          rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
          process_lines = c(process_lines,nf_script_dat[i,])
          rlog::log_info(paste("UPDATING_PROCESS",process_name))
          nf_script_process_lines[[process_name]][["process_lines"]] = process_lines
          nf_script_process_lines[[process_name]][["process_labels"]] = process_labels
          nf_script_process_lines[[process_name]][["line_numbers"]] = line_numbers
          nf_script_process_lines[[process_name]][["cpus_parsed"]] = process_cpus
          nf_script_process_lines[[process_name]][["memeory_in_gb_parsed"]] = process_memory
          in_process = FALSE
          out_process = TRUE
          process_name = NULL
          process_has_input = FALSE
          process_label = NULL
          container_specified = FALSE
          in_expression = FALSE
          process_labels = c()
          line_numbers = c()
          process_cpus = c()
          process_memory = c()
          process_lines = c()
          process_container = default_container
        }
        if(in_process){
          ## main goal is to replace any line containing label, cpu, memory, errorStrategy, queue
          ## with pod annotation, container --- if not found, publishDir --- if not found or if it's not an expression
          ## 
          #line_numbers = c(line_numbers,i)
          rlog::log_info(paste("PROCESS_LINE:",nf_script_dat[i,]))
          if(clean_line[1] == "label"){
            process_is_labeled = TRUE
            line_indent = gsub("label","",nf_script_dat[i,])
            process_label = gsub("\\'","",clean_line[2])
            rlog::log_info(paste("PROCESS_LABEL:",process_label))
            process_labels = c(process_labels,process_label)
          }
          if(clean_line[1] == "input:"){
            process_has_input = TRUE
            line_indent = gsub("input:","",nf_script_dat[i,])
            rlog::log_info(paste("LINE_INDENT_FIRST_TOKEN:",line_indent))
            ############
            process_labels_exist = length(process_labels) > 0 
            process_labels_metadata_exist = sum(process_labels %in% names(nf_process_metadata)) > 0
            process_name_metadata_exist = sum(process_name %in% names(nf_process_metadata)) > 0
            ######################
            if(process_labels_exist){
              if(!(process_label %in% names(nf_process_metadata))){
                rlog::log_warn(paste("NO_PROCESS_LABEL:","For process name:",process_name))
              }
            } else{
              rlog::log_warn(paste("NO_PROCESS_LABEL:","For process name:",process_name))
            }
            rlog::log_info(paste("FOUND_PROCESS_LABEL:",paste(process_labels,collapse=",")))
            #### adding container
            if(!container_specified){
              ### check if process label identifies container
              if(process_labels_exist && process_labels_metadata_exist){
                for(pl in 1:length(process_labels)){
                  if(process_labels[pl] %in% names(nf_process_metadata)){
                    if("container" %in% names(nf_process_metadata[[process_labels[pl]]])){
                      rlog::log_info(paste("ADDING container line:",nf_process_metadata[[process_labels[pl]]][["container"]]))
                      if(grepl("\\:",nf_process_metadata[[process_labels[pl]]][["container"]])){
                        container_selection_split = strsplit(nf_process_metadata[[process_labels[pl]]][["container"]],"\\s+")[[1]]
                        container_string = paste(line_indent,"container ",paste("'",gsub("\"","",container_selection_split[length(container_selection_split)]),"'",sep=""),sep="")
                        process_lines = c(process_lines,container_string)
                      } else{
                        process_lines = c(process_lines,nf_process_metadata[[process_labels[pl]]][["container"]])
                      }
                      container_specified = TRUE
                      break
                    }
                  }
                }
              }
              ### check if process name identifies container
              if(!container_specified){
                if(process_name_metadata_exist){
                  if("container" %in% names(nf_process_metadata[[process_name]])){
                    rlog::log_info(paste("ADDING container line:",nf_process_metadata[[process_name]][["container"]]))
                    if(grepl("\\:",nf_process_metadata[[process_name]][["container"]])){
                      container_selection_split = strsplit(nf_process_metadata[[process_name]][["container"]],"\\s+")[[1]]
                      container_string = paste(line_indent,"container ",paste("'",gsub("\"","",container_selection_split[length(container_selection_split)]),"'",sep=""),sep="")
                      process_lines = c(process_lines,container_string)
                    } else{
                      process_lines = c(process_lines,nf_process_metadata[[process_name]][["container"]])
                    }
                    container_specified = TRUE
                  }
                }
              }
              ### add default container
              if(!container_specified){
                rlog::log_info(paste("ADDING container line:",paste(line_indent,"container",process_container)))
                process_lines = c(process_lines,paste(paste(line_indent,"container",sep=""),process_container))
              }
              #########
              if(length(process_cpus) == 0){
                if(process_labels_exist && process_labels_metadata_exist){
                  for(pl in 1:length(process_labels)){
                    if(process_labels[pl] %in% names(nf_process_metadata)){
                      if("cpus" %in% names(nf_process_metadata[[process_labels[pl]]])){
                        process_cpus = c(process_cpus,nf_process_metadata[[process_labels[pl]]][["cpus"]])
                      }
                    }
                  }
                }
                if(process_name_metadata_exist){
                  if("cpus" %in% names(nf_process_metadata[[process_name]])){
                    process_cpus = c(process_cpus,nf_process_metadata[[process_name]][["cpus"]])
                  }
                }
              }
              if(length(process_memory) == 0){
                if(process_labels_exist && process_labels_metadata_exist){
                  for(pl in 1:length(process_labels)){
                    if(process_labels[pl] %in% names(nf_process_metadata)){
                      if("mem" %in% names(nf_process_metadata[[process_labels[pl]]])){
                        process_memory = c(process_memory,nf_process_metadata[[process_labels[pl]]][["mem"]])
                      }
                    }
                  }
                }
                if(process_name_metadata_exist){
                  if("mem" %in% names(nf_process_metadata[[process_name]])){
                    process_memory = c(process_memory,nf_process_metadata[[process_name]][["mem"]])
                  }
                }
              }
            }
            process_pod_annotation = getInstancePodAnnotation(cpus = process_cpus,mem = process_memory,container_name = process_container,ica_instance_table = ica_instance_table)
            rlog::log_info(paste("ADDING_POD_ANNOTATION:",process_pod_annotation))
            process_lines = c(process_lines,paste(line_indent,process_pod_annotation,sep=""))
            rlog::log_info(paste("ADDING_LEINENT_ERROR_STRATEGY:",paste("errorStrategy 'ignore'",sep="")))
            process_lines = c(process_lines,paste(line_indent,"errorStrategy 'ignore'",sep=""))
            rlog::log_info(paste("ADDING_TIME_COMPONENT:",paste("time","'1day'")))
            process_lines = c(process_lines,paste(line_indent,paste("time","'1day'"),sep=""))
            rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
            process_lines = c(process_lines,nf_script_dat[i,])
          } else if(clean_line[1] == "output:" && !process_has_input){
            line_indent = gsub("output:","",nf_script_dat[i,])
            rlog::log_info(paste("OUTPUT_LINE_INDENT_FIRST_TOKEN:",line_indent))
            ############
            process_labels_exist = length(process_labels) > 0 
            process_labels_metadata_exist = sum(process_labels %in% names(nf_process_metadata)) > 0
            process_name_metadata_exist = sum(process_name %in% names(nf_process_metadata)) > 0
            ######################
            if(process_labels_exist){
              if(!(process_label %in% names(nf_process_metadata))){
                rlog::log_warn(paste("NO_PROCESS_LABEL:","For process name:",process_name))
              }
            } else{
              rlog::log_warn(paste("NO_PROCESS_LABEL:","For process name:",process_name))
            }
            rlog::log_info(paste("FOUND_PROCESS_LABEL:",paste(process_labels,collapse=",")))
            #### adding container
            if(!container_specified){
              ### check if process label identifies container
              if(process_labels_exist && process_labels_metadata_exist){
                for(pl in 1:length(process_labels)){
                  if(process_labels[pl] %in% names(nf_process_metadata)){
                    if("container" %in% names(nf_process_metadata[[process_labels[pl]]])){
                      rlog::log_info(paste("ADDING container line:",nf_process_metadata[[process_labels[pl]]][["container"]]))
                      if(grepl("\\:",nf_process_metadata[[process_labels[pl]]][["container"]])){
                        container_selection_split = strsplit(nf_process_metadata[[process_labels[pl]]][["container"]],"\\s+")[[1]]
                        container_string = paste(line_indent,"container ",paste("'",gsub("\"","",container_selection_split[length(container_selection_split)]),"'",sep=""),sep="")
                        process_lines = c(process_lines,container_string)
                      } else{
                        process_lines = c(process_lines,nf_process_metadata[[process_labels[pl]]][["container"]])
                      }
                      container_specified = TRUE
                      break
                    }
                  }
                }
              }
              ### check if process name identifies container
              if(!container_specified){
                if(process_name_metadata_exist){
                  if("container" %in% names(nf_process_metadata[[process_name]])){
                    rlog::log_info(paste("ADDING container line:",nf_process_metadata[[process_name]][["container"]]))
                    if(grepl("\\:",nf_process_metadata[[process_name]][["container"]])){
                      container_selection_split = strsplit(nf_process_metadata[[process_name]][["container"]],"\\s+")[[1]]
                      container_string = paste(line_indent,"container ",paste("'",gsub("\"","",container_selection_split[length(container_selection_split)]),"'",sep=""),sep="")
                      process_lines = c(process_lines,container_string)
                    } else{
                      process_lines = c(process_lines,nf_process_metadata[[process_name]][["container"]])
                    }
                    container_specified = TRUE
                  }
                }
              }
              ### add default container
              if(!container_specified){
                rlog::log_info(paste("ADDING container line:",paste(line_indent,"container",process_container)))
                process_lines = c(process_lines,paste(paste(line_indent,"container",sep=""),process_container))
              }
              #########
              if(length(process_cpus) == 0){
                if(process_labels_exist && process_labels_metadata_exist){
                  for(pl in 1:length(process_labels)){
                    if(process_labels[pl] %in% names(nf_process_metadata)){
                      if("cpus" %in% names(nf_process_metadata[[process_labels[pl]]])){
                        process_cpus = c(process_cpus,nf_process_metadata[[process_labels[pl]]][["cpus"]])
                      }
                    }
                  }
                }
                if(process_name_metadata_exist){
                  if("cpus" %in% names(nf_process_metadata[[process_name]])){
                    process_cpus = c(process_cpus,nf_process_metadata[[process_name]][["cpus"]])
                  }
                }
              }
              if(length(process_memory) == 0){
                if(process_labels_exist && process_labels_metadata_exist){
                  for(pl in 1:length(process_labels)){
                    if(process_labels[pl] %in% names(nf_process_metadata)){
                      if("mem" %in% names(nf_process_metadata[[process_labels[pl]]])){
                        process_memory = c(process_memory,nf_process_metadata[[process_labels[pl]]][["mem"]])
                      }
                    }
                  }
                }
                if(process_name_metadata_exist){
                  if("mem" %in% names(nf_process_metadata[[process_name]])){
                    process_memory = c(process_memory,nf_process_metadata[[process_name]][["mem"]])
                  }
                }
              }
            }
            process_pod_annotation = getInstancePodAnnotation(cpus = process_cpus,mem = process_memory,container_name = process_container,ica_instance_table = ica_instance_table)
            rlog::log_info(paste("ADDING_POD_ANNOTATION:",process_pod_annotation))
            process_lines = c(process_lines,paste(line_indent,process_pod_annotation,sep=""))
            rlog::log_info(paste("ADDING_LEINENT_ERROR_STRATEGY:",paste("errorStrategy 'ignore'",sep="")))
            process_lines = c(process_lines,paste(line_indent,"errorStrategy 'ignore'",sep=""))
            rlog::log_info(paste("ADDING_TIME_COMPONENT:",paste("time","'1day'")))
            process_lines = c(process_lines,paste(line_indent,paste("time","'1day'"),sep=""))
            rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
            process_lines = c(process_lines,nf_script_dat[i,])
            } else if(clean_line[1] == "cpus"){
            line_indent = gsub("cpus","",nf_script_dat[i,])
            cpus = stroi(clean_line[2])
            process_cpus = c(process_cpus,cpus)
          } else if(clean_line[1] == "memory"){
            line_indent = gsub("memory","",nf_script_dat[i,])
            memory = stroi(gsub("\\.GB","",clean_line[2]))
            process_memory = c(process_memory,memory)
          } else if(clean_line[1] == "container"){
            line_indent = gsub("container","",nf_script_dat[i,])
            container_specified = TRUE
            process_container = clean_line[2]
            rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
            process_lines = c(process_lines,nf_script_dat[i,])
          } else if(clean_line[1] == "publishDir"){
            line_indent = gsub("publishDir","",nf_script_dat[i,])
            publish_dir_metadata = str_extract(nf_script_dat[i,],"(?<=\\()[^\\)]+")
            if(!is.na(publish_dir_metadata)){
              publish_dir_metadata_location = strsplit(publish_dir_metadata,"\\,")[[1]][1]
              publish_dir_metadata_location_split = strsplit(publish_dir_metadata_location,"/")[[1]]
              if(publish_dir_metadata_location == "out" || "out" %in% publish_dir_metadata_location_split){
                rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
                process_lines = c(process_lines,nf_script_dat[i,])
              } else{
                rlog::log_info(paste("ADDING_custom_line:",nf_script_dat[i,]))
                process_lines = c(process_lines,paste(line_indent,"publishDir","out","mode: copy") )
              }
            }
            ### check that publishDir is being sent to an expression
            else if(grepl("\\$\\{",nf_script_dat[i,])){
              process_lines = c(process_lines,nf_script_dat[i,])
            } else if(clean_line[2] == "out" || clean_line[2] == "'out'"){
              rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
              process_lines = c(process_lines,nf_script_dat[i,])
            }
          } else if(clean_line[1] != "queue" && clean_line[1] != "label" && clean_line[1] != "errorStrategy" && clean_line[1] != "time" && clean_line[1] != "publishDir"){
            rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
            process_lines = c(process_lines,nf_script_dat[i,])
          } else{
            if(clean_line[1] != "queue" && clean_line[1] != "label"){
              rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
              process_lines = c(process_lines,nf_script_dat[i,])
            }
          }
        }
      }
    } else{
      if(in_process){
        rlog::log_info(paste("NOT_PARSING_LINE:",nf_script_dat[i,]))
        rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
        process_lines = c(process_lines,nf_script_dat[i,])
        line_numbers = c(line_numbers,i)
      }
    }
  }
  return(nf_script_process_lines)
}

updated_nf_processes = parseProcessesInNextflowScript(nf_script=updated_nf_file,nf_process_metadata=nf_process_metadata,default_container = default_container,ica_instance_table = ica_instance_table)
print(updated_nf_processes[["VEP"]])

############
## check out the updated processes and the line #s. Keep what isn't a process-line # (i.e. line # defining a process)
## when you find a process, update it

# get first line # of each process to determine the order in which we'll update our updated_nf_file
# this will inform us of lines outside the process that we'll want to keep
process_updated_nf_file = gsub(".nf$",".dev.nf",updated_nf_file)
key_order = c()
second_pass_updated_lines = c()
for(i in 1:length(names(updated_nf_processes))){
  line_num = min(updated_nf_processes[[names(updated_nf_processes)[i]]][["line_numbers"]])
  key_order[i] = line_num
}
process_order = names(updated_nf_processes)[order(key_order)]
updated_nf_dat = read.delim(updated_nf_file,quote="",header=F)
previous_line_count = 0
for(pidx in 1:length(process_order)){
  current_process = updated_nf_processes[[process_order[pidx]]]
  current_process_new_lines = current_process[["process_lines"]]
  current_process_line_numbers = current_process[["line_numbers"]]
  if(length(current_process_new_lines) == 0){
    rlog::log_warn(paste("NO_LINES_FOUND_FOR_PROCESS",process_order[pidx]))
  }
  lower_line_count_bound = 0
  upper_line_count_bound = min(current_process_line_numbers)
  if(pidx > 1){
    lower_line_count_bound = max(updated_nf_processes[[process_order[pidx-1]]][["line_numbers"]])
  }
  rlog::log_info(paste("LOWER_LINE_BOUND",lower_line_count_bound,"UPPER_LINE_BOUND",upper_line_count_bound))
  lines_to_add = TRUE
  for(lidx in 1:nrow(updated_nf_dat)){
    if(!lines_to_add){
      break
    }
    if(lidx < upper_line_count_bound && lidx > lower_line_count_bound){
      second_pass_updated_lines = c(second_pass_updated_lines,updated_nf_dat[lidx,])
    } else if(lidx == min(current_process_line_numbers)){
        for(k in 1:length(current_process_new_lines)){
          second_pass_updated_lines = c(second_pass_updated_lines,current_process_new_lines[k])
        }
      lines_to_add = FALSE
    }
  }
  current_line_count = length(second_pass_updated_lines)
  rlog::log_info(paste("ADDED",current_line_count-previous_line_count,"lines from process:",process_order[pidx]))
  previous_line_count = current_line_count
}
##### copy intermediate files
#### get the rest of the script
line_count_to_finish = max(current_process_line_numbers) + 1
if(line_count_to_finish < nrow(updated_nf_dat)){
  for(lidx in line_count_to_finish:nrow(updated_nf_dat)){
    second_pass_updated_lines = c(second_pass_updated_lines,updated_nf_dat[lidx,])
  }
}
rlog::log_info(paste("Writing updated processes to",process_updated_nf_file))
write.table(x=second_pass_updated_lines,file=process_updated_nf_file,sep="\n",quote=F,row.names=F,col.names=F)
###############
#STEP4: Check for file_paths based-off the parameter XML file to ensure that they are
# wrapped by file(myChannel1)