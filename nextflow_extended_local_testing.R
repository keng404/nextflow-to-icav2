options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
library(rlog)
library(stringr)
library(XML)
source('parameter_xml_utils.R')
# create parser object
parser <- ArgumentParser()
parser$add_argument("-x","--parameters-xml-file","--parameters_xml_file",
                    default=NULL, help = " parameters XML file output",required=TRUE)
# step 1 : parse XML 
################
# test nextflow run (pipeline) prior to running on ICA ---- not just main script:
#   - parse XML for parameters
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()
parameters_xml_file = args$parameters_xml_file
if(!file.exists(parameters_xml_file)){
  stop(paste("Please define a valid path to th4e parameter XML file"))
}
#########
createDummyInputs <- function(my_input_list){
  my_dummy_inputs = list()
  for(i in 1:length(names(my_input_list))){
    rlog::log_info(paste("Param_name:",names(my_input_list)[i]))
    my_dummy_inputs[[names(my_input_list)[i]]] = paste(names(my_input_list)[i],paste("dummy_file.",names(my_input_list)[i],sep=""),sep="/")
  }
  return(my_dummy_inputs)
}
stageInputs <- function(my_dummy_inputs){
  for(i in 1:length(names(my_dummy_inputs))){
    file_path_of_interest = my_dummy_inputs[[names(my_dummy_inputs)[i]]] 
    dir_of_interest  = dirname(file_path_of_interest)
    if(!dir.exists(dir_of_interest)){
      rlog::log_info(paste("Creating directory:",dir_of_interest))
      #system(paste("mkdir",dir_of_interest))
    } else{
      rlog::log_info(paste("No need to create directory:",dir_of_interest))
    }
    if(!file.exists(file_path_of_interest)){
      rlog::log_info(paste("Creating file:",file_path_of_interest))
      #system(paste("touch",file_path_of_interest))
    } else{
      rlog::log_info(paste("No need to create file:",file_path_of_interest))
    }
  }
}
# parse XML file
xml_doc =  xmlTreeParse(parameters_xml_file,addAttributeNamespaces = TRUE,useInternalNodes = TRUE)

rlog::log_info(paste("Looking for parameters in :",parameters_xml_file))
parameter_list = getParametersFromXML(xml_doc)
rlog::log_info(parameter_list)

rlog::log_info(paste("Looking for dataInputs in :",parameters_xml_file))
data_input_list = getDataInputsFromXML(xml_doc)
rlog::log_info(data_input_list)
# - create dummy files and stage them in current working directory
dummy_inputs = createDummyInputs(data_input_list)
stage_inputs = stageInputs(dummy_inputs)
# - craft nextflow run command
# - identify modules with:
#   - missing pod annotation labels
# - missing cpus and memory declarations
# - missing maxForks declarations
################
