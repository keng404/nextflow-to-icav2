# nextflow-to-icav2
R-based helper scripts to generate XML files and modifications to NF scripts for ICAv2 compatiblity.
This is meant to be a developer tool to help them develop Nextflow pipelines that will run successfully on ICA

What these scripts do is parse configuration files and the main NF script of a pipeline and update the underlying processes with what's mentioned below.
Additionally parameters mentioned in these configuration files that are not referenced in the main NF file are brought into the main NF script. As ICA does not
allow you to use your own configuration files currently

Nextflow workflows on ICA are orchestrated by kubernetes and require a parameters XML file 
- containing data inputs (i.e. files + folders) and other string-based options for all configurable parameters to properly be passed from ICA to your Nextflow workflows
- processes will need to contain a reference to a container --- a Docker image that will run that specific process
- processes will need a  ```pod annotation``` specified for ICA to know what instance type to run the process.
  - A table of instance types and the associated CPU + Memory specs can be found [here](https://illumina.gitbook.io/ica/project/p-flow/f-pipelines#compute-types)  

These scripts have been made to be compatible with [nf-core](https://github.com/nf-core) workflows

The scripts mentioned below can be run in a docker image ```keng404/nfcore-to-icav2:0.0.7```

You'll first need to download the python module from nf-core via a ```pip install nf-core``` command
Then you can use nf-core list --json to return a JSON metadata file containing current pipelines in the nf-core repository. You can choose which pipelines to  ```git clone``` but as a convenience, the wrapper ```nf-core.conversion_wrapper.R ``` will perform a git pull, parse nextflow_schema.json files and generate parameter XML files, and then read configuration and nextflow scripts and make some initial modifications for ICA development. Lastly these pipelines are created in an ICA project of your choosing. So you will need to generate and download an API key from the ICA domain of your choosing.

```bash
Rscript nf-core.conversion_wrapper.R --input {PIPELINE_JSON_FILE} --staging_directory {DIRECTORY_WHERE_NF_CORE_PIPELINES_ARE_LOCATED} --run-scripts {DIRECTORY_WHERE_THESE_R_SCRIPTS_ARE_LOCATED}  --intermediate-copy-template {DIRECTORY_WHERE_THESE_R_SCRIPTS_ARE_LOCATED}/dummy_template.txt --create-pipeline-in-ica --api-key-file {API_KEY_FILE} --ica-project-name {ICA_PROJECT_NAME}--nf-core-mode 
```

In summary, you will need the following prerequisites, either to run the wrapper referenced above or to carry out individual steps below.
- 1) ```git clone``` nf-core pipelines of interest
- 2) Install the python module ```nf-core`` and create a JSON file using the command line ``` nf-core list --json > {PIPELINE_JSON_FILE}```

** For non nf-core pipelines, you can try to generate a parameters XML and modify the NF scripts of your pipeline by using the ```nf-core.ica_mod_nf_script.R ``` script. Just be sure to add the ```generate-xml``` flag to enable the creation of an XML file and ```--enable-dsl2``` flag to have the script appropriately handle Nextflow workflows implemented in DSL2.


**DSL2 compatibility is now implemented but needs to be tested (i.e. successful pipeline runs)**

# To generate an XML file from nf-core pipeline
```bash
Rscript nf-core.json_to_params_xml.R --json {PATH_TO_SCHEMA_JSON}
```
- A Nextflow schema JSON is generated by nf-core's python library nf-core
- nf-core can be installed via a ```pip install nf-core``` command
```bash
nf-core schema build -d {PATH_NF-CORE_DIR}
```

# To generate an XML file and edits to Nextflow script, use the following template
```bash
Rscript  nf-core.ica_mod_nf_script.R --nf-script {MAIN_NF_SCRIPT} --nf-config {DEFAULT_NF_CONFIG}  [OPTIONAL: --parameters-xml {PATH} or --generate-parameters-xml] --intermediate-copy-template {PATH_TO_RSCRIPTS}/dummy_template.txt
```
Specifying the ```--parameters-xml``` parameter tells the ```nf-core.ica_mod_nf_script.R``` to generate an XML files based on the NF script and config you specify.
Default behavior is to traverse the config file you provide and parse additional config files that might be referenced. Not setting the ```--parameters-xml``` flag will tell the script to just focus on making edits to NF script.

For DSL2-enabled workflows add '--dsl2-enabled' to your command line.

This script will first parse your DEFAULT_NF_CONFIG and try to create a dictionary of most of the configurations in this file as well as any additional configs referenced in DEFAULT_NF_CONFIG via an ```includeConfig 'path-to-config'``` statement. If your configuration file is not as complex, add the flag ```--is-simple-config``` as a workaround to avoid the script error-ing out when trying to build this configuration dictionary.

A current list of todos for this script is [here](https://github.com/keng404/nextflow-to-icav2/blob/master/todos.nf_editing_for_icav2.md)

# To create a pipeline in ICA, you can use the following helper script ```nf-core.create_ica_pipeline.R```
```bash
Rscript nf-core.create_ica_pipeline.R --nextflow-script {NF_SCRIPT} --workflow-language nextflow --parameters-xml {PARAMETERS_XML} --nf-core-mode --ica-project-name {NAME} --pipeline-name {NAME} --api-key-file {PATH_TO_API_KEY_FILE}
```

Add the flag ```--simple-mode``` if you have custom groovy libraries or modules files your workflow references. What this script will do when this flag is specified is to upload these files and directories to ICA and to update the parameter XML file to allow you to specify directories under the parameters project_dir and files under input_files. This will ensure that these files and directories will be placed in the workflow.launchDir when the pipeline is invoked.

# As a convenience, one can also get a templated CLI command to help them run a pipeline in ICA via the following:
```bash
Rscript launch_pipeline_mock_pipeline_cli.R --pipeline-name {PIPELINE_NAME} --workflow-language {xml or nextflow} --parameters-xml {PATH_TO_PARAMETERS_XML}
```

By default, this script will automatically try to upload all files found in the same directory as your {NF_SCRIPT}, excluding any nextflow config files (i.e. *config)

