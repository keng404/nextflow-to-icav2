# IMPLEMENTATION TODO LIST:
- [X] 1) properly identify glob expressions or expressions as strings and not input paths
- [ ] 2) relative / absolute file paths in DSL 1 NF scripts
- [X] 3) get default container
- [ ] 4) create lists in parameters XML where appropriate for the nf-core pipelines. 
 	```nextflow_schema.json``` files --under 'properties' have 'list' and 'enum' fields that can be used.
- [ ] 5) add dummy process at end of script that:
      	       	- [ ] copies intermediate results and logs for dev work
		- [ ] grabs trace logs
- [X] 6) NF script updates
      	    	- [X] update params in main script and write updated script
		- [X] parse processes and propose updates saved in another object
		- [X] read updated script and whenever there's a process, update this script
- [X] 7) parse NFscripts for process enclosures
		- [X] look for labels, cpu, memory  ( edits where we remove)
		- [X] add pod anotation, container references when needed (edits where we add)
		  - How to manage processes with no input expression? -- second pass?
		  - How to select for process container where configurations point to a Groovy expression?
- [ ] 8) add DSL2 support
 		- might not have to worry about 2)
 		- make sure to track and properly edit multiple NF scripts that get referenced

# DEVOPS TODO LIST:
- [X]  *install_packages.R* script that defines libraries to install
- [ ]   test script to perform dummy ```nextflow run``` command
- [X]   script containing CLI commands to create NF workflows in an ICA domain --- you must be logged in
- [ ]   demo/test containing CLI commands to stage data for testing out these workflows
- [ ]   script containing CLI commands to create project analysis runs for these workflows
- [X] 	Docker image that contains all the scripts, binaries, and libraries ready for all users to use thes scripts without having to worry about any setup
