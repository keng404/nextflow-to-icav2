# IMPLEMENTATION TODO LIST:
- [ ] 1) properly identify glob expressions or expressions as strings and not paths
- [ ] 2) relative / absolute file paths in DSL 1 NF scripts
- [X] 3) get default container
- [ ] 4) NF script index --- by line
		- keep original indexes ??
		- Or update indexes of the script and the indexes edits our script is propoosing
- [ ] 5) parse NFscripts for process enclosures
		- [X] look for labels, cpu, memory  ( edits where we remove)
		- [X] add pod anotation, container references when needed (edits where we add)
		  - How to manage processes with no input expression? -- second pass?
		  - How to select for process container where configurations point to a Groovy expression?
- [ ] 6) add DSL2 support
 		- might not have to worry about 2)
 		- make sure to track and properly edit multiple NF scripts that get referenced

# DEVOPS TODO LIST:
- [X]  *install_packages.R* script that defines libraries to install
- [ ]   test script to perform dummy ```nextflow run``` command
- [ ]   script containing CLI commands to create NF workflows in an ICA domain --- you must be logged in
- [ ]   demo/test ontaining CLI commands to stage data for testing out these workflows
- [ ]   script containing CLI commands to create project analysis runs for these workflows
- [ ] 	Docker image that contains all the scripts, binaries, and libraries ready for all users to use thes scripts without having to worry about any setup
