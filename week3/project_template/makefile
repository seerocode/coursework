# Comments that begin with ## will be shown from target help

## This is template about how to use make automate the workflow of R project

<<<<<<< HEAD
# Add to Windows Path: Open Windows Explorer; RClick on This Computer; Advanced System Settings; Advanced Tab; Environment variable; For the User change Path to add "C:\Program Files\RStudio\bin\pandoc" (quotes not needed) then "C:\Program Files\Microsoft\MRO-3.3.1\bin"
=======
# Make sure that Rscript.exe and pandoc are in your path (see Readmd.md for directions)
>>>>>>> 4c1311f18b80773052258b9264b35cee6a613a58

.PHONY: list help all
help : 
	@echo "Showing Help Comments:"
	@echo
	@sed -n 's/^##//p' makefile
	@printf "\nList of all targets: "
	@$(MAKE) -s list

# List targets (http://stackoverflow.com/a/26339924/3429373)
list:
	@$(MAKE) -pRrq -f $(lastword $(MAKEFILE_LIST)) : 2>/dev/null | awk -v RS= -F: '/^# File/,/^# Finished Make data base/ {if ($$1 !~ "^[#.]") {print $$1}}' | sort | egrep -v -e '^[^[:alnum:]]' -e '^$@$$' | xargs

outputs.RData: analysis.R
<<<<<<< HEAD
	RScript analysis.R

writeup.html: writeup.Rmd outputs.RData
	RScript -e "rmarkdown::render('writeup.Rmd')"
=======
	Rscript.exe analysis.R

writeup.html: writeup.Rmd outputs.RData
	Rscript.exe -e "rmarkdown::render('writeup.Rmd')"
>>>>>>> 4c1311f18b80773052258b9264b35cee6a613a58

all: writeup.html
