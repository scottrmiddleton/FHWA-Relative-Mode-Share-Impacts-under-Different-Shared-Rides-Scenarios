
# Summary:

This repository contains the source code of the scenario analysis tool developed through the Federal Highway Administration research project Analysis of Travel Choices and Scenarios for Sharing Rides. The source code was programmed in R using the RShiny package.   

# Organizational Outline:
* Project Description
* Prerequisites
* Usage
* Additional Notes
* Version History and Retention
* License
* Contributions
* Contact Information
* Acknowledgements

# Project Description: Analysis of Travel Choices and Scenarios for Sharing Rides

This repository contains analytical code developed for the FHWA sponsored project on Analysis of Travel Choices and Scenarios for Sharing Rides. Using survey data from a large Transportation Network Company (TNC) and other data sources, the project formulated various scenarios with the intention of better understanding use of higher-occupancy travel options, including ridesharing and carpooling. The scenarios incorporated research findings on the use of shared rides in the context of TNCs and also price changes for personal vehicle travel.  The analytical code allows a user to understand how changes in trip cost and travel time affect shared ride choices for various market segmentations. The code is created in R Shiny and is available to users as described in this README document. 

# Prerequisites

Requires:
- RStudio Deskop (Version 1.2.1335 or higher)
- Required RStudio packages: shiny, tidyverse, plotly, rsconnect, shinyBS, scales, scales, reshape2, basictabler, shinydashboard

### Installing

Step 1. If not already available, download and install the RStudio development environment using the instructions available here: https://rstudio.com/products/rstudio/download/

Step 2. To install required packages, open RStudio and run "install.packages(c(	"shiny"","tidyverse","plotly","rsconnect","shinyBS","scales","reshape2",
"basictabler","shinydashboard"))"

# Usage

- In RStudio, open the file "policy_tool.Rproj"
- In the console, enter the code: "shiny::runApp()" without the quotation marks.
- Tool will open locally, with the option to view in browser. 
- All required files are included in the zipped folder. No files other than 
"policy_tool.Rproj" need to be opened for tool to run correctly.

## Building
Code does not require any type of build/compilation.

## Testing
Test cases are automatically run as part of the build process.

## Execution
Tool can be used directly in RStudio. User will be able to modify scenario details and run analysis by following the point-and-click instructions in the associated report and using the tool tips indicated by question marks in the user interface. 

# Additional Notes

All required files are included in the zipped folder. No files other than 
"policy_tool.Rproj" need to be opened for tool to run correctly. Tool will not work correctly in Internet Explorer, but can be used in other major browsers as well as in RStudio.  

## Required Files
The zipped folder includes the following files:

R Files:
- policy_tool.Rproj
- server.R
- ui.R

Data Files:
- elasticities.csv
- mode_share_by_city.csv
- trip_cost_by_city.csv
- vmt_share_by_segment.csv

Image Files (in www folder):
- tree_diagram.png
- question.png

All files are required for the tool to run correctly. 

# Version History and Retention

**Status:** This project is in the release phase.

**Release Frequency:** This project is not intended for regular update

**Release History: See [CHANGELOG.md](CHANGELOG.md)**

# License

This project is licensed under the Creative Commons 1.0 Universal (CC0 1.0).  

# Contributions

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our Code of Conduct, the process for submitting pull requests to us, and how contributions will be released.

# Contact Information

Contact Name: Allen Greenberg
Contact Information: Allen.Greenberg@dot.gov,  (202) 366-2425

## Authors

Scott Middleton
EBP US 
155 Federal Street, Suite 600, Boston, MA 02110
scott.middleton@ebp-us.com tel: 617-338-6775, x209, fax: 617-338-1174

Kyle Schroeckenthaler
EBP US 
155 Federal Street, Suite 600, Boston, MA 02110
kyle.schroeckenthaler@ebp-us.com, tel: 617-338-6775, x221, fax: 617-338-1174

Deepak Gopalakrishna
ICF International 
9300 Lee Hwy, Fairfax, VA 22031
Deepak.Gopalakrishna@icf.com, tel: 202-862-1564

# Acknowledgements

To track how this government-funded code is used, we request that if you decide to build additional software using this code please acknowledge its Digital Object Identifier in your software’s README/documentation. Digital Object Identifier: https://doi.org/10.21949/1520429

