# R25-TrafficIncidentDetectionGeneralization
ROADII Use Case 25 - Traffic Incident Detection through ML methods

This repository contains code and documentation for explorations in the generalization and deployment of
techniques for detecting traffic incidents and assessing roadway network vulnerabilities using 
real-time and historical datasets. Intended audiences are state DOTs and traffic management centers 
wanting to improve incident response rates and understand how traffic incidents affect the larger
roadway network.

## Contents
1. Various folders and files for public-facing informational use case website.

    These files and folders contain the code necessary for deployment of a static website that provides information about the development of the solutions housed in this repository. The website provides information in an interactive form-factor to introduce the concepts and provide educational content to potential users of the solutions stored here.

    Deployment: 

    - source code is stored on the main branch of this repository. 
    - Use [gh-pages](https://www.npmjs.com/package/gh-pages) to deploy website to github pages.
    - Run `npm run deploy` to deploy to the "gh-pages" branch for deployment
    - Configure github pages in the repository to deploy from the "gh-pages" branch.

2. Solutions folder:

    This folder contains the code and results of the ROADII-Lab exploration of the traffic incident detection use case. It will be populated with functions, model code, training parameters, and eductional materials to help potential users or stakeholders with the development process for the deployment of their own traffic incident detection system.
