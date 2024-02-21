# Traffic Incident Detection
ROADII Use Case 25 - Traffic Incident Detection through ML methods

For more information on TRIMS and ROADII see the [TRIMS organization home page](https://github.com/ITSJPO-TRIMS)

This repository contains code and documentation for explorations in the generalization and deployment of
techniques for detecting traffic incidents and assessing roadway network vulnerabilities using 
real-time and historical datasets. Intended audiences are state DOTs and traffic management centers 
wanting to improve their ability to respond quickly to incidents on their roadways, as well as to better
understand how traffic incidents affect the larger roadway network.

## Contents
1. [Code](https://github.com/ITSJPO-TRIMS/R25-IncidentDetection/tree/main/Code)

    This folder contains the code and installation instructions of the ROADII-Lab exploration of the traffic incident detection use case. It will be populated with functions, model code, training parameters, and eductional materials to help potential users or stakeholders with the development process for the deployment of their own traffic incident detection system. 


2. [Demos](https://github.com/ITSJPO-TRIMS/R25-IncidentDetection/tree/main/Demos):

    This folder contains pre-built demonstrations of the results of the analysis, data explorations, visualizations, and perforamance metrics for the solutions developed for this use case.

## Current Status
This use case is currently in the exploratory phase. The ROADII team has been updating 2019 work with the Tennessee Highway Patrol to forecast crash probability over the next week. The ROADII team has also been conducting stakeholder outreach with FHWA Office of Operations and state DOTs Traffic Managment Centers to evaluate interest and feasibility of the use case. The ROADII team will be updating this repository as stable developments are created. 

This repository and associated "README" files will be updated as the ROADII development team produces results for the traffic incident use case. 

## Context on Previous Related Work
This use case builds on 2019 work that the Volpe Center did with Tennessee Highway Patrol under the U.S. DOT [Safety Data Initiative](https://www.transportation.gov/SafetyDataInitiative/Pilots).  The [Tennessee Integrated Traffic Analysis Network](https://www.transportation.gov/office-policy/transportation-policy/sdi-waze-project-summary-documents) (TITAN) crash model was built to be used by the Tennessee Highway Patrol (TN HP) to prioritize patrol locations. The model combined historical data such as fatal crashes and DUI arrests with current data including weather forecasts, and scheduled special events to generate heat maps that identifies areas of high likelihood for crashes. The TITAN model estimates crash propensity in 42 square mile grids. This resolution provides the most accurate estimates of fatal crash propensity in four hour time windows over the upcoming week, based on scheduled special events and weather forecasts. The objective of the TN HP and U.S. DOT SDI partnership was to test if Waze alerts can improve the spatial resolution of the TITAN model, particularly in urban areas. TN HP provided training data from the TITAN model, and the Volpe team quantitatively assessed the value that Waze data adds to the existing TITAN model.

![THP Dashboard](https://github.com/ITSJPO-TRIMS/R25-IncidentDetection/blob/main/Demos/TN/TN_dashboard.JPG)


## Credits
- Eric Englin (Volpe) Eric.Englin@dot.gov
- Dan Flynn (Volpe)
- Andrew Breck (Volpe)
- Joey Reed (Volpe)
- Robin Wilkinson (Volpe)
- Billy Chupp (Volpe)

## Project Sponsors
The development of ROADII that contributed to this public version was funded by the U.S. Intelligent Transportation Systems Joint Program Office (ITS JPO) under IAA HWE3A122. Any opinions, findings, conclusions or recommendations expressed in this material are those of the authors and do not necessarily reflect the views of the ITS JPO.
