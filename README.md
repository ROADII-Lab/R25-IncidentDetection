# TrafficIncidentDetectionGeneralization
ROADII Use Case 25 - Traffic Incident Detection through ML methods\

For more information on TRIMS and ROADII see the TRIMS organization home page [here](https://github.com/ITSJPO-TRIMS)

This repository contains code and documentation for explorations in the generalization and deployment of
techniques for detecting traffic incidents and assessing roadway network vulnerabilities using 
real-time and historical datasets. Intended audiences are state DOTs and traffic management centers 
wanting to improve their ability to respond quickly to incidents on their roadways, as well as to better
understand how traffic incidents affect the larger roadway network.

## Contents
1. Code

    This folder contains the code and results of the ROADII-Lab exploration of the traffic incident detection use case. It will be populated with functions, model code, training parameters, and eductional materials to help potential users or stakeholders with the development process for the deployment of their own traffic incident detection system. 


2. Demos folder:

    This folder contains pre-built demonstrations of the results of the analysis, data explorations, visualizations, and perforamance metrics for the solutions developed for this use case.

## Current Status
This use case is currently in the exploratory phase. The ROADII use case evaluation process defined [here](https://itsjpo-trims.github.io/#/about) has been conducted using information from various sources. This evaluation process resulted in a finding that this would be an appropriate use case for the ROADII-Lab Development Team to take on. USDOT is a logical place for coordination and development of guidance for developing data and analysis resources for transportation incident management relevant to all state and local DOTs and potentially incorporating several input data streams across public and private sources.

The ROADII organization is working to define the specific problem statements with associated stakeholder groups who have explored similar problems in the past. This involves targetting stakeholder outreach efforts to state DOTs who have explored solutions to these problems in the past. 

The ROADII-Lab team will focus on generalization of the solutions developed previously so they may be used by other stakeholders, as well as solving issues with deployment so these solutions may be more easily transformed into production level systems.

This repository and associated "README" files will be updated as the ROADII development team produces results for the traffic incident use case. Stay tuned!

## Datasets
The internal ROADII team has explored a number of datasets that may suport the development of AI models to improve and generalize traffic incident detection systems. Some of these datasets are real-time and crowd sourced while others are historical and based on measurements of traffic characteristics over time.

### Waze Data
The Waze roadway incident and jams data are provided by the Waze for Cities Program (previously known as the Connected Citizens Program). These data are provided free by Waze to public agencies around the world, partly in exchange for participation in provisioning roadway closure data, and partly as a public service by Waze. Waze provides real-time, anonymous, proprietary incident and slow-down information directly from drivers, aggregate these data, and provides them to public agencies. Public agency partners provide real-time and advance information on construction, crash, and road closure data. 

The data are provided nationally across the US to the USDOT through a Memorandum of Understanding with the USDOT Chief Data Officer. The data have been stored in the USDOT Secure Data Commons (SDC) since spring 2017.
The stream of data from the Waze API is in JSON format, with an API call every 2 minutes. The structure of these data and fields are described in the Waze Traffic Data Specification Document, Version 2.7.1 (Waze_Traffic_Data_Spec.pdf). 

In SDC, these data are housed in a Redshift relational database. There are three main tables that are used in data analysis: alerts, jams, and jam point sequences. 

### HSIS Data
The Highway Safety Information System (HSIS) was developed by the Federal Highway Administration (FHWA) to address the need for a comprehensive, data-driven approach to highway safety. Highway engineers and administrators need to make informed decisions concerning the design and operation of the highway system, considering factors such as the geometric design of the roadway, the selection and placement of roadside hardware, the use of traffic control measures, the size and performance capabilities of the vehicles, and the needs and abilities of the users. To facilitate these decisions, it's important to have data about crashes, roadway geometrics, traffic control devices, traffic volume, and the location of hardware and obstacles on the roadside. This data needs to be in computerized files and easily linked so that it can be rapidly assembled and prepared for analysis.

The HSIS is a roadway-based system that provides quality data on a large number of accident, roadway, and traffic variables. It uses data already being collected by the States for managing the highway system and studying highway safety. HSIS can be used to analyze a wide range of safety problems, from basic "problem identification" issues to modeling efforts that attempt to predict future crashes from roadway characteristics and traffic factors. It is used in support of the FHWA safety research program and provides input for program policy decisions. HSIS is also available to professionals conducting research under the National Cooperative Highway Research Program, universities, and others studying highway safety.

### NPMRDS Data
The NPMRDS contains field-observed travel time and speed data collected anonymously from a fleet of probe vehicles (cars and trucks) equipped with mobile devices. Using time and location information from probe vehicles, the NPMRDS generates speed and travel time data aggregated in 5-minute, 15-minute, or 1-hour increments. The data are available across the National Highway System (NHS), with a spatial resolution defined by Traffic Message Channel (TMC) location codes. A TMC represents a unique, directional roadway segment that is about half a mile to a mile long in urban and suburban areas and could be as long as five to ten miles long in rural areas. The NPMRDS covers more than 400,000 TMCs and includes several billions of speed and travel time observations across the NHS for both freeways and arterials. The NPMRDS has been available since 2013, with freeway data dating back as far as 2008.

