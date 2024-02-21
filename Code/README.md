## Quick-Start
For now, this repository replicates Volpe's 2019 work with Tennessee Highway Patrol using R 4.2.3. 

The steps are as follows:
1.	Download zipped data files and R scripts, preserving the directory structure. Place files into home directory. For Windows users, this is C:\Users\[user]\Documents. If needed, update file paths to locations where data and scripts are saved, although placing files into the home directory should work without any updates.
2.	Run RandomForest_WazeGrids_TN.R
a.	Install packages with install.packages(“[package name]”) as needed
3.	Run PredictWeek_TN.R


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

### OpenStreetMap (OSM) Data
The updated use case will use OSM road segments, rather than grid cells. OSM is nationally available and includes all roads in the U.S. This use case will focus on [OSM's road classification](https://wiki.openstreetmap.org/wiki/United_States/Road_classification) on motorways (generally aligned to the National Highway System), but will eventually include all highway and arterial roads. 
