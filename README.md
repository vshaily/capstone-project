# capstone-project
Interactive R Shiny web application to visualize U.S. EPA Air-Quality Index data by year and quarter. <br />
Generates customizable trend visualizations. Shows single-pollutant trends across multiple states, multi-pollutant comparisons for a single state, and quarterly(seasonal) comparisons across multiple states. <br />
Data is sourced from EPA Air Quality System(AQS) API via RAQSAPI.<br />
Tutorial for use is displayed in the app.<br />

# Required
Packages: shiny, dplyr, lubridate, ggplot2, RAQSAPI, memoise, shinythemes<br />
EPA AQS credentials (email + API key); You can sign up at https://aqs.epa.gov/aqsweb/documents/data_api.html <br />
It may take a few days to obtain credentials. Be sure to read up on what actions could result in a temporary ban from the dataset. <br />
