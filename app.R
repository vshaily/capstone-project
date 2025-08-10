library(shiny)
library(bslib)
library(RAQSAPI)
library(dplyr)
library("keyring")
library(ggplot2)
library(shinythemes)
library(lubridate)
library(memoise)

# Capstone Project 2024
# Contains the code for an R shiny webapp that visualizes EPA pollutant data
# throughout the USA.

# Make sure to install all above packages before running this code
# Also move and set the working directory to a folder with nothing but this 
# R file.

# For purposes of accessing the EPA dataset, you MUST sign up for access. They 
# may take a few days to get back to you. After this, paste your email where
# specified, and make sure to use the passcode they emailed you.

# You can sign up at the following link:
# https://aqs.epa.gov/aqsweb/documents/data_api.html

# Keyring setup (make sure to follow tutorial on EPA site.)
keyring::key_set(service = "AQSDatamart",
                 username = "YOUR EMAIL HERE") # YOU HAVE TO USE YOUR EMAIL
datamartAPI_user <- "YOUR EMAIL HERE" # USE YOUR EMAIL
server <- "AQSDatamart"
aqs_credentials(username = datamartAPI_user,
                key = key_get(service = server,
                              username = datamartAPI_user
                )
)

# List of states and their FIPS code.

state_list <- c("Alabama" = "01", "Alaska" = "02", "Arizona" = "04", 
                "Arkansas" = "05", "California" = "06", "Colorado" = "08", 
                "Connecticut" = "09", "Delaware" = "10", 
                "District of Columbia" = "11", "Florida" = "12", 
                "Georgia" = "13", "Hawaii" = "15", "Idaho" = "16", 
                "Illinois" = "17", "Indiana" = "18", "Iowa" = "19", 
                "Kansas" = "20", "Kentucky" = "21", "Louisiana" = "22", 
                "Maine" = "23", "Maryland" = "24", "Massachusetts" = "25", 
                "Michigan" = "26", "Minnesota" = "27", "Mississippi" = "28", 
                "Missouri" = "29", "Montana" = "30", "Nebraska" = "31", 
                "Nevada" = "32", "New Hampshire" = "33", "New Jersey" = "34", 
                "New Mexico" = "35", "New York" = "36", "North Carolina" = "37",
                "North Dakota" = "38", "Ohio" = "39", "Oklahoma" = "40",
                "Oregon" = "41", "Pennsylvania" = "42", "Rhode Island" = "44",
                "South Carolina" = "45", "South Dakota" = "46", 
                "Tennessee" = "47", "Texas" = "48", "Utah" = "49", 
                "Vermont" = "50", "Virginia" = "51", "Washington" = "53", 
                "West Virginia" = "54", "Wisonsin" = "55", "Wyoming" = "56")

# AQS parameter codes
pollutant_list <- c("Ozone (O3)"= "44201", "PM2.5"= "88101", "PM10"= "81102",
                    "Carbon Monoxide (CO)"= "42101", 
                    "Sulfur Dioxide (SO2)"= "42401",
                    "Nitrogen Dioxide (NO2)"= "42602",
                    "Benzene"= "45201"
                    )

# Timekeepers
this_year <- year(Sys.Date())

# Here I created a trendline cap for the years looked at.
# This is to prevent overloading the system, and to lessen the chances of
# users getting banned by the epa
MAX_YEARS_TREND <- 12


# Caching setup (memoise)

# To lessen chances of the user getting banned, we preserve results via memoise
# so that identical requests do not need to call the AQS again.

# local folder for cached objects
cache_dir <- "cache"
if (!dir.exists(cache_dir)) dir.create(cache_dir)
aqs_cache <- memoise::cache_filesystem(cache_dir)

# Wrappers are around RAQSAPI calls. The function arguments are the
# the cache key, so changing any of them triggers a fresh API call.

# Done for both annual and quarterly. 

m_aqs_annual_by_state <- memoise::memoise(function(parameter, bdate, edate, 
                                                   stateFIPS) {
  aqs_annualsummary_by_state(parameter = parameter, bdate = bdate, 
                             edate = edate, stateFIPS = stateFIPS)},
  cache = aqs_cache)

m_aqs_quarterly_by_state <- memoise::memoise(function(parameter, bdate, edate, 
                                                      stateFIPS) {
  aqs_quarterlysummary_by_state(parameter = parameter, bdate = bdate, 
                                edate = edate, stateFIPS = stateFIPS)},
  cache = aqs_cache)


# Define UI for the app
ui <- fluidPage(
  titlePanel("Pollutant Trends Explorer"),
  sidebarLayout(
    sidebarPanel(
      # Time range (Annual vs Quarterly)
      selectInput("timeRange", "Time range:",
                  choices = c("Annual" = "annual", "Quarterly" = "quarterly"),
                  selected = "annual"),
      
      # Chart type (depends on timeRange)
      uiOutput("chartTypeUI"),
      
      # Annual Trendline range (Start/End Year)
      # (Shown only when Annual → Trendline)
      uiOutput("annualRangeUI"),
      
      # Number of pollutants (Annual Bar only: 2 or 3)
      uiOutput("numPollutantsUI"),
      
      # Pollutant selectors (N depends on chart type)
      uiOutput("pollutantSelectors"),
      
      # Quarterly mode (single vs compare states)
      uiOutput("comparisonModeUI"),
      
      # Number of states (Annual Trendline: 1–3; Quarterly multi: 2–3)
      uiOutput("numStatesUI"),
      
      # State selectors (dynamic count)
      uiOutput("stateSelectors"),
      
      # Single Year (Annual Bar, Quarterly Bar)
      uiOutput("singleYearUI"),
      
      # Submit to fetch & render; Generate Chart Button
      actionButton("submit", "Generate chart")
    ),
    mainPanel(
      # Information on the webapp
      # How To Use / Customized Figures / Additional Information.
      tabsetPanel(type = "tab",
                  # How To Use 
                  tabPanel("How To Use",
                           p(style = "text-align: justify; font-size = 16px",
                             "This tool allows users to observe data regarding pollutants in
                           the United States provided by the EPA's RAQSAPI R package"),
                           p(style = "text-align: justify; font-size = 16px",
                             "**IMPORTANT NOTE** Any plots you generate can be viewed on the
                           'Customize Plot' tab. Manipulating the sidebar elements on this
                           tab will not result in the tab being displayed here. Please head
                           to that panel after viewing the tutorial. Thank you!"),
                           p(style = "text-align: justify; font-size = 30px; color: green",
                             "TUTORIAL:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "First, begin customizing your plot by first selecting the type 
                           of time range you would like to look at. Annual will allow you to 
                           observe different annual trends,
                           while quarterly will allow you to observe different quarterly trends. 
                           If you would like to look at seasonal trends, for example, the latter 
                           example would suit your
                           goals better."),
                           p(style = "text-align: justify; font-size = 16px",
                             "As you continue to customize your graph, make sure to take note of
                             all of the options you are given in the sidebar. Follow 
                           through these options to 
                           continue observing the trends you would like to!"),
                           p(style = "text-align: justify; font-size = 30px; color: green",
                             "RULES, CONSIDERATIONS & COMMON ERRORS:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Remember to limit your requests to avoid getting banned from the dataset
                            by the EPA! Try to keep your graph customizations to 1-2 per minute. Exercise
                            further caution when looking at more variables."),
                           p(style = "text-align: justify; font-size = 16px",
                             "Allow graphs to fully load before beginning customization to prevent overloading
                           the system. A tip to make this easier is to do other things in the meantime to allow 
                           graphs to load. The more variables there are, the longer it can take."),
                           p(style = "text-align: justify; font-size = 16px",
                             "If you attempt to change a variable and receive the following error:"),
                           p(style = "text-align: justify; font-size = 16px; color: red",
                             "Error: [object OBJECT]"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Then the data that you have requested is not available. Causes of this 
                           are either due to the dataset not encompassing the data you have requested 
                           (for example: a state not having data for a set of years) or a potential ban
                           from the server for requesting too many sets within a short amount of time. 
                           A good way to know the difference is if data for other states is something that
                           you still have the ability to access. If you suspect the latter situation, please wait
                           another day before attempting to use this service again. If the program still 
                           does not work and you believe you have been banned, the EPA will contact you via email with 
                           details."),
                           p(style = "text-align: justify; font-size = 16px",
                             "Have fun exploring!")),
                  
                  # Customized Figures
                  tabPanel("Customized Figures", plotOutput("trendPlot", width = "150%")),
                  
                  # Additional Information
                  tabPanel("Additional Information", 
                           p(style = "text-align: justify; font-size = 50px; color: blue",
                             "It is important to stay informed on ...umerous risks associated with exposure to certain pollutants."),
                           p(style = "text-align: justify; font-size = 30px; color: green",
                             "Benzene:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "A highly flammable gas that has a sweet odor. It evaporates quickly and is heavier than air, 
                           so it is found closer to the ground and can go into lower elevation areas. {1}"),
                           p(style = "text-align: justify; font-size = 16px",
                             "What it does to you:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Benzene carries both acute and...ts. Acute benzene poisoning can come in the form of exposure to 
                           liquid or high levels of benzen...utomotive or manafacturing industries. {3} This can cause issues
                           ranging from vomiting, dizzines... and even death. Long term exposure can harm bone marrow leading
                           to anemia. It can harm the immu...increasing the likelihood and potency of subsequent infections. 
                           Additionally, the department of...n sciences has determined that benzene causes cancer in humans, 
                           specifically leukemia and other types of blood cancer.{4}"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Significant Concentrations:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "The significant concentration of benzene is roughly 1 ppm. {5}"),
                           p(style = "text-align: justify; font-size = 16px",
                             "What to do:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Leave area and go inside."),
                           p(style = "text-align: justify; font-size = 16px",
                             "Vulnerable Populations:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Children, elder adults, and pregnant women."),
                           p(style = "text-align: justify; font-size = 30px; color: green",
                             "Carbon Monoxide (CO):"),
                           p(style = "text-align: justify; font-size = 16px",
                             "A colorless, tasteless, odorless gas that forms when fuel is not burned completely. {6}"),
                           p(style = "text-align: justify; font-size = 16px",
                             "What it does to you:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Carbon Monoxide has a higher affinity for hemoglobin...gen. This reduces the amount of oxygen that
                           is transported throughout the body {7} Common symptoms include weakness, nausea, and shortness of
                           breath. However, exposure to CO can also escalate to ...ness and even death. {7}"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Significant Concentrations:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "At around concentrations of 150 ppm, ...sion of CO poisoning can be reached. {7}"),
                           p(style = "text-align: justify; font-size = 16px",
                             "What to do:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Leave the area and seek fresh air."),
                           p(style = "text-align: justify; font-size = 16px",
                             "Vulnerable Populations:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Children, elder adults, individuals with heart disease"),
                           p(style = "text-align: justify; font-size = 30px; color: green",
                             "Ozone (O3):"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Ozone forms in the troposphere ...s from cars and power plants.") ,
                           p(style = "text-align: justify; font-size = 16px",
                             "What it does to you:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Ozone is known to have a strong effect on both children, and adults with
                           respiratory comorbidities, showing an irritative effect on the pulmonary tissue.
                           It has also been shown to have ...mpact on the cardiovascular system, resulting in morbidity which
                           has been shown to disproportionately affect the elderly. {8}"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Significant Concentrations:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "The significant concentration of ozone is roughly 0.07 ppm {9}"),
                           p(style = "text-align: justify; font-size = 16px",
                             "What to do:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Avoid going outside.{1,10}"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Vulnerable Populations:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Individuals Over 65."),
                           p(style = "text-align: justify; font-size = 30px; color: green",
                             "Nitrogen Dioxide (NO2):"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Nitrogen dioxide forms primarily as a result of emissions fr...red as a subset of nitrogen oxides."),
                           p(style = "text-align: justify; font-size = 16px",
                             "What it does to you:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Nitrogen dioxide is known to irritate the airways in the ...ease to mortalities and hospitalizations."),
                           p(style = "text-align: justify; font-size = 16px",
                             "Significant Concentrations:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Significant concentration is 100 ppm for primary exposure.{11}"),
                           p(style = "text-align: justify; font-size = 16px",
                             "What to do:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Avoid exercise and stay indoors."),
                           p(style = "text-align: justify; font-size = 16px",
                             "Vulnerable Populations:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Individuals with asthma."),
                           p(style = "text-align: justify; font-size = 30px; color: green",
                             "Sulfur Dioxide (SO2):"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Sulfur dioxide is a colorless gas that can be detected by its strong gassy smell. {12}"),
                           p(style = "text-align: justify; font-size = 16px",
                             "What it does to you:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "SO2 exposure is linked to adverse respiratory outcomes such as ...depression, and chest tightness.") ,
                           p(style = "text-align: justify; font-size = 16px",
                             "Significant Concentrations:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Significant concentration for primary exposure is 75 ppb.{13}"),
                           p(style = "text-align: justify; font-size = 16px",
                             "What to do:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Avoid exercise and go indoors."),
                           p(style = "text-align: justify; font-size = 16px",
                             "Vulnerable Populations:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Children, elder adults, and individuals with heart and lung disease."),
                           p(style = "text-align: justify; font-size = 30px; color: green",
                             "Particulate Matter (PM 10):"),
                           p(style = "text-align: justify; font-size = 16px",
                             "A mixture of find inhalable particles with diameters under 10 ...ion occur through nose and throat."),
                           p(style = "text-align: justify; font-size = 16px",
                             "What it does to you:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Short term exposure to PM 10 c...ng in the form of bronchitis and asthma."),
                           p(style = "text-align: justify; font-size = 16px",
                             "Significant Concentrations:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "The annual standard level of PM 10 is 150 μg/m3 {18}"),
                           p(style = "text-align: justify; font-size = 16px",
                             "What to do:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Avoid exercise and go indoors."),
                           p(style = "text-align: justify; font-size = 16px",
                             "Vulnerable Populations:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Individuals with lung and cardiac conditions as well as children and elderly."),
                           p(style = "text-align: justify; font-size = 30px; color: green",
                             "Carbon Dioxide (CO2):"),
                           p(style = "text-align: justify; font-size = 16px",
                             "A colorless, odorless gas which is naturally present in the...erns surrounding its greenhouse effect."),
                           p(style = "text-align: justify; font-size = 16px",
                             "What it does to you:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Symptoms of CO2 poisoning can be seen towards the upper l... minutes to do so can be life-threatening."),
                           p(style = "text-align: justify; font-size = 16px",
                             "Significant Concentrations:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "The PEL limit for CO2 is 5000 ppm {19}"),
                           p(style = "text-align: justify; font-size = 16px",
                             "What to do:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Avoid exercise and go indoors."),
                           p(style = "text-align: justify; font-size = 16px",
                             "Vulnerable Populations:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Individuals with asthma, mainly children{20}"),
                           p(style = "text-align: justify; font-size = 30px; color: green",
                             "Particulate Matter (PM 2.5):"),
                           p(style = "text-align: justify; font-size = 16px",
                             "A mixture of particulate matter in both liquid and solid form that is around 2.5 micrometers.
                           Numerous sources, from construction sites to fires."),
                           p(style = "text-align: justify; font-size = 16px",
                             "What it does to you:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Long-term exposure has been shown to display a link with premature death"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Significant Concentrations:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Significant concentration is 9.0 μg/m3 for primary exposure.{10}"),
                           p(style = "text-align: justify; font-size = 16px",
                             "What to do:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Stay inside and keep your windows closed."),
                           p(style = "text-align: justify; font-size = 16px",
                             "Vulnerable Populations:"),
                           p(style = "text-align: justify; font-size = 16px",
                             "Children, elder adults, and individuals with heart and lung conditions.{21, 22, 23}"),
                           p(style = "text-align: justify; font-size = 16px",
                             "References can all be found in submitted final paper.")  )
      )
    )
  ))
  
  # Server
  server <- function(input, output, session) {
    # Chart type depends on time range
    output$chartTypeUI <- renderUI({
      req(input$timeRange)
      selectInput(
        "chartType", "Chart type:",
        choices = if (input$timeRange == "annual")
          c("Trendline" = "annual_line", "Bar" = "annual_bar")
        else
          c("Bar (Quarterly)" = "quarterly_bar")
      )
    })
    
    # Annual Trendline:
    output$annualRangeUI <- renderUI({
      req(input$chartType == "annual_line")
      tagList(
        selectInput("startYear", "Start year:", choices = 2000:2021, selected = 2005),
        selectInput("endYear",   "End year:",   choices = 2000:2021, selected = 2015)
      )
    })
    
    # Annual Bar:
    output$numPollutantsUI <- renderUI({
      req(input$chartType)
      if (input$chartType == "annual_bar")
        selectInput("numPollutants", "Number of pollutants:", choices = 2:3, selected = 2)
    })
    
    # Number of pollutants
    # Makes sure the right number of pollutants is presented for each chart type
    correctNumPollutants <- reactive({
      if (identical(input$chartType, "annual_bar")) {
        req(input$numPollutants); as.integer(input$numPollutants)
      } else 1L
    })
    
    # Pollutant selectors (N depends on chart type)
    output$pollutantSelectors <- renderUI({
      n <- correctNumPollutants()
      lapply(seq_len(n), function(i) {
        selectInput(
          inputId = if (n == 1) "pollutant" else paste0("pollutant_", i),
          label = if (n == 1) "Pollutant" else paste("Pollutant", i),
          choices = pollutant_list
        )
      })
    })
    
    # Quarterly modes
    output$comparisonModeUI <- renderUI({
      req(input$chartType == "quarterly_bar")
      selectInput("comparisonMode", "Quarterly Chart Type:",
                  choices = c("Single state" = "single", "Compare states" = "multi"),
                  selected = "single")
    })
    
    # Number of states control
    output$numStatesUI <- renderUI({
      req(input$chartType)
      if (input$chartType == "annual_line") {
        selectInput("numStates", "Number of states:", choices = 1:3, selected = 1)
      } else if (input$chartType == "quarterly_bar" && isTruthy(input$comparisonMode) && input$comparisonMode == "multi") {
        selectInput("numStates", "Number of states:", choices = 2:3, selected = 2)
      }
    })
    
    # State selectors
    correctNumStates <- reactive({
      if (input$chartType == "annual_line") {
        req(input$numStates); as.integer(input$numStates)
      } else if (input$chartType == "quarterly_bar") {
        if (!isTruthy(input$comparisonMode) || input$comparisonMode == "single") 1L else {
          req(input$numStates); as.integer(input$numStates)
        }
      } else 1L
    })
    
    # Create N dropdowns for state pickers
    # aoids redundancy
    output$stateSelectors <- renderUI({
      n <- correctNumStates()
      lapply(seq_len(n), function(i) {
        selectInput(paste0("state_", i), paste("Select State", i), choices = state_list)
      })
    })
    
    # Single year for annual_bar and quarterly_bar
    output$singleYearUI <- renderUI({
      req(input$chartType %in% c("annual_bar", "quarterly_bar"))
      selectInput("singleYear", "Year:", choices = 2000:2021, selected = 2005)
    })
    
    # Selected pollutants/states
    
    selectedPollutants <- reactive({
      n <- correctNumPollutants()
      if (n == 1) c(req(input$pollutant)) else sapply(seq_len(n), function(i) input[[paste0("pollutant_", i)]])
    })
    # put into a vector; this way, data is processed same regardless of state number
    selectedStates <- reactive({
      n <- correctNumStates()
      sapply(seq_len(n), function(i) input[[paste0("state_", i)]])
    })
    
    # Label for pollutant codes so that the names are displayed
    pol_code_to_label <- function(codes) {
      idx <- match(codes, pollutant_list)
      lbl <- names(pollutant_list)[idx]
      ifelse(is.na(lbl), codes, lbl)
    }
    
    # Fetch and summarize on Submit
    # Use eventReactive tied to a Submit button so plots don't automatically generate
    dataSummary <- eventReactive(input$submit, {
      req(input$chartType)
      pol_codes <- selectedPollutants()
      states    <- selectedStates()
      
      # Progress bar so users can view how much data is loading
      withProgress(message = "Fetching data...", value = 0, {
        if (input$chartType == "annual_line") {
          # Annual Trendline: 1 pollutant, 1–3 states, Start/End years
          req(length(pol_codes) == 1, isTruthy(input$startYear), isTruthy(input$endYear))
          validate(need(as.integer(input$startYear) <= as.integer(input$endYear), "Start year must be ≤ End year"))
          
          # Enforce a reasonable span so the API calls stay quick and the users don't
          # get banned
          span_years <- as.integer(input$endYear) - as.integer(input$startYear) + 1
          validate(need(span_years <= MAX_YEARS_TREND,
                        paste0("Please select ", MAX_YEARS_TREND, " years or fewer for Annual Trendline.")))
          
          bdate <- as.Date(paste0(input$startYear, "0101"), "%Y%m%d")
          edate <- as.Date(paste0(input$endYear,   "1231"), "%Y%m%d")
          
          n_calls <- length(states)
          dfs <- lapply(seq_along(states), function(i) {
            st <- states[i]
            incProgress(1 / n_calls, detail = paste("Annual summary for", st))
            m_aqs_annual_by_state(parameter = pol_codes[1], bdate = bdate, edate = edate, stateFIPS = st)
          })
          # combine all the dfs
          raw_all <- bind_rows(dfs) 
          # find units
          u <- unique(na.omit(raw_all$units_of_measure))
          unit_lbl <- if (length(u)) u[1] else "units"
          # Compute averages by taking monitor averages
          df <- raw_all %>%
            group_by(year, state) %>%
            summarise(mean_conc = mean(arithmetic_mean, na.rm = TRUE), .groups = "drop") %>%
            mutate(pollutant = pol_code_to_label(pol_codes[1]))
          attr(df, "y_label") <- paste0("Mean Concentration (", unit_lbl, ")")
          df
          
        } else if (input$chartType == "annual_bar") {
          # Annual Bar: 2–3 pollutants, single state, single year
          req(length(states) == 1, isTruthy(input$singleYear))
          obs_year <- paste0(input$singleYear, "0515")
          
          n_calls <- length(pol_codes)
          dfs <- lapply(seq_along(pol_codes), function(i) {
            pc <- pol_codes[i]
            incProgress(1 / n_calls, detail = paste("Annual summary for", pol_code_to_label(pc)))
            raw <- m_aqs_annual_by_state(parameter = pc,
                                         bdate = as.Date(obs_year, "%Y%m%d"),
                                         edate = as.Date(obs_year, "%Y%m%d"),
                                         stateFIPS = states[1])
            # Normalization of units, like CO/O3 ppm to ppb
            if (pc %in% c("42101", "44201")) raw <- mutate(raw, arithmetic_mean = arithmetic_mean / 1000)
            mutate(raw, pollutant = pol_code_to_label(pc))
          })
          df <- bind_rows(dfs) %>%
            group_by(pollutant) %>%
            summarise(mean_conc = mean(arithmetic_mean, na.rm = TRUE), .groups = "drop") %>%
            mutate(state = states[1], year = input$singleYear)
          attr(df, "y_label") <- "Mean Concentration (ppb)"
          df
          
        } else { # quarterly_bar
          req(length(pol_codes) == 1, isTruthy(input$singleYear))
          obs_year <- paste0(input$singleYear, "0515")
          
          n_calls <- length(states)
          dfs <- lapply(seq_along(states), function(i) {
            st <- states[i]
            incProgress(1 / n_calls, detail = paste("Quarterly summary for", st))
            m_aqs_quarterly_by_state(parameter = pol_codes[1],
                                     bdate = as.Date(obs_year, "%Y%m%d"),
                                     edate = as.Date(obs_year, "%Y%m%d"),
                                     stateFIPS = st)
          })
          raw_all <- bind_rows(dfs)
          # Find the units
          u <- unique(na.omit(raw_all$units_of_measure))
          unit_lbl <- if (length(u)) u[1] else "units"
          df <- raw_all %>%
            group_by(quarter, state) %>%
            summarise(mean_conc = mean(arithmetic_mean, na.rm = TRUE), .groups = "drop") %>%
            mutate(pollutant = pol_code_to_label(pol_codes[1]), year = input$singleYear)
          attr(df, "y_label") <- paste0("Mean Concentration (", unit_lbl, ")")
          df
        }
      })
    }, ignoreInit = TRUE)
    
    # renderPlot that switches on chartType and uses shared labels/attributes
    output$trendPlot <- renderPlot({
      df <- dataSummary()
      req(nrow(df) > 0)
      y_lab <- attr(df, "y_label"); if (is.null(y_lab)) y_lab <- "Mean Concentration"
      
      if (input$chartType == "annual_line") {
        ggplot(df, aes(year, mean_conc, color = state, group = state)) +
          geom_line() + geom_point() +
          labs(title = paste0("Annual Trend (", min(df$year), "–", max(df$year), ") — ", unique(df$pollutant)),
               x = "Year", y = y_lab, color = "State") +
          theme_minimal()
        
      } else if (input$chartType == "annual_bar") {
        ggplot(df, aes(pollutant, mean_conc, fill = pollutant)) +
          geom_bar(stat = "identity", position = position_dodge()) +
          labs(title = paste0("Annual Comparison (", unique(df$year), ") — ", unique(df$state)),
               x = "Pollutant", y = y_lab, fill = "Pollutant") +
          theme_minimal()
        
      } else { # quarterly_bar
        multistate <- length(unique(df$state)) > 1
        ggplot(df, aes(quarter, mean_conc, fill = if (multistate) state else quarter)) +
          geom_bar(stat = "identity", position = if (multistate) position_dodge() else "stack") +
          labs(title = paste0("Quarterly Averages (", unique(df$year), ") — ", unique(df$pollutant)),
               x = "Quarter", y = y_lab,
               fill = if (multistate) "State" else "Quarter") +
          theme_minimal()
      }
    })
  }
  
  # Run the app
  shinyApp(ui, server)
  
