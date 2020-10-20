##### libraries -----
library(shiny)
library(tidyverse)
library(plotly)
library(rsconnect)
library(shinyBS)
library(scales)
library(reshape2)
library(basictabler)
library(shinydashboard)

##### data in  -----
elasticities <-
  read.csv("elasticities.csv", stringsAsFactors = FALSE)

mode_share <-
  read.csv("mode_share_by_city.csv", stringsAsFactors = FALSE)

tree_diagram <- "tree_diagram.PNG"

##### user interface ----

fluidPage(
  # App title
  titlePanel("Relative Mode Share Impacts under Different
             Shared Rides Scenarios: Calculator"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar to present various slider options and other user inputs
    sidebarPanel(width = 3,
    # Creating tabs
                 tabsetPanel(
                   tabPanel("Intro",
                            h4("Overview"),
                            h5(tags$i(
                              paste0("This tool provides one way to explore the
 findings of research tasks conducted as part
 of the FHWA report  Analysis of Travel Choices
 and Scenarios for Sharing Rides.  This study
 sought to better understand how travelers chose
 to take shared or private trips in several modes.
 The focus was on ridehailing apps and social
 carpooling apps. ",
 "The tool can be used to explore changes in mode
 choice that might be observed if the relative
 time and price of various transportation modes
 changed. The tool allows tests of three
 scenarios (a fourth is experimental only) that
 were developed from the research on price
 and time. The scenarios are intentionally not
 based on policy mechanisms since there might be
 many or few ways for the relative cost and travel
 time scenarios to come about in various local contexts.
 The scenario inputs do not represent all potential
 policies related to ridehailing apps, social carpooling,
 or other modal trade-offs and the tool does not consider
 all likely effects of cost and time changes due to the
 bounds of the research. Despite the lack of perfect
 information on interactions between policies, we hope that
 the user finds its useful to mix and match effects.
 For example, the user may apply scenarios for changes
 affecting personal cars and ridehailing simultaneously. See the
 limitations section for more design considerations.")
 )),
                            hr(),
                            h4("Using the Tool"),
                            h5(tags$i(
                              "The tabs included in this sidebar offer
                               several ways for the user to interact
                               with the tool ."
                            ),
                            tags$li("The Scenarios tab allows the user to
                                    apply different levels and combinations
                                    of the three scenarios."),
                            tags$li("The Segments tab allows the user to narrow
                                     the scenario analysis by geography,
                                     population
                                     segment, and time-of-day. "),
                            tags$li("The Assumptions tab presents the user with
                             8 assumptions built into the scenario calculations.
                              The user can override
                                     these defaults using the slider bars. "),
                            tags$li("The Customize Initial
 Mode Shares tab allows
 the user to input customized initial mode
 shares (by person trip) for analysis. By
 default, mode shares are derived from
 National
 Household Travel Survey (NHTS) 2016 data.
 Mode shares presented here refer to
 person trips. ")
 ),
                            hr(),
                            h4("Limitations of the Tool"),
                            h5(tags$i(
                               "This tool is not intended to test the impact of
                                specific policies, but rather to model the mode
                                 choice
 implications of certain policy outcomes, such as a
 relative price difference increase between single-
 occupancy and carpool trips. Assumptions and data
 sources are explained in pop-up question mark text.
 Default values should be updated within the tool if
 better or more current information becomes available.
 Except for the third scenario, interactions
 across all modes are not considered. The first and
 second scenarios reallocate trips between TNCs
 (private and shared) and the third scenario
 reallocates only between private vehicles
 (drive alone or carpool). These reallocation
 patterns should still provide interesting
 insights, and may not be too far from accurate,
 1) travelers make decisions between personal cars,
 for-hire vehicles, transit, and active transportation
 before making decisions about shared vs. private
 personal cars and for-hire vehicles and 2) the
 relative prices of the first-order choices remain
 the same regardless of changes within the categories.
 In the third scenario, Drive Own Car trips are
 reallocated to other modes according to initial
 mode share.
 For this scenario (and the others), the research
 does not
 consider induced demand or that some people
 may choose not to
 travel if mode characteristics change."
                            ))
                   ),
                   tabPanel("Scenarios",
                            h6("Use this tab to test and to adjust the level
                                 of each scenario. The default value of
                                  each scenario
                                is zero."),
                            div(class = "row",
                                div(class = "span2",
                                    sliderInput("size_tnc_dollar",
                                                               h5(HTML(paste0(
 "Scenario 1: Increase cost
 savings for shared TNC trips relative
 to private TNC trips ($/mi)",
 "<i>",
 " - only affects TNC mode shares",
 "</i>"))),
 min = 0, max = 2.8,
 value = 0, step = 0.05,
 width = "250px")),
 div(class = "span2",
     tags$img(id = "question_tnc_dollar",
 src = "question.png",
 height = 15, width = 15))),
 div(class = "row",
     div(class = "span2", sliderInput("size_tnc_minute",
                                    h5(HTML(paste0(
                                    "Scenario 2: Reduce
                                    travel time penalty
                                    for shared TNC trips
                                    relative to private
                                    TNC trips  (mins/mi)",
                                    "<i>",
                                    " - only
                                    affects TNC mode
                                    shares", "</i>"))),
                                    min = 0, max = 2.5,
                                    value = 0, step = 0.05,
                                    width = "250px")),
                                div(class = "span2",
                                    tags$img(id = "question_tnc_minute",
                                             src = "question.png",
                                             height = 15, width = 15))),
                            div(class = "row",
                                div(class = "span2",
                                    sliderInput("size_nonshare_dollar",
                                                h5(HTML(paste0(
"Scenario 3: Increase price of
 private car trips relative to
 all other modes ($/trip)",
 "<i>",
 " - affects
 all modes", "</i>"))),
 min = 0,
 max = 5,
 value = 0,
 step = .25, width = "250px")),
                                div(class = "span2",
                                    tags$img(id = "question_nonshare_dollar",
                                             src = "question.png",
                                             height = 15, width = 15)))
                   ),
                   tabPanel("Experimental",
                            h6("Use this tab to test and to
                             adjust the level of Scenario 4
                                (experimental only)."),
                            div(class = "row",
                                div(class = "span2",
                                    sliderInput("size_carpool_reward",
                                                h5(HTML(
                                                paste0("CURRENTLY
                                                DISABLED:
                                                Scenario 4:
                                                Reward shared
                                                personal car
                                                trips but not
                                                private car
                                                trips
                                                ($/trip)",
                                                "<i>",
                                                " - only
                                                affects
                                                private car
                                                trips",
                                                "</i>"))),
                                                min = 0, max = 5,
                                                value = 0, step = 0.25,
                                                width = "250px")),
                                div(class = "span2",
                                    tags$img(id = "question_carpool_reward",
                                             src = "question.png",
                                             height = 15, width = 15))),
                            hr()
                   ),
                   tabPanel("Segments",
                            h6("Use this tab to select
 specific population segments and geographies for scenario testing"),
                            div(class = "row",
                                div(class = "span2",
                                    selectInput("geography", "View Geography:",
   choices = unique(mode_share$Market),
                                                selected  =  "Study Cities"),
                                    style = "min-width: 250px;"),
                                div(class = "span2",
                                    tags$img(id = "question_geography",
                                             src = "question.png",
                                             height = 15, width = 15))),
                            div(class = "row",
                                div(class = "span2",
                                    selectInput("segment",
                                                "Population Segment",
                                                choices =
                                                  unique(elasticities$Segment),
                                                width = "250px")),
                                div(class = "span2",
                                    tags$img(id = "question_segment",
                                             src = "question.png",
                                             height = 15, width = 15))),
                            radioButtons(inputId = "peak",
                                         label = "Select Time of Day",
                                         choiceNames = c("All Day",
                                                         "AM/PM Peak"),
                                         choiceValues = c(0, 1), inline = TRUE),
                            hr()
                   ),
                   tabPanel("Assumptions",
                            h6("Use this tab to adjust assumptions that
 affect the impact of changing scenarios on VMT."),
                            div(class = "row",
 div(class = "span2", sliderInput("tnc_match_rate",
 "Shared TNC Trip Overlap Rate (%):",
 35, min = 0, max = 100, width = "250px")),
                                div(class = "span2",
                                    tags$img(id = "question_tnc_match_rate",
                                             src = "question.png",
                                             height = 15, width = 15))),
                            div(class = "row",
                                div(class = "span2", sliderInput("tnc_circuity",
                                "TNC Non-passenger Miles (%):",
                                42, min = 0, max = 100,
                                step = 1, width = "250px")),
                                div(class = "span2",
                                    tags$img(id = "question_tnc_circuity",
                                             src = "question.png",
                                             height = 15, width = 15))),
                            div(class = "row",
                                div(class = "span2",
                                    sliderInput("pool_circuity",
                                                "Pool Circuity (%):",
                                                10, min = 0,
                                                max = 100,
                                                width = "250px")),
                                div(class = "span2",
                                    tags$img(id = "question_pool_circuity",
                                             src = "question.png",
                                             height = 15, width = 15))),
                            div(class = "row",
                                div(class = "span2",
                                    sliderInput("carpool_circuity",
                                                "Carpool Detour Factor (%):",
                                                10, min = 0, max = 100,
                                                width = "250px")),
                                div(class = "span2",
                                    tags$img(id = "question_carpool_circuity",
                                             src = "question.png",
                                             height = 15, width = 15))),
                            div(class = "row",
                                div(class = "span2",
                                    sliderInput("policy_3_reach",
"Trips Affected by Car Pricing Policies (%):",
                                                70, min = 0, max = 100,
                                                width = "250px")),
                                div(class = "span2",
                                    tags$img(id = "question_policy_3_reach",
                                             src = "question.png",
                                             height = 15, width = 15))),
                            div(class = "row",
 div(class = "span2", sliderInput("app_penetration",
 "Penetration of Social Carpooling Apps (%):",
 25, min = 0, max = 100, width = "250px")),
 div(class = "span2",
     tags$img(id = "question_penetration",
              src = "question.png", height = 15, width = 15))),
                            hr(),
                            div(class = "row",
 div(class = "span2", sliderInput("base_occupancy",
 "Vehicle Occupancy for Carpool Trips Before Applying
 Scenarios (Persons/Vehicle):",
 value = 2.05, step = 0.05, min = 2, max = 3, width = "250px")),
 div(class = "span2", tags$img(id = "question_base_occupancy",
 src = "question.png", height = 15, width = 15))),
 div(class = "row",
 div(class = "span2", sliderInput("scenario_occupancy",
 "Vehicle Occupancy for Carpool Trips After Applying
 Scenarios (Persons/Vehicle):",
 value = 2.05, step = 0.05, min = 2, max = 3, width = "250px")),
 div(class = "span2",
     tags$img(id = "question_scenario_occupancy",
              src = "question.png", height = 15, width = 15)))
 ),
                   tabPanel("Customize Initial Mode Shares",
                            h6(paste0(
 "Use this tab to override the initial mode share
  distribution (person trips) with custom values. ",
 "By default, the initial mode shares are calculated
 from National Household Travel Survey (NHTS) 2016 data")),
                            br(),
                            radioButtons("override_mode_share",
 "Customize Initial Mode Shares (Person Trips)?",
 choiceNames = c("Yes", "No"), selected = 0,
 choiceValues = c(1, 0), inline = TRUE),
                            tags$table(
                              tags$tr(width = "100%",
                                      tags$td(width = "60%",
                                              div("Car (drive)")),
                                      tags$td(width = "40%",
 numericInput(inputId = "override_car",
                                                           label = NULL,
                                                           value = 14.3,
                                                           min = 0,
                                                           max = 100,
                                                           width = "75px"))
                              ),
                              tags$tr(width = "100%",
                                      tags$td(width = "60%",
                                              div("Car (passenger)")),
                                      tags$td(width = "40%",
                                              numericInput(
                                                inputId = "override_carpool",
                                                label = NULL, value = 14.3,
                                                min = 0, max = 100,
                                                width = "75px"))
                              ),
                              tags$tr(width = "100%",
                                      tags$td(width = "60%",
                                              div("TNC (private)")),
                                      tags$td(width = "40%",
                                              numericInput(inputId =
 "override_tnc_private",
 label = NULL, value = 14.3,
                                                           min = 0, max = 100,
                                                           width = "75px"))
                              ),
                              tags$tr(width = "100%",
                                      tags$td(width = "60%",
                                              div("TNC (shared)")),
                                      tags$td(width = "40%",
                                              numericInput(inputId =
 "override_tnc_shared",
                                                           label = NULL,
                                                           value = 14.3,
                                                           min = 0,
                                                           max = 100,
                                                           width = "75px"))
                              ),
                              tags$tr(width = "100%",
                                      tags$td(width = "60%",
                                              div("Transit")),
                                      tags$td(width = "40%",
                                              numericInput(inputId =
                                                             "override_transit",
 label = NULL, value = 14.3,
 min = 0, max = 100, width = "75px"))
                              ),
                              tags$tr(width = "100%",
                                      tags$td(width = "60%",
                                              div("Walk")),
                                      tags$td(width = "40%",
                                              numericInput(
                                                inputId = "override_walk",
                                                label = NULL, value = 14.3,
                                                min = 0, max = 100,
                                                width = "75px"))
                              ),
                              tags$tr(width = "100%",
                                      tags$td(width = "60%", div("Bike")),
                                      tags$td(width = "40%",
                                              numericInput(
                                                inputId = "override_bike",
                                                label = NULL, value = 14.2,
                                                min = 0, max = 100,
                                                width = "75px"))
                              ),
                              tags$tr(width = "100%",
                                      tags$td(div(
                                        style = "margin: 0px 0px -10px 0px",
                                        strong("Sum"))),
                                      tags$td(strong(
                                        valueBoxOutput("sum_of_inputs")))
                              )
                            ),
                            #Setting bsTooltip text
                            bsTooltip("question_tnc_dollar",
                                      paste0("Under this scenario,
 there is a larger discount for sharing ",
 "which applies only
 to private TNC rides on a
 per-mile basis. ",
 "No cross-mode shifts are captured
 in the data, which might represent
 the ",
 "average TNC (private and shared together) cost remaining roughly the same. ",
 "This is based on analysis of TNC survey data."),
 placement = "right", trigger = "hover"),
                            bsTooltip("question_tnc_minute",
                                      paste0("Under this scenario, total travel
 time for shared TNC trips is reduced ",
                                             "possibly through shorter wait
                                              times for ",
                                             "shared TNC trips, which have
 longer expected in-vehicle times. ",
 "The scenario applies only to TNC trips,
 on a per-trip basis, with no cross-mode
 effects. ",
 "This is based on analysis of TNC survey data."),
                                      placement = "right", trigger = "hover"),
                            bsTooltip("question_nonshare_dollar",
paste0("Under this scenario, either non-shared
                                             trip costs ",
"become greater or shared trip costs decrease
                                       on a per trip ",
                                      "basis for cars. This is based on a arc
                                       elasticity of demand of ",
                                      "-0.30."),
                                      placement = "right", trigger = "hover"),
                            bsTooltip("question_carpool_reward",
                                      paste0("Under this experimental scenario
                                              (currently disabled), drivers and
 passengers would receive rewards for
 carpooling ",
 "through a hypothetical mechanism like
                                             a social carpooling app. ",
                                             "This scenario applies only to car
                                              trips and assumes carpool rewards
 would not attract people from other
                                              modes. "
                                      ),
                                      placement = "right", trigger = "hover"),
                            bsTooltip("question_pool_circuity",
                                      "This value represents the additional
                                       distance that a shared TNC trip travels
 to accomodate matched parties when compared
 with a private TNC trips more direct route.",
                                      placement = "right", trigger = "hover"),
                            bsTooltip("question_carpool_circuity",
                                      "This value represents the additional
                                       distance a carpool driver would travel
                                       to pick up their passenger relative to
 driving alone directly to their destination.",
                                      placement = "right", trigger = "hover"),
                            bsTooltip("question_tnc_circuity",
 "This value represents the share of TNC VMT
                                       without a passenger (i.e., cruising and
                                       deadheading). The starting value is from
                                       Balding et al (2018), Table 3.",
                                      placement = "right", trigger = "hover"),
                            bsTooltip("question_penetration",
 "This value represents the share of travelers
 that would opt in to using a social
  carpooling app which supports the use of reward incentives.",
 placement = "right", trigger = "hover"),
                            bsTooltip("question_tnc_match_rate",
                                      paste0("This value captures the percent of
 trips in which a user willing to share
 does share (defaulted to 50%) and ",
 "the percent of their trip where two
 parties are in vehicle
 (defaulted to 70%)."),
                                      placement = "right", trigger = "hover"),
                            bsTooltip("question_policy_3_reach",
                                      paste0("This value controls the impact of
 the third scenario that affects private
                                              car trips. ",
                                             "This can be used to tailor your
                                              scenario definition to be specific
 to the type of policy instrument applied. ",
 "For example, 100% reach affects all
 vehicle trips in the metro area. ",
 "A lower percentage would be
 appropriate for a smaller cordon,
 or parking surcharge-based policy instrument."),
                                      placement = "right", trigger = "hover"),
                            bsTooltip("question_base_occupancy",
 paste0("This value represents the occupancy of
 carpool trips (excluding single-occupancy vehicles) ",
 "prior to the application of the third and
 fourth scenarios, which would be
 expected to impact the occupancy of carpool trips."),
                                      placement = "right", trigger = "hover"),
                            bsTooltip("question_scenario_occupancy",
                                      paste0("This value represents the new
                                              occupancy after applications of
                                              the third and fourth scenarios. ",
"If, for example, there is a conversion
 of 2-person carpools to 3-person
 carpools, then this value should be
 expected to increase."),
 placement = "right", trigger = "hover"),
                            bsTooltip("question_segment",
"Choice of segment will affect the elasticities applied for
 TNC scenarios and will reduce total VMT considered relative to selecting All.",
                                      placement = "right", trigger = "hover"),
                            bsTooltip("question_geography",
                                      "Select metropolitan area (CBSA)
 to consider, or select all study cities.",
                                      placement = "right", trigger = "hover"),
                            #Setting CSS styles
                            tags$style(type = "text/css",
                                       "#question_geography {width: 15px;
                                       margin-top: 0px;}"),
                            tags$style(type = "text/css",
                                       "#question_segment {width: 15px;
                                       margin-top: 0px;}"),
                            tags$style(type = "text/css",
                                       "#question_tnc_dollar {width: 15px;
                                       margin-top: 80px;}"),
                            tags$style(type = "text/css",
                                       "#question_tnc_minute {width: 15px;
                                       margin-top: 80px;}"),
                            tags$style(type = "text/css",
                                       "#question_nonshare_dollar
                                       {width: 15px; margin-top: 80px;}"),
                            tags$style(type = "text/css",
                                       "#question_carpool_reward
                                       {width: 15px; margin-top: 80px;}"),
                            tags$style(type = "text/css",
                                       "#question_pool_circuity
                                       {width: 15px;margin-top: 80px;}"),
                            tags$style(type = "text/css",
                                       "#question_carpool_circuity
                                       {width: 15px;margin-top: 80px;}"),
                            tags$style(type = "text/css",
                                       "#question_tnc_circuity
                                       {width: 15px;margin-top: 80px;}"),
                            tags$style(type = "text/css",
                                       "#question_tnc_match_rate
                                       {width: 15px;margin-top: 80px;}"),
                            tags$style(type = "text/css",
                                       "#question_penetration
                                        {width: 15px;margin-top: 80px;}"),
                            # this changes the size of the tooltips
                            tags$head(
                              tags$style(".tooltip{width:500px}"),
                              tags$style(type = "text/css", ".span2
                                         { display: inline-block; }")
                            ),
                            # this changes the size of the tabs
                            tags$head(
                              tags$style(type = "text/css", ".nav-tabs
                                         {font-size: 12px;} ")
                            )))),
    # Main panel for displaying outputs ----
    mainPanel(width = 9,
          fluidRow(
                 h5(tags$i("Decision trees by scenario:")),
                 img(src = tree_diagram, width = 750, height = 200),
                  # Output: Text indicating what segments the user has selected
                  htmlOutput("segments_chosen"),
                  hr(),
                  # Output: Table summarizing the values entered
                  tableOutput("values"),
                  # Output: Bar Graph
                  plotlyOutput("stacked_bar"),
                  p(tags$i("Hover over the above chart to access the toolbar
                            which enables the ability to zoom in or out."))
            )
      )
    )
  )