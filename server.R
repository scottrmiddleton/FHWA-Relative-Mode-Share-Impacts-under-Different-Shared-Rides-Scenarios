##### data in  -----
elasticities <-
  read.csv("elasticities.csv",  stringsAsFactors = FALSE)

mode_share <-
  read.csv("mode_share_by_city.csv",  stringsAsFactors = FALSE)

vmt_share_by_segment <-
  read.csv("vmt_share_by_segment.csv",  stringsAsFactors = FALSE)

trip_cost_by_city <-
  read.csv("trip_cost_by_city.csv",
           stringsAsFactors = FALSE,
           fileEncoding = "UTF-8-BOM")

# server ----
function(input,  output) {
  #Creating values for display table ----
  create_table <- reactive({
    #Creating a value for the peak vs. all day toggle ----
    peak <- input$peak
    #pulling inputs from assumption sliders in UI ----
    tnc_circuity <- 1 + input$tnc_circuity / 100
    pool_circuity <- 1 + input$pool_circuity / 100
    carpool_circuity <- input$carpool_circuity / 100
    tnc_match_rate <- 1 - input$tnc_match_rate / 100
    app_penetration <- input$app_penetration / 100
    policy_3_reach <- input$policy_3_reach / 100
    base_occupancy_bonus <- input$base_occupancy - 2
    scenario_occupancy_bonus <- input$scenario_occupancy - 2
    # Pulling effects from input data on elasticities ----
    #Scenario 1
    tnc_dollar_effect <-
      input$size_tnc_dollar *
      elasticities[which(elasticities$Segment == input$segment &
                           elasticities$Peak == peak), ]$tnc_dollar
    #Scenario 2
    tnc_minute_effect <-
      input$size_tnc_minute *
      elasticities[
        which(elasticities$Segment ==
                input$segment & elasticities$Peak == peak), ]$tnc_minute
    #Scenario 3
    q1 <- mode_share[which(mode_share$Market == input$geography), ]$Share[1]
    p1 <-
      trip_cost_by_city[which(
        trip_cost_by_city$City == input$geography), ]$Trip_Cost_Drive_Alone
    fee <- input$size_nonshare_dollar
    p2 <- p1 + fee
    e <- elasticities[which(elasticities$Segment ==
                              input$segment &
                              elasticities$Peak == peak), ]$nonshare_dollar
    q2 <-
      (q1 + q1 * ((fee / mean(p1, p2)) * e)) /
      (1 + ((fee / mean(p1, p2)) * e) / 2)
    nonshare_dollar_effect <-
      policy_3_reach *
      (q1 - q2)
    #Scenario 4
    carpool_reward_effect <-
      0 * app_penetration *
      elasticities[which(elasticities$Segment == input$segment &
                           elasticities$Peak == peak), ]$carpool_reward
    #Defining vectors for dataframe----
    #Creating a vector for modes
    modes <- c("Drive Private Car", "Passenger in Private Car",
              "Use TNC (private)", "Use TNC (shared)",
              "Use Transit", "Walk", "Bike", "Total")
    #Defining total trips for the market
    total_trips <-
      as.numeric(
        c(
          mode_share[which(mode_share$Market == input$geography), ]$total_trips,
          1
        )
      )
    #Assigning initial shares --
    # if statement determines whether to use user input or defaults
    if (input$override_mode_share == 0) {
    initial_share <-
      c(
        mode_share[which(mode_share$Market == input$geography), ]$Share,
        1)
    } else {
    initial_share <-
      initial_share <-
      c(input$override_car / 100,
        input$override_carpool / 100,
        input$override_tnc_private / 100,
        input$override_tnc_shared / 100,
        input$override_transit / 100,
        input$override_walk / 100,
        input$override_bike / 100,
        (input$override_car +
           input$override_carpool +
           input$override_tnc_private +
           input$override_tnc_shared +
           input$override_transit +
           input$override_walk + input$override_bike) / 100)
    }
    #Defining initial trips based on total trips and initial share
    initial_trips <- total_trips * initial_share
    #Adjusting initial car trips to account for carpool occupancy > 2
    initial_trips[1] <- initial_trips[1] -
      initial_trips[2] * base_occupancy_bonus
    #Calculating VMT per trip to account for different trip lengths in each city
    vmt_per_trip <-
      as.numeric(
        c(
          mode_share[which(
            mode_share$Market == input$geography), ]$Trip_Length_mi,
          1)
      )
    #Calculating VMT by segment
    vmt_share_by_segment <-
      as.numeric(
        c(
          rep(vmt_share_by_segment[
            which(vmt_share_by_segment$Segment == input$segment &
                    vmt_share_by_segment$Peak == peak), ]$VMT_share,
              8)
        )
      )
    #Adjusting trip lengths for circuity assumptions
    vmt_per_trip[2] <- vmt_per_trip[2] * carpool_circuity
    vmt_per_trip[3] <- vmt_per_trip[3] * tnc_circuity
    vmt_per_trip[4] <- vmt_per_trip[4] * tnc_circuity *
      pool_circuity * tnc_match_rate
    #defining TNC and non-SOV share,
    # which is needed to calculate the elasticities vector
    # by redistributing trips to other modes
    tnc_share <- initial_share[3] + initial_share[4]
    non_sov_share <- 1 - initial_share[1]
    #Defining vector of seven elasticities
    if (- ((-tnc_dollar_effect - tnc_minute_effect) *
         tnc_share + nonshare_dollar_effect *
         initial_share[3] / non_sov_share) < initial_share[3]) {
      elasticities <- c(
        -nonshare_dollar_effect - carpool_reward_effect,  #Car (drive)
        (nonshare_dollar_effect * initial_share[2] / non_sov_share) +
          carpool_reward_effect,   #Car (passenger)
        (-tnc_dollar_effect - tnc_minute_effect) * tnc_share +
          nonshare_dollar_effect *
          initial_share[3] / non_sov_share,  #TNC (private)
        (tnc_dollar_effect + tnc_minute_effect) *
          tnc_share + nonshare_dollar_effect *
          initial_share[4] / non_sov_share,  #TNC (shared)
        nonshare_dollar_effect * initial_share[5] / non_sov_share,  #Transit
        nonshare_dollar_effect * initial_share[6] / non_sov_share,  #Walk
        nonshare_dollar_effect * initial_share[7] / non_sov_share,  #Bike
        0
        )}
    else{
      elasticities <- c(
        -nonshare_dollar_effect -
          carpool_reward_effect,  #Car (drive)
        (nonshare_dollar_effect * initial_share[2] /
           non_sov_share) + carpool_reward_effect,   #Car (passenger)
        -initial_share[3],  #TNC (private)
        initial_share[3],  #TNC (shared)
        nonshare_dollar_effect * initial_share[5] / non_sov_share,  #Transit
        nonshare_dollar_effect * initial_share[6] / non_sov_share,  #Walk
        nonshare_dollar_effect * initial_share[7] / non_sov_share,  #Bike
        0
      )
    }

    #Calculating new share of trips.
    # If no incentives are applied new_share is the same as initial_share.
    # If an incentive is applied,  then elasticities are added
    if (tnc_dollar_effect != 0 |
       tnc_minute_effect != 0 |
       carpool_reward_effect != 0 |
       nonshare_dollar_effect != 0
    ) {
      new_share <- initial_share + elasticities
      }
    else {
      new_share <- initial_share
      }
    #Calculating initial VMT from trips,
    # vmt per trip,  and shared by segment. Creating sum.
    initial_vmt <- initial_trips * vmt_per_trip * vmt_share_by_segment
      initial_vmt[8] <- sum(initial_vmt)
    #Adjusting initial VMT to combine car passenger and car driver trips
    initial_vmt[1] <- initial_vmt[1] + initial_vmt[2]
    initial_vmt[2] <- 0
    #Calculating new trips from total trips and new share
    new_trips <- total_trips * new_share
    #Adjusting new car trips to account for carpool occupancy > 2
    new_trips[1] <- new_trips[1] - new_trips[2] * scenario_occupancy_bonus
    #Calculating new vmt from new trips,
    # VMT per trip,  and segment adjustment. Creating sum.
    new_vmt <- new_trips * vmt_per_trip * vmt_share_by_segment
    new_vmt[8] <- sum(new_vmt)
    #Adjusting new VMT to combine car passenger and car driver trips
    new_vmt[1] <- new_vmt[1] + new_vmt[2]
    new_vmt[2] <- 0
    #Calculating change in VMT. Without incentives
    # (i.e.,  elasticity value) the change is zero.
    change_in_vmt <-
      (new_vmt - initial_vmt) / initial_vmt
      change_in_vmt[5:7] <- 0.00
    #Adjusting change in VMT to set carpool to zero
    change_in_vmt[2] <- 0.00
    #Creating dataframe ----
    data <- data.frame(
      Mode = modes,
      "Initial Share" =
        percent(initial_share, digits = 0.01),
      "New Share" =
        percent(new_share, digits = 0.01),
      "Initial VMT" =
        c(format(round(initial_vmt[1],  -4), nsmall = 0,  big.mark = ", "),
          "N/A",
          format(round(initial_vmt[3],  -4), nsmall = 0,  big.mark = ", "),
          format(round(initial_vmt[4],  -4), nsmall = 0,  big.mark = ", "),
          "N/A",
          "N/A",
          "N/A",
          format(round(initial_vmt[8],  -4), nsmall = 0,  big.mark = ", ")),
      "New VMT" =
        c(format(round(new_vmt[1],  -4), nsmall = 0,  big.mark = ", "),
          "N/A",
          format(round(new_vmt[3],  -4), nsmall = 0,  big.mark = ", "),
          format(round(new_vmt[4],  -4), nsmall = 0,  big.mark = ", "),
          "N/A",
          "N/A",
          "N/A",
          format(round(new_vmt[8],  -4), nsmall = 0,  big.mark = ", ")),
      "Change in VMT" =
        percent(change_in_vmt,  accuracy = .1),
        check.names = FALSE
    )
  })
  # Creating output for stacked bar pie ----
  create_stacked_bar <- reactive({
    mode <- c("Car (drive)", "Car (passenger)",
              "TNC (private)", "TNC (shared)",
              "Transit", "Walk", "Bike")
    initial_share <- mode_share[which(
      mode_share$Market == input$geography), ]$Share
    # Code below is to update new share vector ----
    initial_share <-
      c(mode_share[which(mode_share$Market == input$geography), ]$Share)
    #Creating a value for the peak vs. all day toggle
    peak <- input$peak
    #pulling inputs from assumption sliders in UI
    tnc_circuity <- 1 + input$tnc_circuity / 100
    pool_circuity <- 1 + input$pool_circuity / 100
    carpool_circuity <- input$carpool_circuity / 100
    tnc_match_rate <- 1 - input$tnc_match_rate / 100
    app_penetration <- input$app_penetration / 100
    policy_3_reach <- input$policy_3_reach / 100
    #pulling effects from input data on elasticities
    #Scenario 1
    tnc_dollar_effect <-
      input$size_tnc_dollar *
      elasticities[which(elasticities$Segment == input$segment &
                           elasticities$Peak == peak), ]$tnc_dollar
    #Scenario 2
    tnc_minute_effect <-
      input$size_tnc_minute *
      elasticities[which(
        elasticities$Segment == input$segment &
          elasticities$Peak == peak), ]$tnc_minute
    #Scenario 3
    q1 <- mode_share[which(mode_share$Market == input$geography), ]$Share[1]
    p1 <-
      trip_cost_by_city[
        which(trip_cost_by_city$City == input$geography),
        ]$Trip_Cost_Drive_Alone
    fee <- input$size_nonshare_dollar
    p2 <- p1 + fee
    e <- elasticities[
      which(elasticities$Segment == input$segment &
              elasticities$Peak == peak), ]$nonshare_dollar
    q2 <-
      (q1 + q1 * ((fee / mean(p1, p2)) * e)) /
      (1 + ((fee / mean(p1, p2)) * e) / 2)
    nonshare_dollar_effect <-
      policy_3_reach *
      (q1 - q2)
    #Scenario 4
    carpool_reward_effect <-
      0 * app_penetration *
      elasticities[
        which(elasticities$Segment == input$segment &
                elasticities$Peak == peak), ]$carpool_reward
    #Defining vectors for dataframe
    modes <- c("Car (drive)", "Car (passenger)",
              "TNC (private)", "TNC (shared)",
              "Transit", "Walk", "Bike")
    #Assigning initial shares --
    # if statement determines whether to use user input or defaults
    if (input$override_mode_share == 0) {
      initial_share <-
        c(
          mode_share[which(
            mode_share$Market == input$geography), ]$Share
          )
    } else {
      initial_share <-
        initial_share <-
        c(input$override_car / 100,
          input$override_carpool / 100,
          input$override_tnc_private / 100,
          input$override_tnc_shared / 100,
          input$override_transit / 100,
          input$override_walk / 100,
          input$override_bike / 100)
    }
    #defining TNC and non-SOV share
    tnc_share <- initial_share[3] + initial_share[4]
    non_sov_share <- 1 - initial_share[1]
    #Defining vector of seven elasticities
    if (- ((-tnc_dollar_effect - tnc_minute_effect) *
         tnc_share + nonshare_dollar_effect * initial_share[3] /
         non_sov_share) < initial_share[3]) {
      elasticities <- c(
        -nonshare_dollar_effect - carpool_reward_effect,  #Car (drive)
        (nonshare_dollar_effect * initial_share[2] / non_sov_share) +
          carpool_reward_effect,   #Car (passenger)
        (-tnc_dollar_effect - tnc_minute_effect) * tnc_share +
          nonshare_dollar_effect * initial_share[3] /
          non_sov_share,  #TNC (private)
        (tnc_dollar_effect + tnc_minute_effect) * tnc_share +
          nonshare_dollar_effect * initial_share[4] /
          non_sov_share,  #TNC (shared)
        nonshare_dollar_effect * initial_share[5] /
          non_sov_share,  #Transit
        nonshare_dollar_effect * initial_share[6] /
          non_sov_share,  #Walk
        nonshare_dollar_effect * initial_share[7] /
          non_sov_share #Bike
      )}
    else{
      elasticities <- c(
        -nonshare_dollar_effect - carpool_reward_effect,  #Car (drive)
        (nonshare_dollar_effect * initial_share[2] / non_sov_share) +
          carpool_reward_effect,   #Car (passenger)
        -initial_share[3],  #TNC (private)
        initial_share[3],  #TNC (shared)
        nonshare_dollar_effect * initial_share[5] / non_sov_share,  #Transit
        nonshare_dollar_effect * initial_share[6] / non_sov_share,  #Walk
        nonshare_dollar_effect * initial_share[7] / non_sov_share #Bike
      )
    }
    new_share <-
      initial_share + elasticities
    # New share ----
    plot_data_temp <- data.frame(mode,  initial_share,  new_share)
    plot_data <- melt(plot_data_temp,  id.vars = "mode")
    stacked_bar <- ggplot(plot_data,  aes(x = mode,
                                         y = value,
                                         fill = factor(variable,
                                          labels = c("Initial Share",
                                                                  "New Share")),
                                         text = paste("Mode:",  mode,
                                                      "<br>Share of Trips:",
                                                      round(value * 100,  1),
                                                      "%"))) +
      scale_fill_manual(values = c("#2b4858",  "#8a2938")) +
      scale_y_continuous(breaks = c(0,  .25,  .50,  .75,  1),
                         labels = c("0%",  "25%",  "50%",  "75%",  "100%")) +
      geom_bar(stat = "identity",  width = 0.7,  position = "dodge") +
      ggtitle(label =
                "Initial Mode Share (Person Trips) Compared to Scenario") +
      labs(x = "Mode",  y = "Share",  fill = "Mode Share") +
      theme_minimal(base_size = 16) +
      theme(
            legend.position = "top",
            legend.title = element_blank(),
            legend.background = element_rect(
              fill = "white",  size = 0.2,  linetype = "solid"),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            plot.title = element_text(hjust = 0.5,
                                      vjust = -1.5,
                                      size = 18,  color = "grey5"),
            axis.title.y = element_text(size = 12,  color = "grey5"),
            axis.text.y = element_text(size = 12,  color = "grey5"),
            panel.grid.major.y = element_line(colour = "grey5",  size = 0.5),
            panel.grid.minor.y = element_line(colour = "grey5",  size = 0.5),
            panel.grid.major = element_blank())
    test <- ggplotly(stacked_bar,  tooltip = "text",  originalData = T)
      plotly::layout(test,  legend = list(x = 0.825,  y = 1))
  })
  # Show the values for VMT and mode share in an HTML table -----
  output$values <- renderTable({
    create_table()
  },
  width = NULL,
  align = "lrrrrr")
  # Output segment info for a callout box -----
  output$segments_chosen <- renderUI({
    geography <- input$geography
    segment <- input$segment
    peak <- input$peak
    str1 <- paste0(
      "Table of results.
      Note that data below represent annual person trips in ",
      if_else(geography == "Study Cities",
              "all study cities",
              paste0("the ",  geography)
      ),
      ", ")
    str2 <- paste0(
      " for the following population segment: ",
      segment,
      " (",
      if_else(
        peak == 0,
        "all day",
        "AM/PM peak only"),
      ")"
    )
    str3 <- ""
    HTML(paste0("<i>", str1, str2, "<br/>", str3, "</i>"))
  }
  )
  # Create stacked bar ----
  output$stacked_bar <- renderPlotly({
    create_stacked_bar()
  })
  # Calculate sum of inputs for initial mode share override ----
  url <- a("at this link.",  href = "URL to come")
  output$link <- renderUI({
    tags$i(
      tagList("More information on this project available ",  url))
  })
  output$sum_of_inputs <- renderValueBox({
    sum <- as.numeric(input$override_car + input$override_carpool +
                      input$override_tnc_private + input$override_tnc_shared +
                      input$override_transit + input$override_walk +
                      input$override_bike) / 100
    sum <- percent(sum, .1)
    valueBox(
      NULL,
      sum
    )
  })
}