library(shiny)
library(leaflet)
library(dplyr)
library(maps)
library(htmltools)
library(sf)
library(ggplot2)

# Load Data
states <- read_sf("us-states.geojson")
breast_cancer_long <- read.csv("breast_cancer_long.csv")
cervical_cancer_long <- read.csv("cervical_cancer_long.csv")
sex_infect_years <- read.csv("sex_infect_years.csv")
syphilis <- read.csv("syphilis_long.csv")
chlamydia <- read.csv("chlamydia_long.csv")
gonorrhea <- read.csv("gonorrhea_long.csv")
mortality_race_long <- read.csv("mortality_race_long.csv")
infant_mortality_long <- read.csv("infant_mortality_long.csv")

breast_rate_clean <- read.csv("breast_rate_clean.csv")
cervical_rate_clean <- read.csv("cervical_rate_clean.csv")
syphilis_rate_clean <- read.csv("syphilis_rate_clean.csv")
syphilis_rate_clean$Footnotes <- NULL
gonorrhea_rate_clean <- read.csv("gonorrhea_rate_clean.csv")
gonorrhea_rate_clean$Footnotes <- NULL
chlamydia_rate_clean <- read.csv("chlamydia_rate_clean.csv")
chlamydia_rate_clean$Footnotes <- NULL
mortality_race <- read.csv("mortality_race.csv")
infant_death_rate_clean <- read.csv("infant_death_rate_clean.csv")



server <- function(input, output, session) {
  
  # Breast Cancer Map
  
  # Create named vector for clean race labels
  race_choices <- c(
    "Overall" = "Overall",
    "White" = "White",
    "Black" = "Black",
    "Hispanic" = "Hispanic",
    "Asian/Native Hawaiian" = "Asian_NativeHawaiian",
    "American Indian/Alaska Native" = "AmericanIndian_AlaskaNative"
  )
  
  # Update selectInput with clean labels
  updateSelectInput(session,
                    "race_filter",
                    choices = race_choices,
                    selected = "Overall")
  
  # REACTIVE DATA
  breast_cancer_filtered_race <- reactive({
    result <- breast_cancer_long  
    
    if (input$race_filter != "Overall") {
      result <- result %>%
        filter(race == input$race_filter)
    }
    result
  })
  
  # REACTIVE MAP DATA
  map_data <- reactive({
    aggregated <- breast_cancer_filtered_race() %>%
      group_by(state) %>%
      summarise(breast_cancer = mean(breast_cancer, na.rm = TRUE), .groups = 'drop')
    states %>%
      left_join(aggregated, by = c("name" = "state"))
  })
  
  # RENDER THE MAP
  output$cancer_map <- renderLeaflet({
    data <- map_data()
    if (is.list(data$breast_cancer)) {
      data$breast_cancer <- as.numeric(unlist(data$breast_cancer))
    }
    
    bins <- c(0, 30, 60, 90, 120, 150, 180, Inf)
    pal <- colorBin("RdPu", domain = data$breast_cancer, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%.1f per 100,000 women",
      data$name, data$breast_cancer
    ) %>% lapply(HTML)
    
    leaflet(data) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(breast_cancer),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~breast_cancer, opacity = 0.7,
                title = "Incidence Rate<br/>(per 100,000)",
                position = "bottomright")
  })
  
  # BREAST CANCER RACE PLOT
  output$breast_race_plot <- renderPlot({
    
    # Filter data for selected state
    state_data <- breast_cancer_long %>%
      filter(state == input$state, !is.na(breast_cancer))
    
    # Improve race labels for display
    state_data <- state_data %>%
      mutate(race_label = case_when(
        race == "White" ~ "White",
        race == "Black" ~ "Black",
        race == "Hispanic" ~ "Hispanic",
        race == "Asian_NativeHawaiian" ~ "Asian/Native\nHawaiian",
        race == "AmericanIndian_AlaskaNative" ~ "American Indian/\nAlaska Native",
        race == "Overall" ~ "Overall",
        TRUE ~ gsub("_", " ", race)
      ))
    
    # Define colors
    race_colors <- c(
      "White" = "#FF8BA0",
      "Black" = "#FFA8B5",
      "Hispanic" = "#FFCCCB",
      "Asian_NativeHawaiian" = "#FFDCD1",
      "AmericanIndian_AlaskaNative" = "#FF8886",
      "Overall" = "#FF8663"
    )
    
    # Create ggplot
    ggplot(state_data, aes(x = reorder(race_label, -breast_cancer),
                           y = breast_cancer,
                           fill = race)) +
      geom_col(width = 0.7, alpha = 0.9) +
      geom_text(aes(label = round(breast_cancer, 1)),
                vjust = -0.5,
                size = 4,
                fontface = "bold",
                color = "#2C3E50") +
      scale_fill_manual(values = race_colors) +
      labs(title = paste("Breast Cancer Incidence in", input$state),
           subtitle = "Per 100,000 Women",
           x = NULL,
           y = "Incidence Rate") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 20, color = "#2C3E50", hjust = 0.5),
        plot.subtitle = element_text(size = 14, color = "#555", hjust = 0.5, margin = margin(b = 20)),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, color = "#2C3E50"),
        axis.text.y = element_text(size = 12, color = "#2C3E50"),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#E0E0E0", linetype = "dashed"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "#FAFAFA", color = NA),
        plot.margin = margin(20, 20, 20, 20)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
    
  }, bg = "white")
  
  # BREAST CANCER TOP STATES RANKING PLOT
  output$top_states_plot <- renderPlot({
    
    # Filter data for selected race
    rank_data <- breast_cancer_long %>%
      filter(race == input$rank_race, !is.na(breast_cancer)) %>%
      arrange(desc(breast_cancer)) %>%
      head(input$top_n)
    
    # Create ggplot with horizontal bars
    ggplot(rank_data, aes(x = reorder(state, breast_cancer), y = breast_cancer)) +
      geom_col(fill = "#FF8BA0", alpha = 0.9, width = 0.7) +
      geom_text(aes(label = round(breast_cancer, 1)),
                hjust = -0.2,
                size = 4,
                fontface = "bold",
                color = "#2C3E50") +
      coord_flip() +
      labs(title = paste("Top", input$top_n, "States with Highest Breast Cancer Rates"),
           subtitle = paste("Race:", input$rank_race),
           x = NULL,
           y = "Incidence Rate (per 100,000 Women)") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 18, color = "#2C3E50", hjust = 0.5),
        plot.subtitle = element_text(size = 14, color = "#555", hjust = 0.5, margin = margin(b = 20)),
        axis.text.y = element_text(size = 12, color = "#2C3E50"),
        axis.text.x = element_text(size = 12, color = "#2C3E50"),
        axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 15)),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "#E0E0E0", linetype = "dashed"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "#FAFAFA", color = NA),
        plot.margin = margin(20, 20, 20, 20)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
    
  }, bg = "white")
  
  
  # Cervical Cancer Plot
  
  # Create named vector for clean race labels (same as breast cancer)
  cervical_race_choices <- c(
    "Overall" = "Overall",
    "White" = "White",
    "Black" = "Black",
    "Hispanic" = "Hispanic",
    "Asian/Native Hawaiian" = "Asian_NativeHawaiian",
    "American Indian/Alaska Native" = "AmericanIndian_AlaskaNative"
  )
  
  # Update cervical race filter choices with clean labels
  updateSelectInput(session,
                    "cervical_race_filter",
                    choices = cervical_race_choices,
                    selected = "Overall")
  
  # CERVICAL REACTIVE DATA - This recalculates automatically when cervical_race_filter changes
  cervical_cancer_filtered_race <- reactive({
    result <- cervical_cancer_long  
    
    if (input$cervical_race_filter != "Overall") {
      result <- result %>%
        filter(race == input$cervical_race_filter)
    }
    result
  })
  
  # CERVICAL REACTIVE MAP DATA - Aggregate data before joining
  cervical_map_data <- reactive({
    # First, aggregate the filtered data by state
    aggregated <- cervical_cancer_filtered_race() %>%
      group_by(state) %>%
      summarise(cervical_cancer = mean(cervical_cancer, na.rm = TRUE), .groups = 'drop')
    
    # Then join with states
    states %>%
      left_join(aggregated, by = c("name" = "state"))
  })
  
  # RENDER THE CERVICAL CANCER MAP
  output$cervical_map <- renderLeaflet({
    data <- cervical_map_data()
    
    # Make sure cervical_cancer is numeric, not a list
    if (is.list(data$cervical_cancer)) {
      data$cervical_cancer <- as.numeric(unlist(data$cervical_cancer))
    }
    
    bins <- c(0, 2, 4, 6, 8, 10, 12, Inf)
    pal <- colorBin("GnBu", domain = data$cervical_cancer, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%.1f per 100,000 women",
      data$name, data$cervical_cancer
    ) %>% lapply(HTML)
    
    leaflet(data) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(cervical_cancer),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~cervical_cancer, opacity = 0.7,
                title = "Incidence Rate<br/>(per 100,000)",
                position = "bottomright")
  })
  
  # CERVICAL CANCER RACE PLOT
  output$cervical_race_plot <- renderPlot({
    
    # Filter data for selected state
    state_data <- cervical_cancer_long %>%
      filter(state == input$cervical_state, !is.na(cervical_cancer))
    
    # Improve race labels for display
    state_data <- state_data %>%
      mutate(race_label = case_when(
        race == "White" ~ "White",
        race == "Black" ~ "Black",
        race == "Hispanic" ~ "Hispanic",
        race == "Asian_NativeHawaiian" ~ "Asian/Native\nHawaiian",
        race == "AmericanIndian_AlaskaNative" ~ "American Indian/\nAlaska Native",
        race == "Overall" ~ "Overall",
        TRUE ~ gsub("_", " ", race)
      ))
    
    # Define colors (teal/turquoise theme for cervical)
    race_colors <- c(
      "White" = "#C0F2F3",
      "Black" = "#81DBDB",
      "Hispanic" = "#48CBC5",
      "Asian_NativeHawaiian" = "#1E9C99",
      "AmericanIndian_AlaskaNative" = "#2A777C",
      "Overall" = "#064D51"
    )
    
    # Create ggplot
    ggplot(state_data, aes(x = reorder(race_label, -cervical_cancer),
                           y = cervical_cancer,
                           fill = race)) +
      geom_col(width = 0.7, alpha = 0.9) +
      geom_text(aes(label = round(cervical_cancer, 1)),
                vjust = -0.5,
                size = 4,
                fontface = "bold",
                color = "#2C3E50") +
      scale_fill_manual(values = race_colors) +
      labs(title = paste("Cervical Cancer Incidence in", input$cervical_state),
           subtitle = "Per 100,000 Women",
           x = NULL,
           y = "Incidence Rate") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 20, color = "#2C3E50", hjust = 0.5),
        plot.subtitle = element_text(size = 14, color = "#555", hjust = 0.5, margin = margin(b = 20)),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, color = "#2C3E50"),
        axis.text.y = element_text(size = 12, color = "#2C3E50"),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#E0E0E0", linetype = "dashed"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "#FAFAFA", color = NA),
        plot.margin = margin(20, 20, 20, 20)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
    
  }, bg = "white")
  
  # CERVICAL CANCER TOP STATES RANKING PLOT
  output$cervical_top_states_plot <- renderPlot({
    
    # Filter data for selected race
    rank_data <- cervical_cancer_long %>%
      filter(race == input$cervical_rank_race, !is.na(cervical_cancer)) %>%
      arrange(desc(cervical_cancer)) %>%
      head(input$cervical_top_n)
    
    # Create ggplot with horizontal bars
    ggplot(rank_data, aes(x = reorder(state, cervical_cancer), y = cervical_cancer)) +
      geom_col(fill = "#1E9C99", alpha = 0.9, width = 0.7) +
      geom_text(aes(label = round(cervical_cancer, 1)),
                hjust = -0.2,
                size = 4,
                fontface = "bold",
                color = "#2C3E50") +
      coord_flip() +
      labs(title = paste("Top", input$cervical_top_n, "States with Highest Cervical Cancer Rates"),
           subtitle = paste("Race:", input$cervical_rank_race),
           x = NULL,
           y = "Incidence Rate (per 100,000 Women)") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 18, color = "#2C3E50", hjust = 0.5),
        plot.subtitle = element_text(size = 14, color = "#555", hjust = 0.5, margin = margin(b = 20)),
        axis.text.y = element_text(size = 12, color = "#2C3E50"),
        axis.text.x = element_text(size = 12, color = "#2C3E50"),
        axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 15)),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "#E0E0E0", linetype = "dashed"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "#FAFAFA", color = NA),
        plot.margin = margin(20, 20, 20, 20)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
    
  }, bg = "white")
  
  
  # STI map
  
  # Create named vector for clean race labels (same as breast cancer)
  std_race_choices <- c(
    "Overall" = "Overall",
    "White" = "White",
    "Black" = "Black",
    "Hispanic" = "Hispanic",
    "Asian/Native Hawaiian" = "Asian_NativeHawaiian",
    "American Indian/Alaska Native" = "AmericanIndian_AlaskaNative"
  )
  
  # Update cervical race filter choices with clean labels
  updateSelectInput(session,
                    "cervical_race_filter",
                    choices = cervical_race_choices,
                    selected = "Overall")
  
  # Update race filter choices for STI map
  updateSelectInput(session,
                    "sti_race_filter",
                    choices = std_race_choices,
                    selected = "Overall")
  
  # Update disease filter choices for STI map
  updateSelectInput(session,
                    "sti_disease_filter",
                    choices = c("syphilis", "chlamydia", "gonorrhea"))
  
  # REACTIVE DATA - Filter by race
  sti_filtered_race <- reactive({
    # Select dataset based on disease
    if(input$sti_disease_filter == "syphilis") {
      result <- syphilis %>%
        rename(rate = syphilis)
    } else if(input$sti_disease_filter == "chlamydia") {
      result <- chlamydia %>%
        rename(rate = chlamydia)
    } else {  # gonorrhea
      result <- gonorrhea %>%
        rename(rate = gonorrhea)
    }
    
    # Filter by race if not "All"
    if (input$sti_race_filter != "Overall") {
      result <- result %>%
        filter(race == input$sti_race_filter)
    }
    
    result
  })
  
  # REACTIVE MAP DATA - Aggregate data before joining with states
  sti_map_data <- reactive({
    # Aggregate the filtered data by state
    aggregated <- sti_filtered_race() %>%
      group_by(state) %>%
      summarise(sti_rate = mean(rate, na.rm = TRUE), .groups = 'drop')
    
    # Join with states shapefile
    states %>%
      left_join(aggregated, by = c("name" = "state"))
  })
  
  # RENDER THE STI MAP
  output$sti_map <- renderLeaflet({
    data <- sti_map_data()
    
    # Make sure sti_rate is numeric, not a list
    if (is.list(data$sti_rate)) {
      data$sti_rate <- as.numeric(unlist(data$sti_rate))
    }
    
    # Define bins based on disease type (different scales)
    if(input$sti_disease_filter == "chlamydia") {
      bins <- c(0, 200, 400, 600, 800, 1000, 1200, Inf)
      pal <- colorBin("Purples", domain = data$sti_rate, bins = bins)
    } else if(input$sti_disease_filter == "syphilis") {
      bins <- c(0, 10, 20, 30, 40, 50, 60, Inf)
      pal <- colorBin("Oranges", domain = data$sti_rate, bins = bins)
    } else {  # gonorrhea
      bins <- c(0, 100, 200, 300, 400, 500, 600, Inf)
      pal <- colorBin("Greens", domain = data$sti_rate, bins = bins)
    }
    
    # Create labels
    disease_name <- tools::toTitleCase(input$sti_disease_filter)
    labels <- sprintf(
      "<strong>%s</strong><br/>%s: %.1f per 100,000",
      data$name, disease_name, data$sti_rate
    ) %>% lapply(HTML)
    
    # Create map
    leaflet(data) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(sti_rate),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~sti_rate, opacity = 0.7,
                title = paste(disease_name, "<br/>Rate per 100,000"),
                position = "bottomright")
  })
  
  
  # STI Time Series: All Three Diseases Over Years (OPTION 1 - Single Chart)
  output$sti_plot <- renderPlot({
    
    # Filter data for selected state and race, all diseases
    plot_data <- sex_infect_years %>%
      filter(state == input$sti_state,
             race == input$sti_race,
             !is.na(rate))
    
    # Check if we have data
    if(nrow(plot_data) == 0) {
      plot(1, type="n", xlim=c(0, 10), ylim=c(0, 10),
           xlab="", ylab="", main="No data available")
      text(5, 5, "No data available\nfor the selected filters", cex=1.5, col="red")
      return()
    }
    
    # Capitalize disease names for better display
    plot_data <- plot_data %>%
      mutate(disease_label = tools::toTitleCase(disease))
    
    # Define colors for diseases
    disease_colors <- c(
      "Chlamydia" = "#9B59B6",
      "Syphilis" = "#E67E22",
      "Gonorrhea" = "#1ABC9C"
    )
    
    # Create ggplot line graph
    ggplot(plot_data, aes(x = year, y = rate, color = disease_label, group = disease_label)) +
      geom_line(size = 2.5, alpha = 0.9) +
      geom_point(size = 4.5, alpha = 0.95) +
      geom_text(data = plot_data %>%
                  group_by(disease_label) %>%
                  filter(year == max(year)),
                aes(label = round(rate, 1)),
                vjust = -1.2,
                hjust = 0.5,
                size = 4.5,
                fontface = "bold",
                show.legend = FALSE) +
      scale_color_manual(values = disease_colors) +
      scale_x_continuous(breaks = unique(plot_data$year)) +
      labs(title = paste("STI Trends in", input$sti_state),
           subtitle = paste("Race:", input$sti_race, "| Years",
                            min(plot_data$year), "-", max(plot_data$year)),
           x = "Year",
           y = "Rate per 100,000",
           color = "Disease") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 20, color = "#2C3E50", hjust = 0.5),
        plot.subtitle = element_text(size = 14, color = "#555", hjust = 0.5, margin = margin(b = 20)),
        axis.text.x = element_text(size = 13, color = "#2C3E50", face = "bold"),
        axis.text.y = element_text(size = 12, color = "#2C3E50"),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.position = "top",
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 12),
        legend.background = element_rect(fill = "white", color = "#E0E0E0", linewidth = 1),
        legend.margin = margin(b = 10),
        legend.key.size = unit(1.5, "lines"),
        panel.grid.major.x = element_line(color = "#E0E0E0", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#E0E0E0", linetype = "dashed"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "#FAFAFA", color = NA),
        plot.margin = margin(20, 20, 20, 20)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))
    
  }, bg = "white")
  
  
  
  
  # STI Chart 2: One Disease by Race
  output$sti_race_plot <- renderPlot({
    
    # Select the appropriate dataset based on chosen disease
    if(input$sti_disease == "chlamydia") {
      plot_data <- chlamydia %>%
        filter(state == input$sti_state2) %>%
        select(race, rate = chlamydia)
    } else if(input$sti_disease == "syphilis") {
      plot_data <- syphilis %>%
        filter(state == input$sti_state2) %>%
        select(race, rate = syphilis)
    } else {  # gonorrhea
      plot_data <- gonorrhea %>%
        filter(state == input$sti_state2) %>%
        select(race, rate = gonorrhea)
    }
    
    # Remove NA values
    plot_data <- plot_data %>%
      filter(!is.na(rate))
    
    # Check if we have data
    if(nrow(plot_data) == 0) {
      plot(1, type="n", xlim=c(0, 10), ylim=c(0, 10),
           xlab="", ylab="", main="No data available")
      text(5, 5, "No data available\nfor the selected filters", cex=1.5, col="red")
      return()
    }
    
    # Improve race labels for display
    plot_data <- plot_data %>%
      mutate(race_label = case_when(
        race == "White" ~ "White",
        race == "Black" ~ "Black",
        race == "Hispanic" ~ "Hispanic",
        race == "Asian_NativeHawaiian" ~ "Asian/Native\nHawaiian",
        race == "AmericanIndian_AlaskaNative" ~ "American Indian/\nAlaska Native",
        race == "Overall" ~ "Overall",
        TRUE ~ gsub("_", " ", race)
      ))
    
    # Define colors for races
    race_colors <- c(
      "White" = "#4A90E2",
      "Black" = "#E74C3C",
      "Hispanic" = "#27AE60",
      "Asian_NativeHawaiian" = "#F39C12",
      "AmericanIndian_AlaskaNative" = "#9B59B6",
      "Overall" = "#34495E"
    )
    
    # Create ggplot
    ggplot(plot_data, aes(x = reorder(race_label, -rate),
                          y = rate,
                          fill = race)) +
      geom_col(width = 0.7, alpha = 0.9) +
      geom_text(aes(label = round(rate, 1)),
                vjust = -0.5,
                size = 4.5,
                fontface = "bold",
                color = "#2C3E50") +
      scale_fill_manual(values = race_colors) +
      labs(title = paste(tools::toTitleCase(input$sti_disease), "Rates in", input$sti_state2),
           subtitle = "By Race",
           x = NULL,
           y = "Rate per 100,000") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 20, color = "#2C3E50", hjust = 0.5),
        plot.subtitle = element_text(size = 14, color = "#555", hjust = 0.5, margin = margin(b = 20)),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11, color = "#2C3E50"),
        axis.text.y = element_text(size = 12, color = "#2C3E50"),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#E0E0E0", linetype = "dashed"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "#FAFAFA", color = NA),
        plot.margin = margin(20, 20, 20, 20)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
    
  }, bg = "white")
  
  # Maternal Mortality State Plot
  
  # Maternal Mortality State Plot
  maternal_mortality <- read.csv("mortality_year_long.csv", stringsAsFactors = FALSE)
  maternal_mortality <- as.data.frame(maternal_mortality)
  maternal_mortality$year <- as.numeric(maternal_mortality$year)
  maternal_mortality$maternal_mortality <- as.numeric(maternal_mortality$maternal_mortality)
  
  # Remove rows with NA in race column
  maternal_mortality <- maternal_mortality %>%
    filter(!is.na(race))
  
  # Create named vector for clean race labels
  maternal_race_choices <- c(
    "Overall" = "Overall",
    "White" = "White",
    "Black" = "Black",
    "Hispanic" = "Hispanic",
    "Asian/Native Hawaiian" = "Asian_NativeHawaiian",
    "American Indian/Alaska Native" = "AmericanIndian_AlaskaNative"
  )
  
  # Update maternal mortality race filter choices with clean labels
  updateSelectInput(session,
                    "maternal_race_filter",
                    choices = maternal_race_choices,
                    selected = "Overall")
  
  
  # MATERNAL MORTALITY REACTIVE DATA - Filter by race and year
  maternal_mortality_filtered <- reactive({
    result <- maternal_mortality  
    
    if (input$maternal_race_filter != "Overall") {
      result <- result %>%
        filter(race == input$maternal_race_filter)
    }
    
    # Filter by the year from the slider
    result <- result %>%
      filter(year == input$maternal_year_slider)
    
    result
  })
  
  # MATERNAL MORTALITY REACTIVE MAP DATA - Aggregate data before joining
  maternal_map_data <- reactive({
    # First, aggregate the filtered data by state
    aggregated <- maternal_mortality_filtered() %>%
      group_by(state) %>%
      summarise(maternal_mortality = mean(maternal_mortality, na.rm = TRUE), .groups = 'drop')
    
    # Then join with states
    states %>%
      left_join(aggregated, by = c("name" = "state"))
  })
  
  # RENDER THE MATERNAL MORTALITY MAP
  output$maternal_map <- renderLeaflet({
    data <- maternal_map_data()
    
    # Make sure maternal_mortality is numeric, not a list
    if (is.list(data$maternal_mortality)) {
      data$maternal_mortality <- as.numeric(unlist(data$maternal_mortality))
    }
    
    bins <- c(0, 5, 10, 15, 20, 25, 30, Inf)
    pal <- colorBin("YlOrRd", domain = data$maternal_mortality, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%.1f per 100,000 live births<br/>Year: %d",
      data$name, data$maternal_mortality, input$maternal_year_slider
    ) %>% lapply(HTML)
    
    leaflet(data) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(maternal_mortality),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~maternal_mortality, opacity = 0.7,
                title = paste0("Maternal Mortality<br/>(per 100,000)<br/>Year: ", input$maternal_year_slider),
                position = "bottomright")
  })
  
  # Populate state choices based on available data
  observe({
    if(input$mortality_type == "Maternal Mortality") {
      states <- unique(mortality_race_long$state)
    } else {
      states <- unique(infant_mortality_long$state)
    }
    states <- sort(states[!is.na(states)])
    
    # Make sure "United States" is in the list and set it as default
    if("United States" %in% states) {
      updateSelectInput(session, "mortality_disparity_state",
                        choices = states,
                        selected = "United States")
    } else {
      # If "United States" doesn't exist, select the first state
      updateSelectInput(session, "mortality_disparity_state",
                        choices = states,
                        selected = states[1]) }
  })
  
  # COMBINED DIVERGING BAR CHART: Maternal & Infant Mortality
  output$mortality_disparity_plot <- renderPlot({
    
    # Select dataset based on user choice
    if(input$mortality_type == "Maternal Mortality") {
      # Aggregate maternal mortality data (average across years if multiple rows)
      state_data <- mortality_race_long %>%
        filter(state == input$mortality_disparity_state, !is.na(maternal_mortality)) %>%
        group_by(state, race) %>%
        summarise(rate = mean(maternal_mortality, na.rm = TRUE), .groups = 'drop')
      
      y_label <- "Difference from Overall Rate (per 100,000 live births)"
      rate_label <- "per 100,000 live births"
      chart_title <- "Maternal Mortality"
    } else {  # Infant Mortality
      # Aggregate infant mortality data (average across years if multiple rows)
      state_data <- infant_mortality_long %>%
        filter(state == input$mortality_disparity_state, !is.na(infant_mortality)) %>%
        group_by(state, race) %>%
        summarise(rate = mean(infant_mortality, na.rm = TRUE), .groups = 'drop')
      
      y_label <- "Difference from Overall Rate (per 1,000 live births)"
      rate_label <- "per 1,000 live births"
      chart_title <- "Infant Mortality"
    }
    
    # Check if we have data
    if(nrow(state_data) == 0) {
      plot(1, type="n", xlim=c(0, 10), ylim=c(0, 10), 
           xlab="", ylab="", main="No data available")
      text(5, 5, "No data available\nfor the selected state", cex=1.5, col="red")
      return()
    }
    
    # Get the overall/baseline rate
    overall_rate <- state_data %>%
      filter(race == "Overall") %>%
      pull(rate)
    
    # If no overall rate, calculate mean
    if(length(overall_rate) == 0) {
      overall_rate <- mean(state_data$rate, na.rm = TRUE)
    } else {
      overall_rate <- overall_rate[1]
    }
    
    # Calculate difference from overall rate
    plot_data <- state_data %>%
      filter(race != "Overall") %>%
      mutate(difference = rate - overall_rate,
             direction = ifelse(difference > 0, "Above Average", "Below Average")) %>%
      arrange(difference)
    
    # Check if we have plot data after filtering
    if(nrow(plot_data) == 0) {
      plot(1, type="n", xlim=c(0, 10), ylim=c(0, 10), 
           xlab="", ylab="", main="No data available")
      text(5, 5, "No racial data available\nfor the selected state", cex=1.5, col="red")
      return()
    }
    
    # Improve race labels for display
    plot_data <- plot_data %>%
      mutate(race_label = case_when(
        race == "White" ~ "White",
        race == "Black" ~ "Black",
        race == "Hispanic" ~ "Hispanic",
        race == "Asian_NativeHawaiian" ~ "Asian/Native Hawaiian",
        race == "AmericanIndian_AlaskaNative" ~ "American Indian/Alaska Native",
        race == "Asian" ~ "Asian",
        race == "Native_Hawaiian_or_PacificIslander" ~ "Native Hawaiian/Pacific Islander",
        race == "Multiple races" ~ "Multiple Races",
        race == "Multiple.races" ~ "Multiple Races",
        race == "Overall" ~ "Overall",
        TRUE ~ gsub("_", " ", race)
      ))
    
    # Create ggplot diverging bar chart
    ggplot(plot_data, aes(x = reorder(race_label, difference), y = difference, fill = direction)) +
      geom_col(width = 0.7, alpha = 0.9) +
      geom_hline(yintercept = 0, color = "#2C3E50", size = 1.5, linetype = "solid") +
      geom_text(aes(label = paste0(ifelse(difference > 0, "+", ""), round(difference, 1))),
                hjust = ifelse(plot_data$difference > 0, -0.2, 1.2),
                size = 4.5,
                fontface = "bold",
                color = "#2C3E50") +
      coord_flip() +
      scale_fill_manual(values = c("Above Average" = "#E74C3C", "Below Average" = "#27AE60")) +
      labs(title = paste(chart_title, "Disparity Analysis:", input$mortality_disparity_state),
           subtitle = paste0("Difference from Overall Rate (", round(overall_rate, 1), " ", rate_label, ")"),
           x = NULL,
           y = y_label,
           fill = NULL) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 20, color = "#2C3E50", hjust = 0.5),
        plot.subtitle = element_text(size = 13, color = "#555", hjust = 0.5, margin = margin(b = 20)),
        axis.text.y = element_text(size = 12, color = "#2C3E50", face = "bold"),
        axis.text.x = element_text(size = 12, color = "#2C3E50"),
        axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 15)),
        legend.position = "top",
        legend.text = element_text(size = 12, face = "bold"),
        legend.key.size = unit(1, "cm"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "#E0E0E0", linetype = "dashed"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "#FAFAFA", color = NA),
        plot.margin = margin(20, 40, 20, 20)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.15, 0.15)))
    
  }, bg = "white")
  
  
  
  # Populate pie chart state choices
  # Populate pie chart state choices
  observe({
    if(input$pie_mortality_type == "Maternal Mortality") {
      states <- unique(mortality_race_long$state)
    } else {
      states <- unique(infant_mortality_long$state)
    }
    states <- sort(states[!is.na(states)])
    
    # Keep "United States" selected if it exists in the list
    if("United States" %in% states) {
      updateSelectInput(session, "pie_state",
                        choices = states,
                        selected = "United States")
    } else {
      updateSelectInput(session, "pie_state",
                        choices = states,
                        selected = states[1])
    }
  })
  
  # Render Pie Chart
  output$mortality_pie_chart <- renderPlot({
    
    # Select dataset based on user choice
    if(input$pie_mortality_type == "Maternal Mortality") {
      # Aggregate maternal mortality data
      pie_data <- mortality_race_long %>%
        filter(state == input$pie_state, 
               race != "Overall",
               !is.na(maternal_mortality)) %>%
        group_by(race) %>%
        summarise(rate = mean(maternal_mortality, na.rm = TRUE), .groups = 'drop')
      
      chart_title <- paste("Maternal Mortality Rates in", input$pie_state)
      rate_label <- "per 100,000 live births"
    } else {  # Infant Mortality
      # Aggregate infant mortality data
      pie_data <- infant_mortality_long %>%
        filter(state == input$pie_state, 
               race != "Overall",
               !is.na(infant_mortality)) %>%
        group_by(race) %>%
        summarise(rate = mean(infant_mortality, na.rm = TRUE), .groups = 'drop')
      
      chart_title <- paste("Infant Mortality Rates in", input$pie_state)
      rate_label <- "per 1,000 live births"
    }
    
    # Check if we have data
    if(nrow(pie_data) == 0) {
      plot(1, type="n", xlim=c(0, 10), ylim=c(0, 10), 
           xlab="", ylab="", main="No data available")
      text(5, 5, "No data available\nfor the selected state", cex=1.5, col="red")
      return()
    }
    
    # Clean up race labels for display
    pie_data <- pie_data %>%
      mutate(race_label = case_when(
        race == "White" ~ "White",
        race == "Black" ~ "Black",
        race == "Hispanic" ~ "Hispanic",
        race == "Asian_NativeHawaiian" ~ "Asian/Native Hawaiian",
        race == "American Indian and Alaska Native" ~ "American Indian/Alaska Native",
        race == "Asian" ~ "Asian",
        race == "Asian or Native Hawaiian" ~ "Native Hawaiian/Pacific Islander",
        race == "Multiple races" ~ "Multiple Races",
        race == "Multiple.races" ~ "Multiple Races",
        TRUE ~ gsub("_", " ", race)
      ))
    
    # Define colors for races
    race_colors <- c(
      "White" = "#FFCDD2",
      "Black" = "#ff7169",
      "Hispanic" = "#ffa178",
      "Asian/Native Hawaiian" = "#fffcc1",
      "American Indian/Alaska Native" = "#ffca93",
      "Asian" = "#F44336",
      "Native Hawaiian/Pacific Islander" = "#ffe3af",
      "Multiple Races" = "#B71C1C"
    )
    
    # Calculate percentages for labels
    pie_data <- pie_data %>%
      mutate(percentage = round((rate / sum(rate)) * 100, 1),
             label_text = paste0(race_label, "\n", round(rate, 1), "\n(", percentage, "%)"))
    
    # Create pie chart using base R
    par(bg = "white", mar = c(2, 2, 4, 2))
    
    # Get colors for the data
    slice_colors <- sapply(pie_data$race_label, function(r) {
      if(r %in% names(race_colors)) {
        return(race_colors[r])
      } else {
        return("#95A5A6")
      }
    })
    
    # Create the pie chart
    pie(pie_data$rate,
        labels = pie_data$label_text,
        col = slice_colors,
        main = chart_title,
        cex.main = 1.8,
        font.main = 2,
        col.main = "#2C3E50",
        cex = 1.1,
        radius = 0.9)
    
    # Add subtitle
    mtext(paste("Rates", rate_label), 
          side = 3, line = 0.5, cex = 1.1, col = "#555")
    
    # Add legend
    legend("bottomright",
           legend = pie_data$race_label,
           fill = slice_colors,
           cex = 0.9,
           bg = "white",
           box.col = "#E0E0E0")
    
  }, bg = "white")
  
  # Breast Cancer vs STI Scatter Plot
  output$breast_sti_scatter <- renderPlot({
    # Calculate averages
    sex_infect_avg <- sex_infect_years %>%
      group_by(state) %>%
      summarise(sti_rate = mean(rate, na.rm = TRUE))
    
    breast_cancer_avg <- breast_cancer_long %>%
      group_by(state) %>%
      summarise(breast_rate = mean(breast_cancer, na.rm = TRUE))
    
    # Merge datasets
    breast_sti <- breast_cancer_avg %>%
      inner_join(sex_infect_avg, by = "state")
    
    # Create scatter plot
    ggplot(breast_sti, aes(x = breast_rate, y = sti_rate)) +
      geom_point(color = "#7B68EE", size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "#2C3E50", linetype = "dashed") +
      geom_text(aes(label = state), size = 2.5, vjust = -0.5, hjust = 0.5) +
      labs(
        title = "Breast Cancer Incidence vs Sexual Infection Rate by State",
        x = "Breast Cancer Incidence Rate (per 100,000)",
        y = "Sexual Infection Rate (per 100,000)",
        caption = paste("Correlation:", round(cor(breast_sti$breast_rate, breast_sti$sti_rate, use = "complete.obs"), 3))
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  # Cervical Cancer vs STI Scatter Plot
  output$cervical_sti_scatter <- renderPlot({
    # Calculate averages
    sex_infect_avg <- sex_infect_years %>%
      group_by(state) %>%
      summarise(sti_rate = mean(rate, na.rm = TRUE))
    
    cervical_cancer_avg <- cervical_cancer_long %>%
      group_by(state) %>%
      summarise(cervical_rate = mean(cervical_cancer, na.rm = TRUE))
    
    # Merge datasets
    cervical_sti <- cervical_cancer_avg %>%
      inner_join(sex_infect_avg, by = "state")
    
    # Create scatter plot
    ggplot(cervical_sti, aes(x = cervical_rate, y = sti_rate)) +
      geom_point(color = "#E91E63", size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "#2C3E50", linetype = "dashed") +
      geom_text(aes(label = state), size = 2.5, vjust = -0.5, hjust = 0.5) +
      labs(
        title = "Cervical Cancer Incidence vs Sexual Infection Rate by State",
        x = "Cervical Cancer Incidence Rate (per 100,000)",
        y = "Sexual Infection Rate (per 100,000)",
        caption = paste("Correlation:", round(cor(cervical_sti$cervical_rate, cervical_sti$sti_rate, use = "complete.obs"), 3))
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  # Populate state choices for correlation plot
  observe({
    states_regions <- unique(maternal_mortality$state)
    states_regions <- sort(states_regions[!is.na(states_regions)])
    
    # Prioritize National at the top if it exists
    if("National" %in% states_regions) {
      states_regions <- c("National", states_regions[states_regions != "National"])
    }
    
    updateSelectInput(session, "maternal_corr_state",
                      choices = states_regions,
                      selected = "National")
  })
  
  # MATERNAL MORTALITY RACE CORRELATION PLOT
  output$maternal_race_correlation <- renderPlot({
    
    # Validate inputs
    req(input$maternal_corr_state)
    req(input$maternal_races_select)
    req(input$maternal_year_range)
    
    # Filter data
    plot_data <- maternal_mortality %>%
      filter(state == input$maternal_corr_state,
             race %in% input$maternal_races_select,
             year >= input$maternal_year_range[1],
             year <= input$maternal_year_range[2],
             !is.na(maternal_mortality))
    
    # Check if we have data
    if(nrow(plot_data) == 0) {
      plot(1, type="n", xlim=c(0, 10), ylim=c(0, 10),
           xlab="", ylab="", main="No data available")
      text(5, 5, "No data available\nfor the selected filters", cex=1.5, col="red")
      return()
    }
    
    # Define colors for races
    race_colors <- c(
      "White" = "#4A90E2",
      "Black" = "#E74C3C",
      "Hispanic" = "#27AE60",
      "Asian or Native Hawaiian" = "#F39C12",
      "American Indian and Alaska Native" = "#9B59B6"
    )
    
    # Create ggplot line graph
    ggplot(plot_data, aes(x = year, y = maternal_mortality, 
                          color = race, group = race)) +
      geom_line(size = 2, alpha = 0.9) +
      geom_point(size = 3.5, alpha = 0.8) +
      geom_smooth(method = "loess", se = TRUE, alpha = 0.15, size = 0.8, 
                  linetype = "dashed") +
      scale_color_manual(values = race_colors) +
      scale_x_continuous(breaks = seq(input$maternal_year_range[1], 
                                      input$maternal_year_range[2], 
                                      by = 2)) +
      labs(title = paste("Maternal Mortality Trends by Race:", input$maternal_corr_state),
           subtitle = paste("Years", input$maternal_year_range[1], "-", 
                            input$maternal_year_range[2]),
           x = "Year",
           y = "Maternal Mortality Rate (per 100,000 live births)",
           color = "Race/Ethnicity") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 20, color = "#2C3E50", 
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14, color = "#555", hjust = 0.5, 
                                     margin = margin(b = 20)),
        axis.text.x = element_text(size = 11, color = "#2C3E50", angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(size = 12, color = "#2C3E50"),
        axis.title.x = element_text(size = 14, face = "bold", 
                                    margin = margin(t = 15)),
        axis.title.y = element_text(size = 14, face = "bold", 
                                    margin = margin(r = 15)),
        legend.position = "right",
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 11),
        legend.background = element_rect(fill = "white", color = "#E0E0E0", 
                                         linewidth = 1),
        legend.key.size = unit(1.2, "lines"),
        panel.grid.major = element_line(color = "#E0E0E0", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "#FAFAFA", color = NA),
        plot.margin = margin(20, 20, 20, 20)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
    
  }, bg = "white")
  
  
  # Populate state choices for infant mortality
  observe({
    available_states <- c("All States", sort(unique(infant_mortality_long$state[!is.na(infant_mortality_long$state)])))
    
    updateSelectInput(session, "infant_corr_state",
                      choices = available_states,
                      selected = "All States")
  })
  
  # INFANT MORTALITY RACE CORRELATION ANALYSIS
  output$infant_race_correlation <- renderPlot({
    
    # Validate inputs
    req(input$infant_corr_state)
    
    # Filter data based on state selection
    if(input$infant_corr_state == "All States") {
      plot_data <- infant_mortality_long %>%
        filter(race != "Overall",
               !is.na(infant_mortality),
               !is.na(state))
    } else {
      plot_data <- infant_mortality_long %>%
        filter(state == input$infant_corr_state,
               race != "Overall",
               !is.na(infant_mortality))
    }
    
    # Check if we have data
    if(nrow(plot_data) == 0) {
      plot(1, type="n", xlim=c(0, 10), ylim=c(0, 10),
           xlab="", ylab="", main="No data available")
      text(5, 5, "No data available\nfor the selected filters", cex=1.5, col="red")
      return()
    }
    
    # Calculate mean and confidence intervals for each race
    summary_data <- plot_data %>%
      group_by(race) %>%
      summarise(
        mean_mortality = mean(infant_mortality, na.rm = TRUE),
        median_mortality = median(infant_mortality, na.rm = TRUE),
        sd_mortality = sd(infant_mortality, na.rm = TRUE),
        n = n(),
        se = sd_mortality / sqrt(n),
        ci_lower = mean_mortality - 1.96 * se,
        ci_upper = mean_mortality + 1.96 * se
      ) %>%
      arrange(desc(mean_mortality))
    
    # Define colors for races
    race_colors <- c(
      "White" = "#4A90E2",
      "Black" = "#E74C3C",
      "Hispanic" = "#27AE60",
      "Asian or Native Hawaiian" = "#F39C12",
      "Asian" = "#F39C12",
      "American Indian and Alaska Native" = "#9B59B6",
      "Native Hawaiian or Pacific Islander" = "#E67E22",
      "Native_Hawaiian_or_PacificIslander" = "#E67E22",
      "Multiple races" = "#95A5A6",
      "Multiple.races" = "#95A5A6"
    )
    
    # Create the plot
    ggplot(summary_data, aes(x = reorder(race, mean_mortality), 
                             y = mean_mortality, 
                             fill = race)) +
      geom_col(alpha = 0.9, width = 0.7) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                    width = 0.3, size = 1, color = "#2C3E50") +
      geom_text(aes(label = sprintf("%.2f", mean_mortality)),
                vjust = -2.5, size = 4.5, fontface = "bold", color = "#2C3E50") +
      geom_text(aes(label = paste0("n=", n)),
                vjust = -1, size = 3.5, color = "#555555") +
      scale_fill_manual(values = race_colors) +
      labs(title = ifelse(input$infant_corr_state == "All States",
                          "Correlation: Race and Infant Mortality Rates (All States)",
                          paste("Correlation: Race and Infant Mortality Rates -", input$infant_corr_state)),
           subtitle = "Mean rates with 95% confidence intervals",
           x = "Race/Ethnicity",
           y = "Mean Infant Mortality Rate (per 1,000 live births)",
           fill = "Race/Ethnicity") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 18, color = "#2C3E50", 
                                  hjust = 0.5, margin = margin(b = 5)),
        plot.subtitle = element_text(size = 12, color = "#555555", 
                                     hjust = 0.5, margin = margin(b = 20)),
        axis.text.x = element_text(size = 11, color = "#2C3E50", angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(size = 12, color = "#2C3E50"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#E0E0E0", linetype = "dashed"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "#FAFAFA", color = NA),
        plot.margin = margin(20, 20, 20, 20)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
    
  }, bg = "white")
  
  
  # Reactive dataset based on user selection
  selected_dataset <- reactive({
    switch(input$dataset_choice,
           "breast_rate" = breast_rate_clean,
           "cervical_rate" = cervical_rate_clean,
           "syphilis_rate" = syphilis_rate_clean,
           "gonorrhea_rate" = gonorrhea_rate_clean,
           "chlamydia_rate" = chlamydia_rate_clean,
           "mortality_race" = mortality_race,
           "infant_death_rate" = infant_death_rate_clean
    )
  })
  
  # Display dataset information
  output$dataset_info <- renderUI({
    data <- selected_dataset()
    
    info_text <- switch(input$dataset_choice,
                        "breast_rate" = "Breast cancer incidence rates by state and demographics.",
                        "cervical_rate" = "Cervical cancer incidence rates across different populations.",
                        "syphilis_rate" = "Syphilis infection rates and statistics.",
                        "gonorrhea_rate" = "Gonorrhea infection rates and statistics.",
                        "chlamydia_rate" = "Chlamydia infection rates and statistics.",
                        "mortality_race" = "Maternal mortality rates by race and ethnicity.",
                        "infant_death_rate" = "Infant mortality rates by state and demographic factors."
    )
    
    tagList(
      p(style = "color: #555; margin-bottom: 10px; line-height: 1.6;", info_text),
      p(style = "color: #2C3E50; margin: 5px 0;", 
        strong("Rows: "), nrow(data)),
      p(style = "color: #2C3E50; margin: 5px 0;", 
        strong("Columns: "), ncol(data))
    )
  })
  
  # Render the data table
  output$data_table <- DT::renderDataTable({
    DT::datatable(
      selected_dataset(),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = 'cell-border stripe',
      rownames = FALSE
    )
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$dataset_choice, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(selected_dataset(), file, row.names = FALSE)
    }
  )
  
  #BREAST CANCER TAB NAVIGATION
  observeEvent(input$bc_goto_state, {
    updateTabsetPanel(session, "bc_tabs", selected = "By State")
  })
  
  observeEvent(input$bc_goto_race, {
    updateTabsetPanel(session, "bc_tabs", selected = "By Race")
  })
  
  observeEvent(input$bc_goto_map, {
    updateTabsetPanel(session, "bc_tabs", selected = "Map")
  })
  
  #CERVICAL CANCER TAB NAVIGATION
  observeEvent(input$cc_goto_map, {
    updateTabsetPanel(session, "cc_tabs", selected = "Map")
  })
  
  observeEvent(input$cc_goto_race, {
    updateTabsetPanel(session, "cc_tabs", selected = "By Race")
  })
  
  observeEvent(input$cc_goto_state, {
    updateTabsetPanel(session, "cc_tabs", selected = "By State")
  })
  
  #STI TAB NAVIGATION
  observeEvent(input$sti_goto_map, {
    updateTabsetPanel(session, "sti_tabs", selected = "Map")
  })
  
  observeEvent(input$sti_goto_trends, {
    updateTabsetPanel(session, "sti_tabs", selected = "Trends")
  })
  
  observeEvent(input$sti_goto_race, {
    updateTabsetPanel(session, "sti_tabs", selected = "By Race")
  })
  
  # MATERNAL-INFANT TAB NAVIGATION
  observeEvent(input$mi_goto_map, {
    updateTabsetPanel(session, "mi_tabs", selected = "Map")
  })
  
  observeEvent(input$mi_goto_equity, {
    updateTabsetPanel(session, "mi_tabs", selected = "Health Equity")
  })
  
  observeEvent(input$mi_goto_distribution, {
    updateTabsetPanel(session, "mi_tabs", selected = "Distribution")
  })
  
  #INSIGHTS TAB NAVIGATION
  observeEvent(input$insights_goto_cancer, {
    updateTabsetPanel(session, "insights_tabs", selected = "Cancer & STI")
  })
  
  observeEvent(input$insights_goto_maternal, {
    updateTabsetPanel(session, "insights_tabs", selected = "Maternal Mortality")
  })
  
  observeEvent(input$insights_goto_infant, {
    updateTabsetPanel(session, "insights_tabs", selected = "Infant Mortality")
  })
  
  #RESOURCES TAB NAVIGATION
  observeEvent(input$resources_goto_cancer, {
    updateTabsetPanel(session, "resources_tabs", selected = "Cancer Screening")
  })
  
  observeEvent(input$resources_goto_sti, {
    updateTabsetPanel(session, "resources_tabs", selected = "STI Testing")
  })
  
  observeEvent(input$resources_goto_maternal, {
    updateTabsetPanel(session, "resources_tabs", selected = "Maternal Health")
  })
  
  #DATA TAB NAVIGATION 
  observeEvent(input$data_goto_browse, {
    updateTabsetPanel(session, "data_tabs", selected = "Browse Datasets")
  })
  
  
  
}