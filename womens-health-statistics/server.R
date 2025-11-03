library(shiny)
library(leaflet)
library(dplyr)
library(maps)
library(htmltools)
library(sf)
library(ggplot2)

states <- read_sf("us-states.geojson")
breast_cancer_long <- read.csv("breast_cancer_long.csv") 
cervical_cancer_long <- read.csv("cervical_cancer_long.csv") 
sex_infect_years <- read.csv("sex_infect_years.csv")
syphilis <- read.csv("syphilis_long.csv")
chlamydia <- read.csv("chlamydia_long.csv") 
gonorrhea <- read.csv("gonorrhea_long.csv")


server <- function(input, output, session) {
  
 # Breast Cancer Map 
  
  # Update race filter choices
  updateSelectInput(session,
                    "race_filter",
                    choices = c("All", unique(breast_cancer_long$race)))  
  
  # REACTIVE DATA - This recalculates automatically when race_filter changes
  breast_cancer_filtered_race <- reactive({
    result <- breast_cancer_long  
    
    if (input$race_filter != "All") {
      result <- result %>%
        filter(race == input$race_filter)
    }
    result
  })
  
  # REACTIVE MAP DATA - Aggregate data before joining
  map_data <- reactive({
    # First, aggregate the filtered data by state
    aggregated <- breast_cancer_filtered_race() %>%
      group_by(state) %>%
      summarise(breast_cancer = mean(breast_cancer, na.rm = TRUE), .groups = 'drop')
    
    # Then join with states
    states %>%
      left_join(aggregated, by = c("name" = "state"))
  })
  
  # RENDER THE MAP
  output$cancer_map <- renderLeaflet({
    data <- map_data()
    
    # Make sure breast_cancer is numeric, not a list
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
  
  # Update cervical race filter choices
  updateSelectInput(session,
                    "cervical_race_filter",
                    choices = c("All", unique(cervical_cancer_long$race)))
  
  # CERVICAL REACTIVE DATA - This recalculates automatically when cervical_race_filter changes
  cervical_cancer_filtered_race <- reactive({
    result <- cervical_cancer_long  
    
    if (input$cervical_race_filter != "All") {
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
  # Update race filter choices for STI map
  updateSelectInput(session,
                    "sti_race_filter",
                    choices = c("All", unique(syphilis$race)))
  
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
    if (input$sti_race_filter != "All") {
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
  
  
  
  
  
  
  
  
  
 #Over Time Plots
    
    # Fill in the spot we created for a plot
    output$diseasePlot <- renderPlot({
      
      # Filter data based on selected state, disease, and race
      filtered_data <- sex_infect_years[sex_infect_years$state == input$state & 
                                          sex_infect_years$disease == input$disease & 
                                          sex_infect_years$race == input$race, ]
      # Check if we have data
      if(nrow(filtered_data) == 0) {
        plot(1, type="n", xlim=c(0, 10), ylim=c(0, 10), 
             xlab="", ylab="", main="No data available for this combination")
        text(5, 5, "No data available\nfor the selected filters", cex=1.5, col="red")
        return() }
      
      # Create a named vector for barplot (years as names, rates as values)
      plot_data <- filtered_data$rate
      names(plot_data) <- filtered_data$year
      
      # Render a barplot
      barplot(plot_data, 
              main=paste(input$state, "-", input$disease, "-", input$race),
              ylab="Rate per 100,000",
              xlab="Year",
              col="steelblue")
    })
  
    
}