library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(maps)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)

# Load data
breast_cancer_long <- read.csv("breast_cancer_long.csv")
cervical_cancer_long <- read.csv("cervical_cancer_long.csv")
sex_infect_years <- read.csv("sex_infect_years.csv")
syphilis <- read.csv("syphilis_long.csv")
chlamydia <- read.csv("chlamydia_long.csv")
gonorrhea <- read.csv("gonorrhea_long.csv")
maternal_mortality <- read.csv("mortality_year_long.csv")
mortality_race_long <- read.csv("mortality_race_long.csv")
infant_mortality_long <- read.csv("infant_mortality_long.csv")

breast_rate_clean <- read.csv("breast_rate_clean.csv")
cervical_rate_clean <- read.csv("cervical_rate_clean.csv")
syphilis_rate_clean <- read.csv("syphilis_rate_clean.csv")
gonorrhea_rate_clean <- read.csv("gonorrhea_rate_clean.csv")
chlamydia_rate_clean <- read.csv("chlamydia_rate_clean.csv")
mortality_race <- read.csv("mortality_race.csv")
infant_death_rate_clean <- read.csv("infant_death_rate_clean.csv")


navbarPage(
  title = "Women's Health in the United States",
  theme = bs_theme(
    bg = "#FFFFFF",
    fg = "#2C3E50",
    primary = "#2C3E50",
    base_font = font_google("Roboto")
  ),
  
  # Home Page Tab
  
  # Clean, Simple Home Page - No Gradients or Emojis
  tabPanel("Home",
           tags$head(
             tags$link(href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@600;700&family=Inter:wght@300;400;500;600&display=swap",
                       rel = "stylesheet")
           ),
           div(style = "background: #f8f9fa; min-height: 100vh; padding: 0;",
               
               # Simple solid header
               div(style = "background: #2C3E50;
                          padding: 80px 20px; text-align: center;",
                   h1("Women's Health",
                      style = "color: white; font-family: 'Playfair Display', serif;
                             font-weight: 700; font-size: 52px; margin: 0; letter-spacing: -0.5px;"),
                   p("Understanding health disparities across the United States",
                     style = "color: #E8F4F8; font-size: 20px; margin: 15px 0 0 0;
                            font-family: 'Inter', sans-serif;")
               ),
               
               # Main content
               div(style = "padding: 60px 20px; font-family: 'Inter', sans-serif;",
                   
                   # Mission section
                   fluidRow(style = "margin-bottom: 60px;",
                            column(12,
                                   div(style = "background: white; padding: 50px;
                                              border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                       fluidRow(
                                         column(6,
                                                h2("About This Project",
                                                   style = "color: #2C3E50; margin-top: 0; font-weight: 600;
                                                          font-family: 'Playfair Display', serif; font-size: 36px;
                                                          margin-bottom: 25px;"),
                                                p("Women's health disparities and outcomes have been severely overlooked
                                                throughout medical research history. This oversight leads to disproportionate
                                                access to healthcare and medical interventions, especially for marginalized populations.",
                                                  style = "font-size: 17px; line-height: 1.9; color: #4a5568;
                                                         font-weight: 400; margin-bottom: 20px;"),
                                                p("Our interactive dashboard highlights data from the CDC WONDER Online Database,
                                                grouped by state and race, to assess health outcomes for women across the United States.",
                                                  style = "font-size: 17px; line-height: 1.9; color: #4a5568; font-weight: 400;")
                                         ),
                                         column(6,
                                                img(src = "purple-hands.png",
                                                    style = "width: 100%; border-radius: 8px;")
                                         )
                                       )
                                   )
                            )
                   ),
                   
                   # Features section
                   fluidRow(style = "margin-bottom: 60px;",
                            column(4,
                                   div(style = "background: white; padding: 40px;
                                              border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);
                                              border-left: 4px solid #2C3E50; height: 100%;",
                                       h3("Interactive Maps",
                                          style = "color: #2C3E50; font-family: 'Playfair Display', serif;
                                                 font-size: 24px; margin-top: 0; margin-bottom: 15px;"),
                                       p("Explore maternal mortality and cancer incidence rates by state and race with
                                       dynamic visualizations.",
                                         style = "font-size: 16px; line-height: 1.8; color: #5a6c7d;")
                                   )
                            ),
                            column(4,
                                   div(style = "background: white; padding: 40px;
                                              border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);
                                              border-left: 4px solid #E8F4F8; height: 100%;",
                                       h3("Disparity Analysis",
                                          style = "color: #2C3E50; font-family: 'Playfair Display', serif;
                                                 font-size: 24px; margin-top: 0; margin-bottom: 15px;"),
                                       p("Compare health outcomes across different racial and ethnic populations to
                                       identify critical gaps in care.",
                                         style = "font-size: 16px; line-height: 1.8; color: #5a6c7d;")
                                   )
                            ),
                            column(4,
                                   div(style = "background: white; padding: 40px;
                                              border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);
                                              border-left: 4px solid #2C3E50; height: 100%;",
                                       h3("Evidence-Based Insights",
                                          style = "color: #2C3E50; font-family: 'Playfair Display', serif;
                                                 font-size: 24px; margin-top: 0; margin-bottom: 15px;"),
                                       p("Data-driven findings to inform policy decisions and target healthcare interventions
                                       where needed most.",
                                         style = "font-size: 16px; line-height: 1.8; color: #5a6c7d;")
                                   )
                            )
                   ),
                   
                   # How to use section
                   fluidRow(style = "margin-bottom: 60px;",
                            column(12,
                                   div(style = "background: #2C3E50; padding: 50px; border-radius: 8px;",
                                       h2("How to Use This Dashboard",
                                          style = "color: white; font-family: 'Playfair Display', serif;
                                                 font-size: 36px; margin-top: 0; margin-bottom: 40px; text-align: center;"),
                                       fluidRow(
                                         column(4,
                                                div(style = "padding: 20px; text-align: center;",
                                                    div(style = "width: 60px; height: 60px; background: #E8F4F8;
                                                               border-radius: 50%; margin: 0 auto 20px auto;
                                                               display: flex; align-items: center; justify-content: center;",
                                                        span("1", style = "font-size: 28px; font-weight: 700; color: #2C3E50;")
                                                    ),
                                                    h4("Select a Topic",
                                                       style = "color: white; font-size: 20px; margin-bottom: 15px;"),
                                                    p("Navigate using the tabs to explore different health conditions",
                                                      style = "color: #E8F4F8; font-size: 15px; line-height: 1.7;")
                                                )
                                         ),
                                         column(4,
                                                div(style = "padding: 20px; text-align: center;",
                                                    div(style = "width: 60px; height: 60px; background: #E8F4F8;
                                                               border-radius: 50%; margin: 0 auto 20px auto;
                                                               display: flex; align-items: center; justify-content: center;",
                                                        span("2", style = "font-size: 28px; font-weight: 700; color: #2C3E50;")
                                                    ),
                                                    h4("Apply Filters",
                                                       style = "color: white; font-size: 20px; margin-bottom: 15px;"),
                                                    p("Use dropdown menus to focus on specific states or demographics",
                                                      style = "color: #E8F4F8; font-size: 15px; line-height: 1.7;")
                                                )
                                         ),
                                         column(4,
                                                div(style = "padding: 20px; text-align: center;",
                                                    div(style = "width: 60px; height: 60px; background: #E8F4F8;
                                                               border-radius: 50%; margin: 0 auto 20px auto;
                                                               display: flex; align-items: center; justify-content: center;",
                                                        span("3", style = "font-size: 28px; font-weight: 700; color: #2C3E50;")
                                                    ),
                                                    h4("Explore Insights",
                                                       style = "color: white; font-size: 20px; margin-bottom: 15px;"),
                                                    p("Interact with visualizations to discover patterns and disparities",
                                                      style = "color: #E8F4F8; font-size: 15px; line-height: 1.7;")
                                                )
                                         )
                                       )
                                   )
                            )
                   ),
                   
                   # Image and about data section
                   fluidRow(style = "margin-bottom: 60px;",
                            column(12,
                                   div(style = "background: white; padding: 50px;
                                              border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                       fluidRow(
                                         column(6, style = "display: flex; justify-content: center; align-items: center;",
                                                img(src = "https://www.nysut.org/-/media/images/nysut/news/2022/july/banner_220706_womenshealth_01.jpg",
                                                    style = "width: 100%; border-radius: 8px;")
                                         ),
                                         column(6,
                                                h2("About This Data",
                                                   style = "color: #2C3E50; margin-top: 0; font-weight: 600;
                                                          font-family: 'Playfair Display', serif; font-size: 36px;
                                                          margin-bottom: 25px;"),
                                                p(strong("Data Sources: "),
                                                  "All datasets were retrieved from KFF (originally Kaiser Family Foundation),
                                                a public charity and national nonprofit that serves to provide data and research
                                                on health policy and health disparities across the United States. KFF currently
                                                researches Women's Health policy through their Policy Analysis sector.",
                                                  style = "font-size: 16px; line-height: 1.9; color: #4a5568; margin-bottom: 20px;"),
                                                p("Datasets on women's health disparities used data from the WONDER Online Database
                                                from the United States Department of Health and Human Services, Centers for Disease
                                                Control and Prevention and National Cancer Institute (CDC WONDER Online Database).",
                                                  style = "font-size: 16px; line-height: 1.9; color: #4a5568; margin-bottom: 20px;"),
                                                p("All data after 1989 meets the National Center for Health Statistics data use
                                                restrictions, where missing values are due to state incidence data not meeting
                                                publication criteria. Suppressed values replace incidence rate, death counts,
                                                death rates and associated confidence intervals and standard errors, as well
                                                as corresponding population figures, when the figure represents one to nine
                                                (1-9) persons for deaths 1999 and after.",
                                                  style = "font-size: 15px; line-height: 1.9; color: #4a5568; margin-bottom: 20px;"),
                                                p(a(href = "https://www.kff.org/about-us/", "About Us, retrieved from https://www.kff.org/about-us/",
                                                    style = "color: #2C3E50; font-size: 16px; font-weight: 600; text-decoration: underline;"),
                                                  style = "margin: 0;")
                                         )
                                       )
                                   )
                            )
                   )
               )
           )
  ),
  
  navbarMenu("Cancer",
             tabPanel("Breast Cancer",
                      tags$head(
                        tags$style(HTML("
             /* Tab styling */
             .nav-tabs {
               background: white;
               border-bottom: 2px solid #ec4899;
               padding: 0;
               margin: 0;
             }
             .nav-tabs > li > a {
               color: #6b7280;
               font-weight: 600;
               padding: 16px 24px;
               border: none;
               border-bottom: 2px solid transparent;
               margin-bottom: -2px;
               transition: all 0.3s ease;
             }
             .nav-tabs > li > a:hover {
               background-color: #fdf2f8;
               color: #2C3E50;
               border: none;
               border-bottom: 2px solid #ec4899;
             }
             .nav-tabs > li.active > a,
             .nav-tabs > li.active > a:hover,
             .nav-tabs > li.active > a:focus {
               color: #ec4899;
               background-color: #fdf2f8;
               border: none;
               border-bottom: 2px solid #ec4899;
             }
             .tab-content {
               padding: 30px 20px;
               background: #fefcfd;
             }
             /* Header styling */
             .bc-header {
               background: white;
               padding: 40px 20px;
               text-align: center;
               border-bottom: 1px solid #fce7f3;
               margin-bottom: 0;
             }
             .bc-header h1 {
               color: #2C3E50;
               font-weight: 700;
               font-size: 36px;
               margin: 0 0 10px 0;
             }
             .bc-header p {
               color: #6b7280;
               font-size: 18px;
               margin: 0;
             }
             /* Card styling */
             .insight-card {
               background: #fdf2f8;
               border-left: 4px solid #ec4899;
               padding: 20px;
               border-radius: 8px;
               margin-bottom: 20px;
             }
             .stat-card {
               background: white;
               border: 1px solid #fce7f3;
               border-radius: 12px;
               padding: 24px;
               text-align: center;
               box-shadow: 0 2px 4px rgba(0,0,0,0.05);
             }
             .stat-value {
               font-size: 48px;
               font-weight: 700;
               color: #ec4899;
               margin: 10px 0;
             }
             .stat-label {
               color: #6b7280;
               font-size: 14px;
               font-weight: 600;
               text-transform: uppercase;
               letter-spacing: 0.05em;
             }
           "))
                      ),
                      
                      # Header
                      div(class = "bc-header",
                          h1("Breast Cancer Incidence Rates"),
                          p("Interactive exploration of geographic and demographic patterns")
                      ),
                      
                      # Tabset
                      tabsetPanel(
                        id = "bc_tabs",
                        type = "tabs",
                        
                        # OVERVIEW TAB
                        tabPanel("Overview",
                                 div(style = "max-width: 1200px; margin: 0 auto; padding: 20px;",
                                     
                                     # Key Stats Row
                                     fluidRow(
                                       column(4,
                                              div(class = "stat-card",
                                                  div(class = "stat-label", "National Average"),
                                                  div(class = "stat-value", "133.8"),
                                                  div(style = "color: #6b7280; font-size: 14px;", 
                                                      "cases per 100,000 women")
                                              )
                                       ),
                                       column(4,
                                              div(class = "stat-card",
                                                  div(class = "stat-label", "Highest Rate State"),
                                                  div(class = "stat-value", style = "font-size: 32px;", "Connecticut"),
                                                  div(style = "color: #6b7280; font-size: 14px;", 
                                                      "157.9 per 100,000")
                                              )
                                       ),
                                       column(4,
                                              div(class = "stat-card",
                                                  div(class = "stat-label", "Highest Risk Group"),
                                                  div(class = "stat-value", style = "font-size: 32px;", "White Women"),
                                                  div(style = "color: #6b7280; font-size: 14px;", 
                                                      "139.2 per 100,000")
                                              )
                                       )
                                     ),
                                     
                                     # About Section
                                     div(style = "margin-top: 40px;",
                                         div(style = "background: white; padding: 30px; border-radius: 12px; 
                                box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
                                             h2("About This Data", 
                                                style = "color: #2C3E50; margin-top: 0; font-size: 28px; margin-bottom: 20px;"),
                                             p("Breast cancer is the most commonly diagnosed cancer in the United States and a leading 
                                  cause of cancer-related deaths. While advances in screening, early detection, and treatment 
                                  have improved survival rates, incidence and outcomes continue to vary widely across different 
                                  populations and geographic regions.",
                                               style = "color: #4b5563; font-size: 16px; line-height: 1.8; margin-bottom: 16px;"),
                                             p("This page provides an overview of breast cancer incidence rates by state and race, highlighting 
                                  the patterns that reveal disparities in health outcomes. By analyzing this data, we can better 
                                  understand how factors such as access to healthcare, socioeconomic conditions, genetics, and 
                                  public health initiatives contribute to differences in breast cancer risk and detection across 
                                  the United States.",
                                               style = "color: #4b5563; font-size: 16px; line-height: 1.8;")
                                         )
                                     ),
                                     
                                     # Key Insights
                                     div(style = "margin-top: 30px;",
                                         h3("Key Insights", style = "color: #2C3E50; font-size: 24px; margin-bottom: 20px;"),
                                         fluidRow(
                                           column(4,
                                                  div(class = "insight-card",
                                                      h4("Geographic Patterns", 
                                                         style = "color: #2C3E50; margin-top: 0; font-size: 18px;"),
                                                      p("Northeastern states show consistently higher incidence rates, 
                                           suggesting regional factors at play.",
                                                        style = "color: #6b7280; margin: 0; font-size: 15px; line-height: 1.6;")
                                                  )
                                           ),
                                           column(4,
                                                  div(class = "insight-card",
                                                      h4("Racial Disparities", 
                                                         style = "color: #2C3E50; margin-top: 0; font-size: 18px;"),
                                                      p("While White women have highest overall rates, Black women face 
                                           more aggressive disease at younger ages.",
                                                        style = "color: #6b7280; margin: 0; font-size: 15px; line-height: 1.6;")
                                                  )
                                           ),
                                           column(4,
                                                  div(class = "insight-card",
                                                      h4("Intervention Priority", 
                                                         style = "color: #2C3E50; margin-top: 0; font-size: 18px;"),
                                                      p("High-rate states benefit most from enhanced screening programs 
                                           and targeted prevention efforts.",
                                                        style = "color: #6b7280; margin: 0; font-size: 15px; line-height: 1.6;")
                                                  )
                                           )
                                         )
                                     ),
                                     
                                     # Explore Data CTA
                                     div(style = "margin-top: 40px; background: linear-gradient(135deg, #ec4899 0%, #f43f5e 100%); 
                            padding: 40px; border-radius: 12px; text-align: center;",
                                         h3("Explore the Data", 
                                            style = "color: white; margin: 0 0 16px 0; font-size: 28px;"),
                                         p("Use the tabs above to dive deeper into state rankings, racial disparities, and geographic patterns.",
                                           style = "color: #fce7f3; font-size: 16px; margin-bottom: 24px;"),
                                         div(style = "display: flex; justify-content: center; gap: 16px; flex-wrap: wrap;",
                                             tags$button("View By State", 
                                                         onclick = "$('#bc_tabs a[href=\"#shiny-tab-by_state\"]').tab('show');",
                                                         style = "background: white; color: #ec4899; border: none; 
                                           padding: 12px 32px; border-radius: 8px; font-weight: 600; 
                                           cursor: pointer; font-size: 16px;"),
                                             tags$button("View By Race",
                                                         onclick = "$('#bc_tabs a[href=\"#shiny-tab-by_race\"]').tab('show');",
                                                         style = "background: rgba(255,255,255,0.2); color: white; border: 2px solid white; 
                                           padding: 12px 32px; border-radius: 8px; font-weight: 600; 
                                           cursor: pointer; font-size: 16px;"),
                                             tags$button("View Map",
                                                         onclick = "$('#bc_tabs a[href=\"#shiny-tab-map\"]').tab('show');",
                                                         style = "background: rgba(255,255,255,0.2); color: white; border: 2px solid white; 
                                           padding: 12px 32px; border-radius: 8px; font-weight: 600; 
                                           cursor: pointer; font-size: 16px;")
                                         )
                                     )
                                 )
                        ),
                        
                        # BY STATE TAB
                        tabPanel("By State",
                                 div(style = "max-width: 1200px; margin: 0 auto;",
                                     div(style = "background: white; padding: 30px; border-radius: 12px; 
                            box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
                                         
                                         h2("States with Highest Breast Cancer Incidence",
                                            style = "color: #2C3E50; margin-top: 0; margin-bottom: 8px;"),
                                         p("Identifying high-risk regions for targeted intervention and resource allocation",
                                           style = "color: #6b7280; font-size: 16px; margin-bottom: 24px;"),
                                         
                                         fluidRow(
                                           column(3,
                                                  div(style = "background: #fdf2f8; padding: 20px; border-radius: 8px;",
                                                      sliderInput("top_n",
                                                                  "Number of states:",
                                                                  min = 5,
                                                                  max = 20,
                                                                  value = 10,
                                                                  step = 1),
                                                      
                                                      selectInput("rank_race",
                                                                  "Select Race:",
                                                                  choices = unique(breast_cancer_long$race),
                                                                  selected = "Overall"),
                                                      
                                                      hr(style = "border-color: #fce7f3;"),
                                                      
                                                      div(style = "background: white; padding: 15px; border-radius: 6px;",

                                                          h4("Insight", style = "color: #ec4899; margin-top: 0; font-size: 16px;"),

                                                          p("Connecticut leads with 157.9 cases per 100,000 women. Adjust the slider 
                                               to explore more states and filter by race to identify disparities.",
                                                            style = "color: #6b7280; margin: 0; font-size: 14px; line-height: 1.6;")
                                                      )
                                                  )
                                           ),
                                           column(9,
                                                  plotOutput("top_states_plot", height = "600px")
                                           )
                                         )
                                     ),
                                     
                                     # Why This Matters
                                     div(style = "margin-top: 24px; background: #fef2f2; border-left: 4px solid #f43f5e; 
                            padding: 20px; border-radius: 8px;",
                                         h4("Why This Matters", style = "color: #2C3E50; margin-top: 0;"),
                                         p("Geographic clusters help public health officials prioritize resources, implement screening 
                              initiatives, and investigate potential environmental or lifestyle factors contributing to 
                              elevated rates.",
                                           style = "color: #6b7280; margin: 0; line-height: 1.6;")
                                     )
                                 )
                        ),
                        
                        # BY RACE TAB
                        tabPanel("By Race",
                                 div(style = "max-width: 1200px; margin: 0 auto;",
                                     div(style = "background: white; padding: 30px; border-radius: 12px; 
                            box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
                                         
                                         h2("Breast Cancer Incidence by Race and Ethnicity",
                                            style = "color: #2C3E50; margin-top: 0; margin-bottom: 8px;"),
                                         p("Disparities across demographic groups reveal inequities in screening and outcomes",
                                           style = "color: #6b7280; font-size: 16px; margin-bottom: 24px;"),
                                         
                                         fluidRow(
                                           column(3,
                                                  div(style = "background: #fdf2f8; padding: 20px; border-radius: 8px;",
                                                      selectInput("state",
                                                                  "Choose a state:",
                                                                  choices = unique(breast_cancer_long$state),
                                                                  selected = "United States"),
                                                      
                                                      hr(style = "border-color: #fce7f3;"),
                                                      
                                                      div(style = "background: white; padding: 15px; border-radius: 6px;",
                                                          h4("Understanding Disparities", 
                                                             style = "color: #ec4899; margin-top: 0; font-size: 16px;"),
                                                          p("Select a state to compare incidence rates across racial groups. 
                                               The 'Overall' category shows the combined rate for all groups.",
                                                            style = "color: #6b7280; margin: 0; font-size: 14px; line-height: 1.6;")
                                                      )
                                                  )
                                           ),
                                           column(9,
                                                  plotOutput("breast_race_plot", height = "500px")
                                           )
                                         )
                                     ),
                                     
                                     # Key Finding
                                     div(style = "margin-top: 24px;",
                                         fluidRow(
                                           column(6,
                                                  div(style = "background: #fef3c7; border-left: 4px solid #f59e0b; 
                                         padding: 20px; border-radius: 8px; height: 100%;",
                                                      h4("Highest Incidence", style = "color: #2C3E50; margin-top: 0;"),
                                                      p("White women have the highest overall incidence at 139.2 per 100,000, 
                                           though rates vary by age and subtype.",
                                                        style = "color: #78350f; margin: 0; line-height: 1.6;")
                                                  )
                                           ),
                                           column(6,
                                                  div(style = "background: #fef2f2; border-left: 4px solid #ef4444; 
                                         padding: 20px; border-radius: 8px; height: 100%;",
                                                      h4("Mortality Disparity", style = "color: #2C3E50; margin-top: 0;"),
                                                      p("Black women face higher mortality despite lower incidence, diagnosed at 
                                           younger ages with more aggressive disease.",
                                                        style = "color: #7f1d1d; margin: 0; line-height: 1.6;")
                                                  )
                                           )
                                         )
                                     )
                                 )
                        ),
                        
                        # MAP TAB
                        tabPanel("Map",
                                 div(style = "max-width: 1400px; margin: 0 auto;",
                                     div(style = "background: white; padding: 30px; border-radius: 12px; 
                            box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
                                         
                                         h2("Geographic Distribution of Breast Cancer",
                                            style = "color: #2C3E50; margin-top: 0; margin-bottom: 8px;"),
                                         p("Interactive map showing breast cancer incidence rates across all 50 states",
                                           style = "color: #6b7280; font-size: 16px; margin-bottom: 24px;"),
                                         
                                         fluidRow(
                                           column(3,
                                                  div(style = "background: #fdf2f8; padding: 20px; border-radius: 8px;",
                                                      selectInput("race_filter",
                                                                  "Select Race/Ethnicity:",
                                                                  choices = c("Overall"),
                                                                  selected = "Overall"),
                                                      
                                                      hr(style = "border-color: #fce7f3;"),
                                                      
                                                      div(style = "background: white; padding: 15px; border-radius: 6px;",
                                                          h4("Map Guide", style = "color: #ec4899; margin-top: 0; font-size: 16px;"),
                                                          tags$ul(
                                                            style = "color: #6b7280; line-height: 1.8; padding-left: 20px; margin: 0;",
                                                            tags$li("Hover over states for exact rates"),
                                                            tags$li("Change race filter to update map"),
                                                            tags$li("Darker colors = higher rates")
                                                          )
                                                      )
                                                  )
                                           ),
                                           column(9,
                                                  leafletOutput("cancer_map", height = "600px")
                                           )
                                         )
                                     ),
                                     
                                     # Geographic Patterns
                                     div(style = "margin-top: 24px; background: linear-gradient(135deg, #ec4899 0%, #f43f5e 100%); 
                            padding: 24px; border-radius: 8px;",
                                         h4("Geographic Patterns", style = "color: white; margin-top: 0;"),
                                         p("Northeastern states consistently show higher breast cancer incidence rates. These patterns 
                              help public health officials prioritize resources and investigate potential environmental or 
                              lifestyle factors.",
                                           style = "color: #fce7f3; margin: 0; line-height: 1.6;")
                                     )
                                 )
                        )
                      )
             ),
             
             tabPanel("Cervical Cancer",
                      tags$head(
                        tags$style(HTML("
             /* Tab styling */
             .nav-tabs {
               background: white;
               border-bottom: 2px solid #00838F;
               padding: 0;
               margin: 0;
             }
             .nav-tabs > li > a {
               color: #6b7280;
               font-weight: 600;
               padding: 16px 24px;
               border: none;
               border-bottom: 2px solid transparent;
               margin-bottom: -2px;
               transition: all 0.3s ease;
             }
             .nav-tabs > li > a:hover {
               background-color: #E0F7FA;
               color: #2C3E50;
               border: none;
               border-bottom: 2px solid #00838F;
             }
             .nav-tabs > li.active > a,
             .nav-tabs > li.active > a:hover,
             .nav-tabs > li.active > a:focus {
               color: #00838F;
               background-color: #E0F7FA;
               border: none;
               border-bottom: 2px solid #00838F;
             }
             .tab-content {
               padding: 30px 20px;
               background: #E0F7FA;
             }
             /* Header styling */
             .cc-header {
               background: white;
               padding: 40px 20px;
               text-align: center;
               border-bottom: 1px solid #80DEEA;
               margin-bottom: 0;
             }
             .cc-header h1 {
               color: #2C3E50;
               font-weight: 700;
               font-size: 36px;
               margin: 0 0 10px 0;
             }
             .cc-header p {
               color: #555;
               font-size: 18px;
               margin: 0;
             }
             /* Card styling */
             .cc-insight-card {
               background: #E0F7FA;
               border-left: 4px solid #00838F;
               padding: 20px;
               border-radius: 8px;
               margin-bottom: 20px;
             }
             .cc-stat-card {
               background: white;
               border: 1px solid #80DEEA;
               border-radius: 12px;
               padding: 24px;
               text-align: center;
               box-shadow: 0 2px 4px rgba(0,0,0,0.05);
             }
             .cc-stat-value {
               font-size: 48px;
               font-weight: 700;
               color: #00838F;
               margin: 10px 0;
             }
             .cc-stat-label {
               color: #6b7280;
               font-size: 14px;
               font-weight: 600;
               text-transform: uppercase;
               letter-spacing: 0.05em;
             }
           "))
                      ),
                      
                      # Header
                      div(class = "cc-header",
                          h1("Cervical Cancer Incidence Rates"),
                          p("Understanding cervical cancer disparities across the United States")
                      ),
                      
                      # Tabset
                      tabsetPanel(
                        id = "cc_tabs",
                        type = "tabs",
                        
                        # OVERVIEW TAB
                        tabPanel("Overview",
                                 div(style = "max-width: 1200px; margin: 0 auto; padding: 20px;",
                                     
                                     # Key Stats Row
                                     fluidRow(
                                       column(4,
                                              div(class = "cc-stat-card",
                                                  div(class = "cc-stat-label", "Most Affected Group"),
                                                  div(class = "cc-stat-value", style = "font-size: 32px;", "Hispanic Women"),
                                                  div(style = "color: #6b7280; font-size: 14px;", 
                                                      "Highest incidence rates")
                                              )
                                       ),
                                       column(4,
                                              div(class = "cc-stat-card",
                                                  div(class = "cc-stat-label", "Preventable"),
                                                  div(class = "cc-stat-value", "90%+"),
                                                  div(style = "color: #6b7280; font-size: 14px;", 
                                                      "with HPV vaccination")
                                              )
                                       ),
                                       column(4,
                                              div(class = "cc-stat-card",
                                                  div(class = "cc-stat-label", "Screening Saves Lives"),
                                                  div(class = "cc-stat-value", style = "font-size: 32px;", "Pap Tests"),
                                                  div(style = "color: #6b7280; font-size: 14px;", 
                                                      "Detect early changes")
                                              )
                                       )
                                     ),
                                     
                                     # About Section
                                     div(style = "margin-top: 40px;",
                                         div(style = "background: white; padding: 30px; border-radius: 12px; 
                                   box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
                                             h2("About This Data", 
                                                style = "color: #2C3E50; margin-top: 0; font-size: 28px; margin-bottom: 20px;"),
                                             p("Cervical cancer is one of the most preventable cancers, yet it remains a significant 
                                  health concern, particularly for certain populations. Regular screening through Pap tests 
                                  and HPV testing can detect precancerous changes early, when treatment is most effective.",
                                               style = "color: #4b5563; font-size: 16px; line-height: 1.8; margin-bottom: 16px;"),
                                             p("This page provides an overview of cervical cancer incidence rates by state and race, 
                                  revealing important disparities in screening access and health outcomes. Understanding 
                                  these patterns helps target prevention efforts and improve access to life-saving screenings 
                                  in underserved communities.",
                                               style = "color: #4b5563; font-size: 16px; line-height: 1.8;")
                                         )
                                     ),
                                     
                                     # Key Insights
                                     div(style = "margin-top: 30px;",
                                         h3("Key Insights", style = "color: #2C3E50; font-size: 24px; margin-bottom: 20px;"),
                                         fluidRow(
                                           column(4,
                                                  div(class = "cc-insight-card",
                                                      h4("Racial Disparities", 
                                                         style = "color: #2C3E50; margin-top: 0; font-size: 18px;"),
                                                      p("Hispanic women have the highest cervical cancer incidence rates, 
                                           highlighting screening access gaps.",
                                                        style = "color: #555; margin: 0; font-size: 15px; line-height: 1.6;")
                                                  )
                                           ),
                                           column(4,
                                                  div(class = "cc-insight-card",
                                                      h4("Prevention Works", 
                                                         style = "color: #2C3E50; margin-top: 0; font-size: 18px;"),
                                                      p("HPV vaccination and regular Pap tests can prevent most cervical cancers 
                                           when administered appropriately.",
                                                        style = "color: #555; margin: 0; font-size: 15px; line-height: 1.6;")
                                                  )
                                           ),
                                           column(4,
                                                  div(class = "cc-insight-card",
                                                      h4("Geographic Variation", 
                                                         style = "color: #2C3E50; margin-top: 0; font-size: 18px;"),
                                                      p("Rates vary significantly by state, reflecting differences in screening 
                                           programs and healthcare access.",
                                                        style = "color: #555; margin: 0; font-size: 15px; line-height: 1.6;")
                                                  )
                                           )
                                         )
                                     ),
                                     
                                     # Explore Data CTA
                                     div(style = "margin-top: 40px; background: linear-gradient(135deg, #00838F 0%, #00ACC1 100%); 
                                   padding: 40px; border-radius: 12px; text-align: center;",
                                         h3("Explore the Data", 
                                            style = "color: white; margin: 0 0 16px 0; font-size: 28px;"),
                                         p("Use the tabs above to dive deeper into state rankings, racial disparities, and geographic patterns.",
                                           style = "color: #E0F7FA; font-size: 16px; margin-bottom: 24px;"),
                                         div(style = "display: flex; justify-content: center; gap: 16px; flex-wrap: wrap;",
                                             tags$button("View Map", 
                                                         onclick = "$('#cc_tabs a[href=\"#shiny-tab-map\"]').tab('show');",
                                                         style = "background: white; color: #00838F; border: none; 
                                                   padding: 12px 32px; border-radius: 8px; font-weight: 600; 
                                                   cursor: pointer; font-size: 16px;"),
                                             tags$button("View By Race",
                                                         onclick = "$('#cc_tabs a[href=\"#shiny-tab-by_race\"]').tab('show');",
                                                         style = "background: rgba(255,255,255,0.2); color: white; border: 2px solid white; 
                                                   padding: 12px 32px; border-radius: 8px; font-weight: 600; 
                                                   cursor: pointer; font-size: 16px;"),
                                             tags$button("View Rankings",
                                                         onclick = "$('#cc_tabs a[href=\"#shiny-tab-by_state\"]').tab('show');",
                                                         style = "background: rgba(255,255,255,0.2); color: white; border: 2px solid white; 
                                                   padding: 12px 32px; border-radius: 8px; font-weight: 600; 
                                                   cursor: pointer; font-size: 16px;")
                                         )
                                     )
                                 )
                        ),
                        
                        # MAP TAB
                        tabPanel("Map",
                                 div(style = "max-width: 1400px; margin: 0 auto;",
                                     div(style = "background: white; padding: 30px; border-radius: 12px; 
                                   box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
                                         
                                         h2("Cervical Cancer Rates by State and Race",
                                            style = "color: #2C3E50; margin-top: 0; margin-bottom: 8px;"),
                                         p("Explore cervical cancer incidence rates across the United States. Use the dropdown 
                              to filter by race/ethnicity and see how rates vary geographically.",
                                           style = "color: #555; font-size: 16px; margin-bottom: 24px;"),
                                         
                                         fluidRow(
                                           column(3,
                                                  div(style = "background: #E0F7FA; padding: 20px; border-radius: 8px;",
                                                      selectInput("cervical_race_filter",
                                                                  "Select Race/Ethnicity:",
                                                                  choices = c("Overall"),
                                                                  selected = "Overall"),
                                                      
                                                      hr(style = "border-color: #80DEEA;"),
                                                      
                                                      div(style = "background: white; padding: 15px; border-radius: 6px;",
                                                          h4("Map Guide", style = "color: #00838F; margin-top: 0; font-size: 16px;"),
                                                          tags$ul(
                                                            style = "color: #555; line-height: 1.8; padding-left: 20px; margin: 0;",
                                                            tags$li("Hover over states for exact rates"),
                                                            tags$li("Change race filter to update map"),
                                                            tags$li("Darker colors = higher rates")
                                                          )
                                                      )
                                                  )
                                           ),
                                           column(9,
                                                  leafletOutput("cervical_map", height = "600px")
                                           )
                                         )
                                     ),
                                     
                                     # Geographic Patterns
                                     div(style = "margin-top: 24px; background: linear-gradient(135deg, #00838F 0%, #00ACC1 100%); 
                                   padding: 24px; border-radius: 8px;",
                                         h4("Geographic Patterns", style = "color: white; margin-top: 0;"),
                                         p("Cervical cancer rates vary across states, reflecting differences in screening access, 
                              HPV vaccination rates, and healthcare infrastructure. Identifying high-rate areas helps 
                              prioritize prevention programs.",
                                           style = "color: #E0F7FA; margin: 0; line-height: 1.6;")
                                     )
                                 )
                        ),
                        
                        # BY RACE TAB
                        tabPanel("By Race",
                                 div(style = "max-width: 1200px; margin: 0 auto;",
                                     div(style = "background: white; padding: 30px; border-radius: 12px; 
                                   box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
                                         
                                         h2("Cervical Cancer Incidence by Race",
                                            style = "color: #2C3E50; margin-top: 0; margin-bottom: 8px;"),
                                         p("Disparities across demographic groups reveal inequities in screening and prevention access",
                                           style = "color: #555; font-size: 16px; margin-bottom: 24px;"),
                                         
                                         fluidRow(
                                           column(3,
                                                  div(style = "background: #E0F7FA; padding: 20px; border-radius: 8px;",
                                                      selectInput("cervical_state",
                                                                  "Choose a state:",
                                                                  choices = unique(cervical_cancer_long$state),
                                                                  selected = "United States"),
                                                      
                                                      hr(style = "border-color: #80DEEA;"),
                                                      
                                                      div(style = "background: white; padding: 15px; border-radius: 6px;",
                                                          h4("Understanding Disparities", 
                                                             style = "color: #00838F; margin-top: 0; font-size: 16px;"),
                                                          p("This chart shows cervical cancer incidence rates by race/ethnicity 
                                               for your selected state.",
                                                            style = "color: #555; margin: 0; font-size: 14px; line-height: 1.6;")
                                                      )
                                                  )
                                           ),
                                           column(9,
                                                  plotOutput("cervical_race_plot", height = "500px")
                                           )
                                         )
                                     ),
                                     
                                     # Key Finding
                                     div(style = "margin-top: 24px;",
                                         fluidRow(
                                           column(6,
                                                  div(style = "background: #FFF9E6; border-left: 4px solid #f59e0b; 
                                                padding: 20px; border-radius: 8px; height: 100%;",
                                                      h4("Highest Risk Group", style = "color: #2C3E50; margin-top: 0;"),
                                                      p("Hispanic women experience the highest cervical cancer incidence rates, 
                                           often due to barriers in accessing regular screening and preventive care.",
                                                        style = "color: #78350f; margin: 0; line-height: 1.6;")
                                                  )
                                           ),
                                           column(6,
                                                  div(style = "background: #E0F7FA; border-left: 4px solid #00838F; 
                                                padding: 20px; border-radius: 8px; height: 100%;",
                                                      h4("Prevention is Key", style = "color: #2C3E50; margin-top: 0;"),
                                                      p("Regular Pap tests and HPV vaccination significantly reduce cervical cancer 
                                           risk across all populations when access barriers are addressed.",
                                                        style = "color: #004d5c; margin: 0; line-height: 1.6;")
                                                  )
                                           )
                                         )
                                     )
                                 )
                        ),
                        
                        # BY STATE TAB
                        tabPanel("By State",
                                 div(style = "max-width: 1200px; margin: 0 auto;",
                                     div(style = "background: white; padding: 30px; border-radius: 12px; 
                                   box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
                                         
                                         h2("Top States by Cervical Cancer Rates",
                                            style = "color: #2C3E50; margin-top: 0; margin-bottom: 8px;"),
                                         p("Identifying high-risk regions for targeted intervention and resource allocation",
                                           style = "color: #555; font-size: 16px; margin-bottom: 24px;"),
                                         
                                         fluidRow(
                                           column(3,
                                                  div(style = "background: #E0F7FA; padding: 20px; border-radius: 8px;",
                                                      sliderInput("cervical_top_n",
                                                                  "Number of states:",
                                                                  min = 5,
                                                                  max = 20,
                                                                  value = 10,
                                                                  step = 1),
                                                      
                                                      selectInput("cervical_rank_race",
                                                                  "Select Race:",
                                                                  choices = unique(cervical_cancer_long$race),
                                                                  selected = "Overall"),
                                                      
                                                      hr(style = "border-color: #80DEEA;"),
                                                      
                                                      div(style = "background: white; padding: 15px; border-radius: 6px;",
                                              h4("Insight", style = "color: #00838F; margin-top: 0; font-size: 16px;"),

                                                          
                                                          p("This chart ranks states by cervical cancer incidence rates, helping 
                                               identify areas with the highest rates for targeted interventions.",
                                                            style = "color: #555; margin: 0; font-size: 14px; line-height: 1.6;")
                                                      )
                                                  )
                                           ),
                                           column(9,
                                                  plotOutput("cervical_top_states_plot", height = "600px")
                                           )
                                         )
                                     ),
                                     
                                     # Why This Matters
                                     div(style = "margin-top: 24px; background: #FFF9E6; border-left: 4px solid #f59e0b; 
                                   padding: 20px; border-radius: 8px;",
                                         h4("Why This Matters", style = "color: #2C3E50; margin-top: 0;"),
                                         p("State-level data reveals where screening programs and HPV vaccination efforts need 
                              strengthening. High-rate states benefit most from increased access to preventive care 
                              and public health education.",
                                           style = "color: #78350f; margin: 0; line-height: 1.6;")
                                     )
                                 )
                        )
                      )
             )
  ),
  
  # Sexual infections
  tabPanel("Sexual Infections",
           div(style = "background: linear-gradient(135deg, #F3E5F5 0%, #E1BEE7 100%);
               min-height: 100vh; padding: 40px 20px;",
               
               # Header Section
               div(style = "text-align: center; margin-bottom: 40px;
                   background: white; padding: 30px;
                   border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   h1("Sexually Transmitted Infection Rates",
                      style = "color: #2C3E50; font-weight: 700; margin-bottom: 10px;"),
                   p("Chlamydia, Syphilis, and Gonorrhea by State and Race",
                     style = "color: #555; font-size: 18px; margin: 0;")
               ),
               
               # Map Section
               div(style = "background: white; padding: 30px;
             border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);
             margin-bottom: 30px;",
                   
                   h2("STI Rates by State and Race",
                      style = "color: #2C3E50; margin-bottom: 20px;"),
                   
                   fluidRow(
                     column(12,
                            p("Explore sexually transmitted infection rates across the United States.
               Use the dropdowns to select a disease and filter by race to see
               how rates vary geographically.",
                              style = "color: #555; font-size: 16px; margin-bottom: 20px;")
                     )
                   ),
                   
                   sidebarLayout(
                     sidebarPanel(
                       style = "background: #F3E5F5; border-radius: 10px; padding: 20px;",
                       
                       selectInput("sti_disease_filter",
                                   "Select Disease:",
                                   choices = c("syphilis", "chlamydia", "gonorrhea"),
                                   selected = "syphilis"),
                       
                       selectInput("sti_race_filter",
                                   "Select Race:",
                                   choices = c("Overall"),
                                   selected = "Overall"),
                       
                       hr(style = "border-color: #CE93D8;"),
                       
                       div(style = "background: white; padding: 15px; border-radius: 8px;",
                           h4("Map Guide", style = "color: #2C3E50; margin-top: 0;"),
                           tags$ul(
                             style = "color: #555; line-height: 1.8;",
                             tags$li("Hover over states for exact rates"),
                             tags$li("Purple = Chlamydia"),
                             tags$li("Orange = Syphilis"),
                             tags$li("Green = Gonorrhea"),
                             tags$li("Darker colors = higher rates")
                           )
                       )
                     ),
                     
                     mainPanel(
                       leafletOutput("sti_map", height = "550px")
                     )
                   )
               ),
               
               #Line graph  
               div(style = "background: white; padding: 30px;
             border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);
             margin-bottom: 30px;",
                   
                   h2("STI Trends Over Time",
                      style = "color: #2C3E50; margin-bottom: 20px;"),
                   
                   fluidRow(
                     column(12,
                            p("View how chlamydia, syphilis, and gonorrhea rates have changed
               over time for a specific state and racial group.",
                              style = "color: #555; font-size: 16px; margin-bottom: 20px;")
                     )
                   ),
                   
                   sidebarLayout(
                     sidebarPanel(
                       style = "background: #F3E5F5; border-radius: 10px; padding: 20px;",
                       
                       selectInput("sti_state",
                                   "Select State:",
                                   choices = unique(sex_infect_years$state),
                                   selected = "United States"),
                       
                       selectInput("sti_race",
                                   "Select Race:",
                                   choices = unique(sex_infect_years$race),
                                   selected = "White"),
                       
                       hr(style = "border-color: #CE93D8;"),
                       
                       div(style = "background: white; padding: 15px; border-radius: 8px;",
                           h4("About This Chart", style = "color: #2C3E50; margin-top: 0;"),
                           tags$ul(
                             style = "color: #555; line-height: 1.8; font-size: 14px;",
                             tags$li("Lines show trends from 2020-2023"),
                             tags$li("Purple = Chlamydia (highest rates)"),
                             tags$li("Orange = Syphilis (lowest rates)"),
                             tags$li("Green = Gonorrhea (medium rates)"),
                             tags$li("Numbers show most recent year values"),
                             tags$li("Upward trends indicate increasing rates")
                           )
                       )
                     ),
                     
                     mainPanel(
                       plotOutput("sti_plot", height = "500px")
                     )
                   )
               ),
               
               # Chart 2: One Disease by Race
               div(style = "background: white; padding: 30px;
                   border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   
                   h2("STI Rates by Race",
                      style = "color: #2C3E50; margin-bottom: 20px;"),
                   
                   fluidRow(
                     column(12,
                            p("Examine racial and ethnic disparities for a specific STI in your selected state.",
                              style = "color: #555; font-size: 16px; margin-bottom: 20px;")
                     )
                   ),
                   
                   sidebarLayout(
                     sidebarPanel(
                       style = "background: #F3E5F5; border-radius: 10px; padding: 20px;",
                       
                       selectInput("sti_state2",
                                   "Select State:",
                                   choices = unique(syphilis$state),
                                   selected = "United States"),
                       
                       selectInput("sti_disease",
                                   "Select Disease:",
                                   choices = c("chlamydia", "syphilis", "gonorrhea"),
                                   selected = "syphilis"),
                       
                       hr(style = "border-color: #CE93D8;"),
                       
                       div(style = "background: white; padding: 15px; border-radius: 8px;",
                           h4("Understanding Disparities", style = "color: #2C3E50; margin-top: 0;"),
                           p("This chart demonstrates how different racial and ethnic groups
                    are disproportionately affected by STIs.",
                             style = "color: #555; margin: 0; font-size: 14px;")
                       )
                     ),
                     
                     mainPanel(
                       plotOutput("sti_race_plot", height = "500px")
                     )
                   )
               )
           )
  ),
  
  
  # Maternal-Infant Health
  tabPanel("Maternal-Infant Health",
           div(style = "background: linear-gradient(135deg, #F0ADAD 0%, #F3E5F5 100%);
               min-height: 100vh; padding: 40px 20px;",
               # Header Section
               div(style = "text-align: center; margin-bottom: 40px;
                   background: white; padding: 30px;
                   border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   h1("Maternal & Infant Mortality Rates",
                      style = "color: #2C3E50; font-weight: 700; margin-bottom: 10px;"),
                   p("Maternal and Infant Death by State and Race",
                     style = "color: #555; font-size: 18px; margin: 0;")
               ),
               
               # Maternal Mortality Map Section
               div(style = "background: white; padding: 30px; margin-bottom: 30px;
                          border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   h3("Maternal Mortality by State",
                      style = "color: #2C3E50; margin-bottom: 20px;"),
                   
                   fluidRow(
                     column(6,
                            selectInput("maternal_race_filter",
                                        "Filter by Race:",
                                        choices = c("Overall"),
                                        selected = "Overall")
                     ),
                     column(6,
                            sliderInput("maternal_year_slider",
                                        "Select Year:",
                                        min = 1999,
                                        max = 2019,
                                        value = 1999,
                                        step = 1,
                                        animate = animationOptions(interval = 1500, loop = TRUE),
                                        sep = "")
                     ) 
                   ),
                   
                   leafletOutput("maternal_map", height = 600)
               )
           ),
           
           # Mortality Disparity Section
           div(style = "background: white; padding: 30px; 
                   border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
               
               h2("Mortality Health Equity Analysis", 
                  style = "color: #2C3E50; margin-bottom: 20px;"),
               
               fluidRow(
                 column(12,
                        p("This chart shows how mortality rates differ from the overall 
                     average for each racial group. Red bars indicate groups with 
                     disproportionately higher rates (above average), while green bars show 
                     groups with lower rates (below average).",
                          style = "color: #555; font-size: 16px; margin-bottom: 20px;")
                 )
               ),
               
               sidebarLayout(
                 sidebarPanel(
                   style = "background: #FFF9E6; border-radius: 10px; padding: 20px;",
                   
                   selectInput("mortality_type",
                               "Select Mortality Type:",
                               choices = c("Maternal Mortality", "Infant Mortality"),
                               selected = "Maternal Mortality"),
                   
                   selectInput("mortality_disparity_state", 
                               "Select State:",
                               choices = c("United States"),
                               selected = "United States"),
                   
                   hr(style = "border-color: #FFD700;"),
                   
                   div(style = "background: white; padding: 15px; border-radius: 8px;",
                       h4("Understanding This Chart", style = "color: #2C3E50; margin-top: 0;"),
                       tags$ul(
                         style = "color: #555; line-height: 1.8; font-size: 14px;",
                         tags$li(strong("Black line at zero"), " = overall average rate"),
                         tags$li(strong("Red bars (right)"), " = higher than average"),
                         tags$li(strong("Green bars (left)"), " = lower than average"),
                         tags$li("Longer bars = bigger disparities"),
                         tags$li("Numbers show exact difference from average")
                       ),
                       tags$div(
                         style = "background: #FEF3CD; padding: 10px; border-radius: 5px; margin-top: 10px;",
                         p(strong("Why This Matters:"), style = "color: #856404; margin: 0; margin-bottom: 5px;"),
                         p(strong("Maternal:"), " Identifies which groups face highest pregnancy death risk", 
                           style = "color: #856404; margin: 0; font-size: 13px;"),
                         p(strong("Infant:"), " Shows disparities in deaths before age 1",
                           style = "color: #856404; margin: 5px 0 0 0; font-size: 13px;")
                       )
                   )
                 ),
                 
                 mainPanel(
                   plotOutput("mortality_disparity_plot", height = "550px")
                 )
               )
           ),
           
           # Pie Chart Section
           div(style = "background: white; padding: 30px; margin-bottom: 30px;
           border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
               
               h2("Mortality Distribution by Race",
                  style = "color: #2C3E50; margin-bottom: 20px;"),
               
               fluidRow(
                 column(12,
                        p("This pie chart shows how mortality rates vary across different racial 
               and ethnic groups. Larger slices indicate higher mortality rates.",
                          style = "color: #555; font-size: 16px; margin-bottom: 20px;")
                 )
               ),
               
               sidebarLayout(
                 sidebarPanel(
                   style = "background: #FFF9E6; border-radius: 10px; padding: 20px;",
                   
                   selectInput("pie_mortality_type",
                               "Select Mortality Type:",
                               choices = c("Maternal Mortality", "Infant Mortality"),
                               selected = "Maternal Mortality"),
                   
                   selectInput("pie_state",
                               "Select State:",
                               choices = NULL,  # Will be populated by server
                               selected = "United States"),
                   
                   hr(style = "border-color: #FFD700;"),
                   
                   div(style = "background: white; padding: 15px; border-radius: 8px;",
                       h4("Understanding This Chart", style = "color: #2C3E50; margin-top: 0;"),
                       tags$ul(
                         style = "color: #555; line-height: 1.8; font-size: 14px;",
                         tags$li("Each slice represents a racial/ethnic group"),
                         tags$li("Larger slices = higher mortality rates")
                       )
                   )
                 ),
                 
                 mainPanel(
                   plotOutput("mortality_pie_chart", height = "600px")
                 )
               )
           )
  ),
  
  # Insights Page
  tabPanel("Insights",
           div(style = "background: linear-gradient(135deg, #CECDF7 0%, #F3E5F5 100%);
             min-height: 100vh; padding: 40px 20px;",
               
               # Header
               div(style = "text-align: center; margin-bottom: 40px;
                 background: white; padding: 30px;
                 border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   h1("Insights and Exploring Correlation Analysis",
                      style = "color: #2C3E50; font-weight: 700; margin-bottom: 10px;")
               ),
               
               sidebarLayout(
                 sidebarPanel(width = 0),  # Empty sidebar
                 mainPanel(
                   width = 12,
                   
                   # Breast Cancer vs STI Plot Section
                   div(style = "background: white; padding: 30px; margin-bottom: 30px;
                     border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                       h2("Breast Cancer vs Sexual Infection Rate",
                          style = "color: #2C3E50; font-weight: 600; margin-bottom: 20px;"),
                       plotOutput("breast_sti_scatter", height = "600px")
                   ),
                   
                   # Cervical Cancer vs STI Plot Section
                   div(style = "background: white; padding: 30px; margin-bottom: 30px;
                     border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                       h2("Cervical Cancer vs Sexual Infection Rate",
                          style = "color: #2C3E50; font-weight: 600; margin-bottom: 20px;"),
                       plotOutput("cervical_sti_scatter", height = "600px")
                   ),
                   
                   # Maternal Mortality by Race Correlation Section
                   div(style = "background: white; padding: 30px; margin-bottom: 30px;
                            border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                       
                       h2("Maternal Mortality Trends by Race",
                          style = "color: #2C3E50; font-weight: 600; margin-bottom: 20px;"),
                       
                       fluidRow(
                         column(12,
                                p("Explore how maternal mortality rates vary across racial groups over time. 
                                This visualization helps identify disparities and trends in maternal health outcomes.",
                                  style = "color: #555; font-size: 16px; margin-bottom: 20px;")
                         )
                       ),
                       
                       sidebarLayout(
                         sidebarPanel(
                           style = "background: #FFF0F0; border-radius: 10px; padding: 20px;",
                           
                           selectInput("maternal_corr_state",
                                       "Select State or Region:",
                                       choices = NULL,  # Will be populated by server
                                       selected = "National"),
                           
                           sliderInput("maternal_year_range",
                                       "Select Year Range:",
                                       min = 1999,
                                       max = 2019,
                                       value = c(1999, 2019),
                                       step = 1,
                                       sep = ""),
                           
                           checkboxGroupInput("maternal_races_select",
                                              "Select Races to Display:",
                                              choices = c("Hispanic", "Black", "White", 
                                                          "American Indian and Alaska Native", 
                                                          "Asian or Native Hawaiian"),
                                              selected = c("Hispanic", "Black", "White")),
                           
                           hr(style = "border-color: #FFB6C1;"),
                           
                           div(style = "background: white; padding: 15px; border-radius: 8px;",
                               h4("Chart Guide", style = "color: #2C3E50; margin-top: 0;"),
                               tags$ul(
                                 style = "color: #555; line-height: 1.8; font-size: 14px;",
                                 tags$li("Each line represents a different racial group"),
                                 tags$li("Higher lines indicate higher mortality rates"),
                                 tags$li("Upward trends show worsening outcomes"),
                                 tags$li("Gaps between lines reveal disparities"),
                                 tags$li("Select/deselect races to compare specific groups")
                               )
                           )
                         ),
                         
                         mainPanel(
                           plotOutput("maternal_race_correlation", height = "550px")
                         )
                       )
                   ),
                   # Infant Mortality by Race Correlation Section
                   div(style = "background: white; padding: 30px; margin-bottom: 30px;
    border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                       
                       h2("Correlation: Race and Infant Mortality Rates",
                          style = "color: #2C3E50; font-weight: 600; margin-bottom: 20px;"),
                       
                       fluidRow(
                         column(12,
                                p("Analyze the correlation between race/ethnicity and infant mortality rates. 
               This visualization shows mean rates with confidence intervals to identify significant disparities.",
                                  style = "color: #555; font-size: 16px; margin-bottom: 20px;")
                         )
                       ),
                       
                       sidebarLayout(
                         sidebarPanel(
                           style = "background: #F0F8FF; border-radius: 10px; padding: 20px;",
                           
                           selectInput("infant_corr_state",
                                       "Select State or Region:",
                                       choices = NULL,  # Will be populated by server
                                       selected = "All States"),
                           
                           hr(style = "border-color: #B0E0E6;"),
                           
                           div(style = "background: white; padding: 15px; border-radius: 8px;",
                               h4("Chart Guide", style = "color: #2C3E50; margin-top: 0;"),
                               tags$ul(
                                 style = "color: #555; line-height: 1.8; font-size: 14px;",
                                 tags$li("Rate measured per 1,000 live births"),
                                 tags$li("Bars show mean (average) infant mortality rate"),
                                 tags$li("Error bars show 95% confidence intervals"),
                                 tags$li("Higher bars indicate stronger correlation with mortality"),
                                 tags$li("Non-overlapping error bars suggest statistically significant differences"),
                                 tags$li("'n=' shows number of data points for each race")
                               ),
                               tags$div(
                                 style = "background: #E3F2FD; padding: 10px; border-radius: 5px; margin-top: 10px;",
                                 p(strong("Interpretation:"), " This chart reveals which racial groups have higher or lower infant mortality rates on average. 
                Large differences indicate health disparities that may result from systemic inequities in healthcare access, 
                socioeconomic factors, and social determinants of health.",
                                   style = "color: #1565C0; margin: 0; font-size: 13px;")
                               )
                           )
                         ),
                         
                         mainPanel(
                           plotOutput("infant_race_correlation", height = "550px")
                         )
                       )
                   )
                 )
               )
           )
  ),
  
  tabPanel("Data",
           div(style = "background: linear-gradient(135deg, #E8EAF6 0%, #C5CAE9 100%);
                      min-height: 100vh; padding: 40px 20px;",
               
               # Header
               div(style = "text-align: center; margin-bottom: 40px;
                          background: white; padding: 30px;
                          border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   h1("View Our Datasets",
                      style = "color: #2C3E50; font-weight: 700; margin-bottom: 10px;"),
                   p("Explore the raw data behind our visualizations",
                     style = "color: #555; font-size: 18px; margin: 0;")
               ),
               
               # Dataset Selection and Display
               div(style = "background: white; padding: 30px;
                          border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   
                   fluidRow(
                     column(12,
                            p("Select a dataset below to view its contents. You can sort columns by clicking on the headers 
                            and use the search box to filter specific values.",
                              style = "color: #555; font-size: 16px; margin-bottom: 25px; line-height: 1.7;")
                     )
                   ),
                   
                   sidebarLayout(
                     sidebarPanel(
                       style = "background: #F5F5F5; border-radius: 10px; padding: 20px;",
                       
                       selectInput("dataset_choice",
                                   "Choose a dataset:",
                                   choices = c(
                                     "Breast Cancer Rates" = "breast_rate",
                                     "Cervical Cancer Rates" = "cervical_rate",
                                     "Syphilis Rates" = "syphilis_rate",
                                     "Gonorrhea Rates" = "gonorrhea_rate",
                                     "Chlamydia Rates" = "chlamydia_rate",
                                     "Maternal Mortality by Race" = "mortality_race",
                                     "Infant Death Rates" = "infant_death_rate"
                                   ),
                                   selected = "breast_rate"),
                       
                       hr(style = "border-color: #BDBDBD;"),
                       
                       div(style = "background: white; padding: 15px; border-radius: 8px;",
                           h4("Dataset Info", style = "color: #2C3E50; margin-top: 0;"),
                           uiOutput("dataset_info")
                       ),
                       
                       hr(style = "border-color: #BDBDBD;"),
                       
                       downloadButton("download_data", 
                                      "Download Dataset",
                                      style = "width: 100%; background-color: #5C6BC0; 
                                             border: none; color: white; padding: 10px;
                                             border-radius: 5px; font-weight: bold;")
                     ),
                     
                     mainPanel(
                       div(style = "overflow-x: auto;",
                           DT::dataTableOutput("data_table")
                       )
                     )
                   )
               )
           )
  ),
  
  
  # Resource Page
  tabPanel("Resources",
           div(style = "background: linear-gradient(135deg, #CECDF7 0%, #F3E5F5 100%);
             min-height: 100vh; padding: 40px 20px;",
               
               # Header
               div(style = "text-align: center; margin-bottom: 40px;
                 background: white; padding: 30px;
                 border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   h1("Connecting to Available Resources",
                      style = "color: #2C3E50; font-weight: 700; margin-bottom: 10px;"),
                   p("We Found Resources to Provide More Information and Help",
                     style = "color: #555; font-size: 18px; margin: 0;")
               ),
               
               # Breast Cancer Self Screening Section
               div(style = "background: white; padding: 30px; margin-bottom: 30px;
                 border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   
                   # Flexbox container for image and content
                   div(style = "display: flex; gap: 30px; align-items: flex-start;",
                       
                       # Text content
                       div(style = "flex: 1;",
                           h2("Breast Cancer Self Screening", 
                              style = "color: #2C3E50; font-weight: 600; margin-top: 0; margin-bottom: 15px;"),
                           p("Breast cancer self screenings are vital ways to examine if you may have breast abnormalities that may be caused by cancer. Self exams should not replace exams done by medical providers, but serve as an additional way to be knowledgeable about potential abnormalities that may arise.",
                             style = "color: #555; font-size: 16px; line-height: 1.6; margin-bottom: 15px;"),
                           p(strong("National Breast Cancer Website: "), 
                             a("https://www.nationalbreastcancer.org/breast-self-exam/", 
                               href = "https://www.nationalbreastcancer.org/breast-self-exam/", 
                               target = "_blank",
                               style = "color: #7B68EE;"),
                             style = "color: #555; font-size: 16px; margin-bottom: 15px;"),
                           p(strong("Symptoms to Watch Out For:"),
                             style = "color: #555; font-size: 16px; margin-bottom: 10px;"),
                           tags$ul(style = "color: #555; font-size: 16px; line-height: 1.6; margin-bottom: 15px;",
                                   tags$li("Change in how the breast or nipple looks"),
                                   tags$li("Change in how the breast or nipple feels"),
                                   tags$li("Change in breast or nipple appearance"),
                                   tags$li("Any unexpected discharge not as a result of breastfeeding")
                           ),
                           p(strong("Mayo Clinic Facts and Information: "), 
                             a("https://www.mayoclinic.org/tests-procedures/breast-exam/about/pac-20393237", 
                               href = "https://www.mayoclinic.org/tests-procedures/breast-exam/about/pac-20393237", 
                               target = "_blank",
                               style = "color: #7B68EE;"),
                             style = "color: #555; font-size: 16px;")
                       ),
                       
                       # Image container (on the right)
                       div(style = "flex-shrink: 0; display: flex; align-items: center;",
                           img(src = "breast_cancer.png", 
                               style = "width: 350px; height: auto;",
                               alt = "Breast Cancer Ribbon")
                       )
                   )
               ),
               
               # STD Tests Section
               div(style = "background: white; padding: 30px; margin-bottom: 30px;
                 border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   
                   div(style = "display: flex; gap: 30px; align-items: flex-start;",
                       
                       div(style = "flex: 1;",
                           h2("Sexually Transmitted Disease (STD) Tests", 
                              style = "color: #2C3E50; font-weight: 600; margin-top: 0; margin-bottom: 15px;"),
                           p("Sexually transmitted diseases are conditions that develop as a result of sexually transmitted infections. Though they are highly similar, it is important that sexually transmitted infections may not always develop into diseases with proper care and treatment.",
                             style = "color: #555; font-size: 16px; line-height: 1.6; margin-bottom: 15px;"),
                           p("Full STD panels generally involve testing for Chlamydia, Gonorrhea, Syphilis, HIV (HIV-1 and HIV-2), Hepatitis B, Hepatitis C, and Trichomoniasis.",
                             style = "color: #555; font-size: 16px; line-height: 1.6; margin-bottom: 15px;"),
                           p(strong("Get tested!"),
                             style = "color: #555; font-size: 16px; margin-bottom: 10px;"),
                           p(strong("Planned Parenthood: "), 
                             a("https://www.plannedparenthood.org/get-care/our-services/std-testing-and-treatment", 
                               href = "https://www.plannedparenthood.org/get-care/our-services/std-testing-and-treatment", 
                               target = "_blank",
                               style = "color: #7B68EE;"),
                             style = "color: #555; font-size: 16px; margin-bottom: 10px;"),
                           p(strong("CVS: "), 
                             a("https://www.cvs.com/minuteclinic/services/std-evaluate-and-treat", 
                               href = "https://www.cvs.com/minuteclinic/services/std-evaluate-and-treat", 
                               target = "_blank",
                               style = "color: #7B68EE;"),
                             style = "color: #555; font-size: 16px;")
                       ),
                       
                       div(style = "flex-shrink: 0; display: flex; align-items: center;",
                           img(src = "std_ribbon.png", 
                               style = "width: 350px; height: auto;",
                               alt = "STD Testing Icon")
                       )
                   )
               ),
               
               # Birth Centers/Hospitals Section
               div(style = "background: white; padding: 30px; margin-bottom: 30px;
                 border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   
                   div(style = "display: flex; gap: 30px; align-items: flex-start;",
                       
                       div(style = "flex: 1;",
                           h2("Birth Centers/Hospitals", 
                              style = "color: #2C3E50; font-weight: 600; margin-top: 0; margin-bottom: 15px;"),
                           p("Choosing a place to give birth is an important part of the pregnancy process. There are many reasons to choose a place other than a standard hospital, and it is important to do sufficient research to see what options may be best for you.",
                             style = "color: #555; font-size: 16px; line-height: 1.6; margin-bottom: 15px;"),
                           p(strong("Choosing a place to give birth: "), 
                             a("https://nationalpartnership.org/childbirthconnection/healthy-pregnancy/choosing-a-place-of-birth/your-options/", 
                               href = "https://nationalpartnership.org/childbirthconnection/healthy-pregnancy/choosing-a-place-of-birth/your-options/", 
                               target = "_blank",
                               style = "color: #7B68EE;"),
                             style = "color: #555; font-size: 16px; margin-bottom: 10px;"),
                           p(strong("Find a birthing center near you: "), 
                             a("https://www.birthcenters.org/find-a-birth-center", 
                               href = "https://www.birthcenters.org/find-a-birth-center", 
                               target = "_blank",
                               style = "color: #7B68EE;"),
                             style = "color: #555; font-size: 16px; margin-bottom: 10px;"),
                           p(strong("Infant mortality: "), 
                             a("https://www.cdc.gov/maternal-infant-health/infant-mortality/index.html", 
                               href = "https://www.cdc.gov/maternal-infant-health/infant-mortality/index.html", 
                               target = "_blank",
                               style = "color: #7B68EE;"),
                             style = "color: #555; font-size: 16px;")
                       ),
                       
                       div(style = "flex-shrink: 0; display: flex; align-items: center;",
                           img(src = "infant_mortality.png", 
                               style = "width: 350px; height: auto;",
                               alt = "Baby Icon")
                       )
                   )
               ),
               
               # Cervical Cancer Section
               div(style = "background: white; padding: 30px; margin-bottom: 30px;
                 border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   
                   div(style = "display: flex; gap: 30px; align-items: flex-start;",
                       
                       div(style = "flex: 1;",
                           h2("Cervical Cancer (Papanicolaou Smear)", 
                              style = "color: #2C3E50; font-weight: 600; margin-top: 0; margin-bottom: 15px;"),
                           p(strong("What is a Papanicolaou (Pap) Smear? "), "A Pap smear is an exam that identifies abnormal cervical cells. It is a great preventative measure to identifying cancer, even if there may be no family history present.",
                             style = "color: #555; font-size: 16px; line-height: 1.6; margin-bottom: 15px;"),
                           p(strong("When to get a Pap smear? "), "Generally, Pap smears should be administered every three years, starting at age 21 until 29. They may be administered more often due to family history of cervical cancer. After age 30 (until 65), they can be administered every five years alongside an HPV test.",
                             style = "color: #555; font-size: 16px; line-height: 1.6; margin-bottom: 15px;"),
                           p(strong("Pap Smear General Information: "), 
                             a("https://www.mayoclinic.org/tests-procedures/pap-smear/about/pac-20394841", 
                               href = "https://www.mayoclinic.org/tests-procedures/pap-smear/about/pac-20394841", 
                               target = "_blank",
                               style = "color: #7B68EE;"),
                             style = "color: #555; font-size: 16px; margin-bottom: 10px;"),
                           p(strong("Planned Parenthood: "), 
                             a("https://www.plannedparenthood.org/learn/cancer/cervical-cancer/whats-pap-test", 
                               href = "https://www.plannedparenthood.org/learn/cancer/cervical-cancer/whats-pap-test", 
                               target = "_blank",
                               style = "color: #7B68EE;"),
                             style = "color: #555; font-size: 16px;")
                       ),
                       
                       div(style = "flex-shrink: 0; display: flex; align-items: center;",
                           img(src = "cervical_cancer.png", 
                               style = "width: 350px; height: auto;",
                               alt = "Cervical Cancer Ribbon")
                       )
                   )
               )
           )  
  ),
  # About Tab
  tabPanel("About",
           div(style = "background: linear-gradient(135deg, #f8f9fa 0%, #f8f9fa 100%);
               min-height: 100vh; padding: 40px 20px;",
               div(style = "text-align: center; margin-bottom: 40px;
                   background: white; padding: 30px;
                   border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   h1("About Us",
                      style = "color: #2C3E50; font-weight: 700; margin-bottom: 10px;"),
                   p("Meet the People Who Created this Website!",
                     style = "color: #555; font-size: 18px; margin: 0;")
               )
           )
  )
)