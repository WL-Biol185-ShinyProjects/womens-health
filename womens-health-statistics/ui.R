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
                   h1("Women's Health Disparities Dashboard", 
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
                      div(style = "background: linear-gradient(135deg, #FFF5F7 0%, #FFE4E9 100%); 
                                   min-height: 100vh; padding: 40px 20px;",
                          
                          # Header Section
                          div(style = "text-align: center; margin-bottom: 40px; 
                                       background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                              h1("Breast Cancer Incidence Rates", 
                                 style = "color: #2C3E50; font-weight: 700; margin-bottom: 10px;"),
                              p("Understanding breast cancer disparities across the United States",
                                style = "color: #555; font-size: 18px; margin: 0;")
                          ),
                          
                          # Map Section
                          div(style = "background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                                       margin-bottom: 30px;",
                              
                              h2("Breast Cancer Rates by State and Race", 
                                 style = "color: #2C3E50; margin-bottom: 20px;"),
                              
                              fluidRow(
                                column(12,
                                       p("Explore breast cancer incidence rates across the United States. 
                                         Use the dropdown to filter by race/ethnicity and see how rates 
                                         vary geographically.",
                                         style = "color: #555; font-size: 16px; margin-bottom: 20px;")
                                )
                              ),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  style = "background: #FFF5F7; border-radius: 10px; padding: 20px;",
                                  
                                  selectInput("race_filter", 
                                              "Select Race/Ethnicity:",
                                              choices = c("All"),
                                              selected = "All"),
                                  
                                  hr(style = "border-color: #FFB6C1;"),
                                  
                                  div(style = "background: white; padding: 15px; border-radius: 8px;",
                                      h4("Map Guide", style = "color: #2C3E50; margin-top: 0;"),
                                      tags$ul(
                                        style = "color: #555; line-height: 1.8;",
                                        tags$li("Hover over states for exact rates"),
                                        tags$li("Change race filter to update map"),
                                        tags$li("Darker colors = higher rates")
                                      )
                                  )
                                ),
                                
                                mainPanel(
                                  leafletOutput("cancer_map", height = "550px")
                                )
                              )
                          ),
                          
                          # Chart Section - By Race
                          div(style = "background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                                       margin-bottom: 30px;",
                              
                              h2("Breast Cancer Incidence by Race", 
                                 style = "color: #2C3E50; margin-bottom: 20px;"),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  style = "background: #FFF5F7; border-radius: 10px; padding: 20px;",
                                  
                                  selectInput("state", 
                                              "Choose a state:",
                                              choices = unique(breast_cancer_long$state),
                                              selected = "United States"),
                                  
                                  hr(style = "border-color: #FFB6C1;"),
                                  
                                  div(style = "background: white; padding: 15px; border-radius: 8px;",
                                      p("This chart shows breast cancer incidence rates by race/ethnicity 
                                        for your selected state.", 
                                        style = "color: #555; margin: 0;")
                                  )
                                ),
                                
                                mainPanel(
                                  plotOutput("breast_race_plot", height = "500px")
                                )
                              )
                          ),
                          
                          # Top States Ranking Section
                          div(style = "background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                              
                              h2("Top States by Breast Cancer Rates", 
                                 style = "color: #2C3E50; margin-bottom: 20px;"),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  style = "background: #FFF5F7; border-radius: 10px; padding: 20px;",
                                  
                                  sliderInput("top_n", 
                                              "Number of states to show:",
                                              min = 5, 
                                              max = 20, 
                                              value = 10,
                                              step = 1),
                                  
                                  selectInput("rank_race", 
                                              "Select Race/Ethnicity:",
                                              choices = unique(breast_cancer_long$race),
                                              selected = "Overall"),
                                  
                                  hr(style = "border-color: #FFB6C1;"),
                                  
                                  div(style = "background: white; padding: 15px; border-radius: 8px;",
                                      p("This chart ranks states by breast cancer incidence rates, 
                                        helping identify areas with the highest rates for targeted interventions.", 
                                        style = "color: #555; margin: 0;")
                                  )
                                ),
                                
                                mainPanel(
                                  plotOutput("top_states_plot", height = "600px")
                                )
                              )
                          )
                      )
             ),
             
             
             tabPanel("Cervical Cancer",
                      div(style = "background: linear-gradient(135deg, #E0F7FA 0%, #B2EBF2 100%); 
                                   min-height: 100vh; padding: 40px 20px;",
                          
                          # Header Section
                          div(style = "text-align: center; margin-bottom: 40px; 
                                       background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                              h1("Cervical Cancer Incidence Rates", 
                                 style = "color: #2C3E50; font-weight: 700; margin-bottom: 10px;"),
                              p("Understanding cervical cancer disparities across the United States",
                                style = "color: #555; font-size: 18px; margin: 0;")
                          ),
                          
                          # Map Section
                          div(style = "background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                                       margin-bottom: 30px;",
                              
                              h2("Cervical Cancer Rates by State and Race", 
                                 style = "color: #2C3E50; margin-bottom: 20px;"),
                              
                              fluidRow(
                                column(12,
                                       p("Explore cervical cancer incidence rates across the United States. 
                                         Use the dropdown to filter by race/ethnicity and see how rates 
                                         vary geographically.",
                                         style = "color: #555; font-size: 16px; margin-bottom: 20px;")
                                )
                              ),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  style = "background: #E0F7FA; border-radius: 10px; padding: 20px;",
                                  
                                  selectInput("cervical_race_filter", 
                                              "Select Race/Ethnicity:",
                                              choices = c("All"),
                                              selected = "All"),
                                  
                                  hr(style = "border-color: #80DEEA;"),
                                  
                                  div(style = "background: white; padding: 15px; border-radius: 8px;",
                                      h4("Map Guide", style = "color: #00838F; margin-top: 0;"),
                                      tags$ul(
                                        style = "color: #555; line-height: 1.8;",
                                        tags$li("Hover over states for exact rates"),
                                        tags$li("Change race filter to update map"),
                                        tags$li("Darker colors = higher rates")
                                      )
                                  )
                                ),
                                
                                mainPanel(
                                  leafletOutput("cervical_map", height = "550px")
                                )
                              )
                          ),
                          
                          # Chart Section - By Race
                          div(style = "background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                                       margin-bottom: 30px;",
                              
                              h2("Cervical Cancer Incidence by Race", 
                                 style = "color: #2C3E50; margin-bottom: 20px;"),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  style = "background: #E0F7FA; border-radius: 10px; padding: 20px;",
                                  
                                  selectInput("cervical_state", 
                                              "Choose a state:",
                                              choices = unique(cervical_cancer_long$state),
                                              selected = "United States"),
                                  
                                  hr(style = "border-color: #80DEEA;"),
                                  
                                  div(style = "background: white; padding: 15px; border-radius: 8px;",
                                      p("This chart shows cervical cancer incidence rates by race/ethnicity 
                                        for your selected state.", 
                                        style = "color: #555; margin: 0;")
                                  )
                                ),
                                
                                mainPanel(
                                  plotOutput("cervical_race_plot", height = "500px")
                                )
                              )
                          ),
                          
                          # Top States Ranking Section
                          div(style = "background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                              
                              h2("Top States by Cervical Cancer Rates", 
                                 style = "color: #2C3E50; margin-bottom: 20px;"),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  style = "background: #E0F7FA; border-radius: 10px; padding: 20px;",
                                  
                                  sliderInput("cervical_top_n", 
                                              "Number of states to show:",
                                              min = 5, 
                                              max = 20, 
                                              value = 10,
                                              step = 1),
                                  
                                  selectInput("cervical_rank_race", 
                                              "Select Race/Ethnicity:",
                                              choices = unique(cervical_cancer_long$race),
                                              selected = "Overall"),
                                  
                                  hr(style = "border-color: #80DEEA;"),
                                  
                                  div(style = "background: white; padding: 15px; border-radius: 8px;",
                                      p("This chart ranks states by cervical cancer incidence rates, 
                                        helping identify areas with the highest rates for targeted interventions.", 
                                        style = "color: #555; margin: 0;")
                                  )
                                ),
                                
                                mainPanel(
                                  plotOutput("cervical_top_states_plot", height = "600px")
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
                                   choices = c("All"),
                                   selected = "All"),
                       
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
           div(style = "padding: 40px 20px;",
               div(style = "text-align: center; margin-bottom: 40px; 
                            background: white; padding: 30px; 
                            border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   h1("Maternal & Infant Mortality Rates", 
                      style = "color: #2C3E50; font-weight: 700; margin-bottom: 10px;"),
                   p("Statement TBD",
                     style = "color: #555; font-size: 18px; margin: 0;")
               )
           )
  ),
  
  # All health combined
  tabPanel("Changes in Health",
           div(style = "padding: 40px 20px;",
               div(style = "text-align: center; margin-bottom: 40px; 
                          background: white; padding: 30px; 
                          border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   h1("Women's Health Over Time", 
                      style = "color: #2C3E50; font-weight: 700; margin-bottom: 10px;"),
                   p("Statement TBD",
                     style = "color: #555; font-size: 18px; margin: 0;")
               ),
               
               # STD Rates Section
               div(style = "background: white; padding: 30px; 
                          border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   h2("STI Rates by Race and Year", 
                      style = "color: #2C3E50; margin-bottom: 20px;"),
                   
                   sidebarLayout(      
                     # Define the sidebar with inputs
                     sidebarPanel(
                       selectInput("state", "State:", 
                                   choices=unique(sex_infect_years$state)),
                       selectInput("disease", "Disease:", 
                                   choices=c("chlamydia", "syphilis", "gonorrhea")),
                       selectInput("race", "Race:", 
                                   choices=c("White", "Black", "Hispanic", "AmericanIndian_AlaskaNative", 
                                             "Asian", "Native_Hawaiian_or_PacificIslander", "Multiple races", "Overall")),
                       hr(),
                       helpText("Data showing STI rates by race from 2020-2023.")
                     ),
                     
                     # Create a spot for the barplot
                     mainPanel(
                       plotOutput("diseasePlot")  
                     )
                   )
               )
           )
  ),
  
  # About Tab
  tabPanel("About",
           div(style = "padding: 40px 20px;",
               fluidRow(
                 column(12,
                        div(style = "background: white; padding: 30px; 
                                     border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                            h2("About This Data", style = "color: #2C3E50; font-weight: 700;"),
                            p("Detailed information about data sources, methodology, and limitations...",
                              style = "font-size: 16px;")
                        )
                 )
               )
           )
  )
)