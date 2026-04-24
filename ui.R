# DATA423-26S1 Assignment 2
# Hoang Anh Nguyen - 82885328


source("global.R")

ui <- navbarPage(
  title = "Assignment 2 - Hoang Anh Nguyen - 82885328",
  theme = NULL,
  
  
  # ---- MAIN TAB 1: EDA ----
  
  tabPanel("EDA",
           tabsetPanel(
             id   = "eda_tabs",
             type = "tabs",
             
             # ---- Summary -----
             tabPanel("Summary",
                      fluidPage(
                        titlePanel("Dataset Overview"),
                        fluidRow(
                          column(4, wellPanel(
                            h4("Dimensions"),
                            verbatimTextOutput("summary_dims")
                          )),
                          column(8, wellPanel(
                            h4("Numeric summary"),
                            verbatimTextOutput("summary_stats")
                          ))
                        ),
                        fluidRow(
                          column(12, wellPanel(
                            h4("Column types and first rows"),
                            verbatimTextOutput("summary_types")
                          ))
                        )
                      )
             ),
             
             # ---- Mosaic Plot ----
             tabPanel("Mosaic Plot",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Controls"),
                          checkboxGroupInput("mosaic_vars",
                                             "Select 2 or 3 categorical variables:",
                                             choices  = categorical_cols,
                                             selected = c("GOVERN_TYPE", "HEALTHCARE_BASIS")),
                          helpText("Figure 1. Mosaic plot. Cell area is proportional",
                                   "to joint frequency. Colour shows Pearson",
                                   "residuals from the independence model.")
                        ),
                        mainPanel(
                          plotOutput("mosaic_plot", height = "580px")
                        )
                      )
             ),
             
             # ---- Pair Plot ----
             tabPanel("Pair Plot",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Controls"),
                          helpText("Pick 2 to 6 numeric variables.",
                                   "More variables slow the plot."),
                          checkboxGroupInput("pair_vars",
                                             "Numeric variables:",
                                             choices  = numeric_cols,
                                             selected = c("DEATH_RATE", "INFANT_MORT",
                                                          "VAX_RATE", "DOCS")),
                          checkboxInput("pair_smooth",
                                        "Smoother on lower panels", FALSE),
                          helpText("Figure 2. GGally pair plot. Diagonal shows",
                                   "distributions, upper panels show pairwise",
                                   "correlations, lower panels show scatter.")
                        ),
                        mainPanel(
                          plotOutput("pair_plot", height = "620px")
                        )
                      )
             ),
             
             # ---- Missingness (EDA) ----
             tabPanel("Missingness",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Controls"),
                          checkboxInput("eda_miss_cluster",
                                        "Cluster rows by missingness pattern", FALSE),
                          helpText("Figure 3. vis_miss chart. Grey = present,",
                                   "black = missing. Clustering groups rows with",
                                   "similar missingness signatures together.")
                        ),
                        mainPanel(
                          plotOutput("miss_plot", height = "580px")
                        )
                      )
             ),
             
             # ---- Boxplot ----
             tabPanel("Boxplot",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Controls"),
                          checkboxGroupInput("box_vars",
                                             "Numeric variables:",
                                             choices  = numeric_cols,
                                             selected = numeric_cols),
                          sliderInput("box_threshold",
                                      "Outlier threshold (IQR multiplier):",
                                      min = 1.0, max = 5.0, value = 1.5, step = 0.5),
                          checkboxInput("box_center", "Centre (subtract mean)", TRUE),
                          checkboxInput("box_scale",  "Scale (divide by SD)",   TRUE),
                          helpText("Figure 4. car::Boxplot of selected numeric variables.",
                                   "Outlier points beyond the IQR threshold are shown",
                                   "as open circles without row-number labels.")
                        ),
                        mainPanel(
                          plotOutput("box_plot", height = "650px")
                        )
                      )
             ),
             
             # ---- Distribution ----
             tabPanel("Distribution",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Controls"),
                          selectInput("dist_var", "Numeric variable:",
                                      choices  = numeric_cols,
                                      selected = "DEATH_RATE"),
                          sliderInput("dist_bins", "Number of bins:",
                                      min = 5, max = 60, value = 30, step = 5),
                          selectInput("dist_facet", "Split by (optional):",
                                      choices  = c("None", "GOVERN_TYPE",
                                                   "HEALTHCARE_BASIS", "OBS_TYPE"),
                                      selected = "None"),
                          checkboxInput("dist_density", "Overlay density curve", TRUE),
                          checkboxInput("dist_log",     "Log10 x-axis",          FALSE),
                          helpText("Figure 5. Histogram of the selected variable.",
                                   "Density overlay shares the y-scale with the",
                                   "histogram. The optional split facets the plot",
                                   "by a categorical variable.")
                        ),
                        mainPanel(
                          plotOutput("dist_plot", height = "580px")
                        )
                      )
             ),
             
             # ---- Data Table ----
             tabPanel("Data Table",
                      fluidPage(
                        titlePanel("Full Dataset"),
                        helpText("Table 1. Full listing of Ass2Data.csv after",
                                 "placeholder replacement. Use the search box and",
                                 "column filters to inspect values."),
                        DTOutput("data_table")
                      )
             )
             
           ) # end EDA tabsetPanel
  ),  # end EDA tabPanel
  
 
  # ---- MAIN TAB 2: Strategy----

  tabPanel("Strategy",
           tabsetPanel(
             id   = "strategy_tabs",
             type = "tabs",
             
             # ---- Miss: Pattern -----
             tabPanel("Miss: Pattern",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Controls"),
                          checkboxInput("miss_pattern_cluster",
                                        "Cluster rows by missingness pattern", TRUE),
                          helpText("rpart tree predicting per-row missing value count.",
                                   "Splits indicate missingness is predictable (MAR).",
                                   "No splits would indicate MCAR.")
                        ),
                        mainPanel(
                          plotOutput("miss_rpart_plot", height = "520px")
                        )
                      )
             ),
             
             # ---- Miss: Thresholds ----
             tabPanel("Miss: Thresholds",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Variable threshold"),
                          sliderInput("var_thresh",
                                      "Remove variables with NA proportion above:",
                                      min = 0.0, max = 1.0, value = 0.5, step = 0.05),
                          h4("Observation threshold"),
                          sliderInput("obs_thresh",
                                      "Remove observations with NA proportion above:",
                                      min = 0.0, max = 1.0, value = 0.6, step = 0.05),
                          hr(),
                          verbatimTextOutput("thresh_dims_text"),
                          helpText("Move either slider to see the vis_miss chart update.",
                                   "Red column names are above the variable threshold.",
                                   "Rows above the observation threshold are lifted",
                                   "to the top of the chart and marked with a red line.")
                        ),
                        mainPanel(
                          plotOutput("thresh_vis_plot", height = "600px")
                        )
                      )
             ),
             
             # ---- Miss: Imputation -----
             tabPanel("Miss: Imputation",
                      fluidPage(
                        fluidRow(
                          column(3,
                                 wellPanel(
                                   h4("Numeric imputation"),
                                   selectInput("impute_method",
                                               "Method:",
                                               choices  = c("Median"      = "median",
                                                            "KNN"         = "knn",
                                                            "Bagged tree" = "bag"),
                                               selected = "median"),
                                   
                                   # K slider only shown when KNN is chosen
                                   conditionalPanel(
                                     condition = "input.impute_method == 'knn'",
                                     sliderInput("knn_k",
                                                 "K (number of neighbours):",
                                                 min = 1, max = 15, value = 5, step = 1)
                                   ),
                                   
                                   helpText("Median: fastest, uses the column median.",
                                            "KNN: uses the K nearest complete rows.",
                                            "Bagged tree: predicts missing cells from",
                                            "an ensemble of decision trees.",
                                            "The chart shows the data after threshold",
                                            "removal and before imputation.")
                                 )
                          ),
                          column(9,
                                 h4("Missingness after threshold removal"),
                                 plotOutput("impute_before_plot", height = "640px")
                          )
                        )
                      )
             ),
             
             # ---- Outlier ----
             tabPanel("Outlier",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Strategy"),
                          selectInput("out_strategy",
                                      "Outlier treatment:",
                                      choices  = c("Leave in place"         = "leave",
                                                   "Cap at IQR fences"      = "cap",
                                                   "Replace with NA (impute)" = "na"),
                                      selected = "leave"),
                          sliderInput("out_iqr",
                                      "IQR multiplier:",
                                      min = 1.0, max = 5.0, value = 1.5, step = 0.5),
                          h4("Apply to variables"),
                          checkboxGroupInput("out_vars",
                                             "Numeric predictors:",
                                             choices  = predictor_numeric,
                                             selected = character(0)),
                          helpText("Leave = no change, recipe still handles skew via",
                                   "Yeo-Johnson. Cap = winsorise flagged values to the",
                                   "chosen IQR fence. Replace with NA = mark flagged",
                                   "values as missing so the imputer treats them like",
                                   "any other NA. The target DEATH_RATE is never",
                                   "modified by this step.")
                        ),
                        mainPanel(
                          h4("Outlier counts by variable"),
                          tableOutput("outlier_count_table"),
                          hr(),
                          h4("Boxplot after outlier treatment"),
                          plotOutput("outlier_preview_plot", height = "520px")
                        )
                      )
             ),
             
             # ---- Recipe ----
             tabPanel("Recipe",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Pre-processing steps"),
                          checkboxInput("rec_center", "Centre numeric predictors",  TRUE),
                          checkboxInput("rec_scale",  "Scale numeric predictors",   TRUE),
                          checkboxInput("rec_yj",
                                        "Yeo-Johnson transformation", FALSE),
                          checkboxInput("rec_dummy",
                                        "Dummy-encode categorical predictors", TRUE),
                          hr(),
                          helpText("The recipe below is built from all strategy",
                                   "choices made in the Threshold, Imputation and",
                                   "Outlier tabs. Changing any control above or in",
                                   "earlier tabs rebuilds the recipe automatically.")
                        ),
                        mainPanel(
                          h4("Recipe summary"),
                          verbatimTextOutput("recipe_summary"),
                          hr(),
                          h4("Baked training data (first 6 rows, selected columns)"),
                          tableOutput("recipe_baked_preview")
                        )
                      )
             )
             
           ) # end Strategy tabsetPanel
  ),  # end Strategy tabPanel
  
  
  
  # ---- MAIN TAB 3: Modeling ----
  
  tabPanel("Modeling",
           tabsetPanel(
             id   = "model_tabs",
             type = "tabs",
             
             # ---- Hyper-parameter Tuning ----
             tabPanel("Hyper-parameter Tuning",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Tuning controls"),
                          sliderInput("cv_folds",
                                      "Cross-validation folds:",
                                      min = 3, max = 10, value = 10, step = 1),
                          sliderInput("tune_length",
                                      "Tune length (lambda grid size):",
                                      min = 5, max = 20, value = 10, step = 1),
                          sliderInput("n_alpha",
                                      "Number of alpha values:",
                                      min = 2, max = 11, value = 6, step = 1),
                          numericInput("seed",
                                       "Random seed:",
                                       value = 42, min = 1, max = 9999),
                          hr(),
                          actionButton("train_btn",
                                       "Train model",
                                       class = "btn-primary",
                                       icon  = icon("play")),
                          helpText("Click Train model to fit glmnet with the current",
                                   "recipe. Training runs cross-validation over an",
                                   "alpha x lambda grid and picks the combination",
                                   "with the lowest resampled RMSE. Large grids or",
                                   "bagged imputation can take a minute.")
                        ),
                        mainPanel(
                          h4("Training status"),
                          verbatimTextOutput("model_status"),
                          hr(),
                          h4("Optimal hyperparameters"),
                          verbatimTextOutput("model_best"),
                          hr(),
                          h4("Tuning curve"),
                          plotOutput("model_tune_plot", height = "500px")
                        )
                      )
             ),
             
             # ---- Chart ----
             tabPanel("Chart",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Test set performance"),
                          verbatimTextOutput("model_test_metrics"),
                          helpText("Test metrics are computed on rows where OBS_TYPE",
                                   "equals Test. Model must be trained first from the",
                                   "Hyper-parameter Tuning tab.")
                        ),
                        mainPanel(
                          h4("Predicted vs actual DEATH_RATE (test set)"),
                          plotOutput("model_pred_plot", height = "550px")
                        )
                      )
             ),
             
             # ---- Residuals ----
             tabPanel("Residuals",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Residual outlier labelling"),
                          sliderInput("res_iqr",
                                      "IQR multiplier:",
                                      min = 1.0, max = 5.0, value = 1.5, step = 0.5),
                          hr(),
                          h4("Flagged residual outlier rows"),
                          tableOutput("res_outlier_table"),
                          helpText("Residuals = observed minus predicted. The boxplot",
                                   "shows the residual distribution on Train, Test",
                                   "and both combined. Points beyond the IQR fence",
                                   "are labelled by CODE and listed in the table.")
                        ),
                        mainPanel(
                          plotOutput("res_box_plot", height = "550px")
                        )
                      )
             )
             
           ) # end Modeling tabsetPanel
  )   # end Modeling tabPanel
  
) # end navbarPage
