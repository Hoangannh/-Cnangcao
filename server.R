# DATA423-26S1 Assignment 2
# Hoang Anh Nguyen - 82885328

library(shiny)
library(ggplot2)
library(GGally)
library(vcd)
library(corrgram)
library(visdat)
library(DT)
library(car)
library(dplyr)
library(tidyr)
library(tibble)
library(rpart)
library(rpart.plot)
library(recipes)
library(caret)
library(glmnet)

server <- function(input, output, session) {
  
  
  # Reactive cascade:
  # getData -> getCleanMissing -> getCleanOutliers -> getRecipe -> getModel
  
  
  # ---- getData: raw data from global.R -----
  getData <- reactive({
    dat
  })
  
  # ---- getCleanMissing: threshold choices ----
  # Shadow variable and Unknown level are always applied because they are
  # structural strategy choices, not tunable options.
  getCleanMissing <- reactive({
    d <- getData()
    
    # Always: shadow variable for HEALTHCARE_COST (created before threshold
    # removal so it survives even if the column is later dropped).
    d$HEALTHCARE_COST_NA <- as.numeric(is.na(d$HEALTHCARE_COST))
    
    # Always: replace GOVERN_TYPE NA with "Unknown" level.
    d$GOVERN_TYPE                       <- as.character(d$GOVERN_TYPE)
    d$GOVERN_TYPE[is.na(d$GOVERN_TYPE)] <- "Unknown"
    d$GOVERN_TYPE                       <- as.factor(d$GOVERN_TYPE)
    
    # Remove variables whose NA proportion exceeds var_thresh.
    # Always protect CODE, OBS_TYPE and DEATH_RATE from removal.
    protected <- c("CODE", "OBS_TYPE", "DEATH_RATE")
    v_ratio   <- colMeans(is.na(d))
    keep_vars <- names(v_ratio)[
      v_ratio < input$var_thresh | names(v_ratio) %in% protected
    ]
    d <- d[, keep_vars, drop = FALSE]
    
    # Remove observations whose NA proportion exceeds obs_thresh.
    o_ratio <- rowMeans(is.na(d))
    d       <- d[o_ratio < input$obs_thresh, , drop = FALSE]
    
    d
  })
  
  # ---- getCleanOutliers: user-selected strategy ----
  # Strategy choices: leave, cap (winsorise), na (replace with NA).
  # The target DEATH_RATE is never modified by this step, regardless of
  # whether the user ticks it or not (defensive guard below).
  getCleanOutliers <- reactive({
    d     <- getCleanMissing()
    strat <- input$out_strategy
    m     <- input$out_iqr
    vars  <- intersect(input$out_vars, names(d))
    vars  <- setdiff(vars, "DEATH_RATE")   # never touch the target
    
    if (strat == "leave" || length(vars) == 0) return(d)
    
    # Loop over selected variables. Intermediate-level base R loop is
    # easier to defend in a viva than a purrr::map call.
    for (v in vars) {
      x   <- d[[v]]
      q1  <- quantile(x, 0.25, na.rm = TRUE)
      q3  <- quantile(x, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lo  <- q1 - m * iqr
      hi  <- q3 + m * iqr
      
      if (strat == "cap") {
        x[!is.na(x) & x < lo] <- lo
        x[!is.na(x) & x > hi] <- hi
      } else if (strat == "na") {
        x[!is.na(x) & (x < lo | x > hi)] <- NA
      }
      d[[v]] <- x
    }
    d
  })
  
  # ---- getRecipe: build and prep the recipes pipeline ----
  getRecipe <- reactive({
    d     <- getCleanOutliers()
    train <- d[d$OBS_TYPE == "Train", ]
    
    rec <- recipes::recipe(DEATH_RATE ~ ., data = d)
    rec <- recipes::update_role(rec, CODE,     new_role = "id")
    rec <- recipes::update_role(rec, OBS_TYPE, new_role = "split")
    
    # step_unknown is safe to call even when no NAs remain.
    if ("GOVERN_TYPE" %in% names(d)) {
      rec <- recipes::step_unknown(rec, GOVERN_TYPE, new_level = "Unknown")
    }
    
    # Numeric imputation: method selected by the user.
    if (input$impute_method == "median") {
      rec <- recipes::step_impute_median(rec, recipes::all_numeric_predictors())
    } else if (input$impute_method == "knn") {
      rec <- recipes::step_impute_knn(rec,
                                      recipes::all_numeric_predictors(),
                                      neighbors = input$knn_k)
    } else if (input$impute_method == "bag") {
      rec <- recipes::step_impute_bag(rec, recipes::all_numeric_predictors())
    }
    
    # Yeo-Johnson before centre/scale if chosen.
    if (input$rec_yj) {
      rec <- recipes::step_YeoJohnson(rec, recipes::all_numeric_predictors())
    }
    
    if (input$rec_center) {
      rec <- recipes::step_center(rec, recipes::all_numeric_predictors())
    }
    if (input$rec_scale) {
      rec <- recipes::step_scale(rec, recipes::all_numeric_predictors())
    }
    
    if (input$rec_dummy) {
      rec <- recipes::step_dummy(rec, recipes::all_nominal_predictors())
    }
    
    recipes::prep(rec, training = train, verbose = FALSE)
  })
  
  # ---- getModel: gated by actionButton ------
  # eventReactive waits for input$train_btn before executing. This stops
  # glmnet retraining on every slider tick.
  getModel <- eventReactive(input$train_btn, {
    rec   <- getRecipe()
    d     <- getCleanOutliers()
    train <- d[d$OBS_TYPE == "Train", ]
    test  <- d[d$OBS_TYPE == "Test",  ]
    
    # Bake train and test through the prepped recipe.
    baked_train <- recipes::bake(rec, new_data = train)
    baked_test  <- recipes::bake(rec, new_data = test)
    
    # Drop id and split columns before passing to caret::train.
    drop_cols   <- intersect(c("CODE", "OBS_TYPE"), names(baked_train))
    baked_train <- baked_train[, setdiff(names(baked_train), drop_cols)]
    baked_test  <- baked_test[,  setdiff(names(baked_test),  drop_cols)]
    
    # Build the tuning grid. alpha values evenly spaced on [0,1],
    # lambda on a log scale chosen by tune_length.
    alpha_grid  <- seq(0, 1, length.out = input$n_alpha)
    lambda_grid <- 10 ^ seq(-3, 1, length.out = input$tune_length)
    grid        <- expand.grid(alpha = alpha_grid, lambda = lambda_grid)
    
    ctrl <- caret::trainControl(method = "cv", number = input$cv_folds)
    
    set.seed(input$seed)
    withProgress(message = "Training glmnet", value = 0.5, {
      fit <- caret::train(
        DEATH_RATE ~ .,
        data      = baked_train,
        method    = "glmnet",
        trControl = ctrl,
        tuneGrid  = grid,
        metric    = "RMSE"
      )
    })
    
    # Predictions on train and test for residuals.
    pred_train <- predict(fit, newdata = baked_train)
    pred_test  <- predict(fit, newdata = baked_test)
    
    # Carry CODE through for residual labelling.
    code_train <- train$CODE
    code_test  <- test$CODE
    
    list(
      fit          = fit,
      baked_train  = baked_train,
      baked_test   = baked_test,
      actual_train = baked_train$DEATH_RATE,
      actual_test  = baked_test$DEATH_RATE,
      pred_train   = pred_train,
      pred_test    = pred_test,
      code_train   = code_train,
      code_test    = code_test
    )
  })
  
  
  
  # ---- EDA subtab 1: Summary ----
  
  output$summary_dims <- renderPrint({
    d <- getData()
    cat("Rows (observations):", nrow(d), "\n")
    cat("Columns (variables):", ncol(d), "\n")
    cat("Numeric columns    :", sum(sapply(d, is.numeric)), "\n")
    cat("Factor columns     :", sum(sapply(d, is.factor)), "\n")
    cat("Missing cells      :", sum(is.na(d)), "\n")
    cat("Rows with any NA   :", sum(apply(is.na(d), 1, any)), "\n")
  })
  
  output$summary_stats <- renderPrint({
    d <- getData()
    summary(d[, numeric_cols])
  })
  
  output$summary_types <- renderPrint({
    tibble::glimpse(getData())
  })
  
  
  
  # ---- EDA subtab 2: Mosaic Plot ----
  
  output$mosaic_plot <- renderPlot({
    req(length(input$mosaic_vars) >= 2)
    
    sel     <- input$mosaic_vars
    sub_dat <- getData()[, sel, drop = FALSE]
    fmla    <- as.formula(paste("~", paste(sel, collapse = " + ")))
    
    vcd::mosaic(
      fmla,
      data   = sub_dat,
      shade  = TRUE,
      legend = TRUE,
      main   = paste("Figure 1. Mosaic Plot:", paste(sel, collapse = " x "))
    )
  })
  
  
  
  # ---- EDA subtab 3: Pair Plot ----
  
  output$pair_plot <- renderPlot({
    req(length(input$pair_vars) >= 2)
    
    cols    <- input$pair_vars
    sub_dat <- getData()[, cols, drop = FALSE]
    
    lower_fn <- if (input$pair_smooth) {
      list(continuous = wrap("smooth", alpha = 0.3, size = 0.5))
    } else {
      list(continuous = wrap("points", alpha = 0.3, size = 0.5))
    }
    
    GGally::ggpairs(
      sub_dat,
      lower    = lower_fn,
      title    = paste0("Figure 2. Pair Plot: ", paste(cols, collapse = ", ")),
      progress = FALSE
    ) +
      theme_bw(base_size = 11)
  })
  
  
  
  # ---- EDA subtab 4: Missingness ----
  
  output$miss_plot <- renderPlot({
    d <- getData()
    p <- visdat::vis_miss(d, cluster = input$eda_miss_cluster) +
      labs(title    = "Figure 3. Missing Data Pattern",
           subtitle = paste0("Clustered: ", input$eda_miss_cluster)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
    print(p)
  })
  
  
  
  # ---- EDA subtab 5: Boxplot ----
  
  output$box_plot <- renderPlot({
    req(length(input$box_vars) >= 1)
    
    sel     <- input$box_vars
    sub_dat <- getData()[, sel, drop = FALSE]
    
    sub_mat <- scale(sub_dat,
                     center = input$box_center,
                     scale  = input$box_scale)
    
    old_par <- par(mar = c(10, 4, 5, 2))
    on.exit(par(old_par), add = TRUE)
    
    car::Boxplot(
      as.data.frame(sub_mat),
      id       = FALSE,
      range    = input$box_threshold,
      las      = 2,
      col      = "lightblue",
      main     = "Figure 4. Boxplot of Selected Numeric Variables",
      ylab     = if (input$box_scale) "Scaled value" else "Value",
      cex.axis = 0.82
    )
    
    mtext(
      text = paste0("IQR multiplier: ", input$box_threshold,
                    "  |  Centred: ",   input$box_center,
                    "  |  Scaled: ",    input$box_scale),
      side = 3, line = 3.2, cex = 0.82, col = "grey40"
    )
  })
  
  
  
  # ---- EDA subtab 6: Distribution ----
  
  output$dist_plot <- renderPlot({
    d    <- getData()
    var  <- input$dist_var
    bins <- input$dist_bins
    
    df <- data.frame(value = d[[var]])
    if (input$dist_facet != "None") {
      df$facet_var <- d[[input$dist_facet]]
    }
    df <- df[!is.na(df$value), , drop = FALSE]
    
    p <- ggplot(df, aes(x = value))
    
    if (input$dist_density) {
      p <- p +
        geom_histogram(aes(y = after_stat(density)),
                       bins = bins, fill = "steelblue",
                       colour = "white", alpha = 0.9) +
        geom_density(colour = "red", linewidth = 0.7, fill = NA)
      y_label <- "Density"
    } else {
      p <- p +
        geom_histogram(bins = bins, fill = "steelblue",
                       colour = "white", alpha = 0.9)
      y_label <- "Count"
    }
    
    p <- p +
      labs(title    = paste("Figure 5. Distribution of", var),
           subtitle = paste0("Bins: ", bins,
                             "  |  Non-missing: ", nrow(df),
                             "  |  Split: ", input$dist_facet),
           x = var, y = y_label) +
      theme_bw(base_size = 12)
    
    if (input$dist_log)             { p <- p + scale_x_log10() }
    if (input$dist_facet != "None") { p <- p + facet_wrap(~ facet_var, scales = "free_y") }
    
    print(p)
  })
  
  
  
  # ---- EDA subtab 7: Data Table ----
  
  output$data_table <- DT::renderDT({
    DT::datatable(
      getData(),
      options  = list(pageLength = 15, scrollX = TRUE,
                      searchHighlight = TRUE),
      filter   = "top",
      rownames = FALSE,
      caption  = "Table 1. Full listing of Ass2Data.csv after placeholder replacement."
    )
  })
  
  
  
  # ---- Strategy subtab: Miss Pattern ----
  
  output$miss_rpart_plot <- renderPlot({
    d                  <- getData()
    L                  <- is.na(d) + 0
    d_tree             <- d
    d_tree$MISSINGNESS <- apply(L, 1, sum)
    
    tree_model <- rpart::rpart(
      formula   = MISSINGNESS ~ . - CODE - OBS_TYPE - DEATH_RATE,
      data      = d_tree,
      method    = "anova",
      na.action = na.rpart
    )
    
    rpart.plot::rpart.plot(
      tree_model,
      shadow.col = "gray",
      main       = "Predicting per-row missing value count",
      roundint   = TRUE,
      clip.facs  = TRUE
    )
  })
  
  
  
  # ---- Strategy subtab: Miss Thresholds ----
  
  output$thresh_dims_text <- renderPrint({
    d <- getData()
    
    protected <- c("CODE", "OBS_TYPE", "DEATH_RATE")
    v_ratio   <- colMeans(is.na(d))
    keep_vars <- names(v_ratio)[
      v_ratio < input$var_thresh | names(v_ratio) %in% protected
    ]
    o_ratio   <- rowMeans(is.na(d))
    keep_rows <- o_ratio < input$obs_thresh
    
    cat("Variables removed   :", ncol(d) - length(keep_vars), "\n")
    cat("Variables kept      :", length(keep_vars), "\n")
    cat("Observations removed:", sum(!keep_rows), "\n")
    cat("Observations kept   :", sum(keep_rows), "\n")
  })
  
  output$thresh_vis_plot <- renderPlot({
    d <- getData()
    
    protected <- c("CODE", "OBS_TYPE", "DEATH_RATE")
    v_ratio   <- colMeans(is.na(d))
    drop_vars <- names(v_ratio)[
      v_ratio >= input$var_thresh & !names(v_ratio) %in% protected
    ]
    
    o_ratio   <- rowMeans(is.na(d))
    drop_rows <- which(o_ratio >= input$obs_thresh)
    
    row_order <- c(drop_rows, setdiff(seq_len(nrow(d)), drop_rows))
    d_sorted  <- d[row_order, , drop = FALSE]
    
    sub_text <- paste0(
      "Variable threshold: ", input$var_thresh,
      "  removes ", length(drop_vars), " column(s): ",
      if (length(drop_vars) == 0) "none" else paste(drop_vars, collapse = ", "),
      "\nObservation threshold: ", input$obs_thresh,
      "  removes ", length(drop_rows), " row(s)"
    )
    
    p <- visdat::vis_miss(d_sorted, cluster = FALSE) +
      labs(title    = "Missingness after applying thresholds",
           subtitle = sub_text) +
      theme(
        axis.text.x = element_text(
          angle  = 45, hjust = 1, size = 8,
          colour = ifelse(names(d_sorted) %in% drop_vars, "red", "black")
        ),
        plot.subtitle = element_text(size = 8, colour = "grey30")
      )
    
    if (length(drop_rows) > 0) {
      p <- p + geom_hline(
        yintercept = length(drop_rows) + 0.5,
        colour     = "red",
        linewidth  = 1
      )
    }
    
    print(p)
  })
  
  
  
  # --- Strategy subtab: Miss Imputation ----
  
  output$impute_before_plot <- renderPlot({
    d        <- getCleanMissing()
    total_na <- sum(is.na(d))
    
    method_text <- if (input$impute_method == "knn") {
      paste0("KNN (K = ", input$knn_k, ")")
    } else if (input$impute_method == "median") {
      "Median"
    } else {
      "Bagged tree"
    }
    
    p <- visdat::vis_miss(d, cluster = FALSE) +
      labs(title    = "Missing values after threshold removal",
           subtitle = paste0("Total missing cells: ", total_na,
                             "  |  Imputation method: ", method_text)) +
      theme(
        axis.text.x   = element_text(angle = 45, hjust = 1, size = 9),
        plot.subtitle = element_text(size = 10, colour = "grey30")
      )
    print(p)
  })
  
  
  
  # Strategy subtab: Outlier
  # Two outputs:
  #   1. outlier_count_table: count of flagged values per selected variable.
  #   2. outlier_preview_plot: boxplot after the chosen strategy has run.
  
  output$outlier_count_table <- renderTable({
    d    <- getCleanMissing()
    m    <- input$out_iqr
    vars <- intersect(predictor_numeric, names(d))
    
    rows <- lapply(vars, function(v) {
      x   <- d[[v]]
      x   <- x[!is.na(x)]
      q1  <- quantile(x, 0.25)
      q3  <- quantile(x, 0.75)
      iqr <- q3 - q1
      lo  <- q1 - m * iqr
      hi  <- q3 + m * iqr
      n_lo  <- sum(x < lo)
      n_hi  <- sum(x > hi)
      data.frame(
        Variable    = v,
        N           = length(x),
        Below_fence = n_lo,
        Above_fence = n_hi,
        Total       = n_lo + n_hi,
        Pct         = round((n_lo + n_hi) / length(x) * 100, 1),
        Lower_fence = round(lo, 2),
        Upper_fence = round(hi, 2)
      )
    })
    do.call(rbind, rows)
  }, digits = 2)
  
  output$outlier_preview_plot <- renderPlot({
    d <- getCleanOutliers()
    
    # Scale everything onto a common axis so all variables fit one plot.
    sub     <- d[, intersect(predictor_numeric, names(d)), drop = FALSE]
    sub_mat <- scale(sub, center = TRUE, scale = TRUE)
    
    old_par <- par(mar = c(10, 4, 5, 2))
    on.exit(par(old_par), add = TRUE)
    
    car::Boxplot(
      as.data.frame(sub_mat),
      id       = FALSE,
      range    = input$out_iqr,
      las      = 2,
      col      = "lightgreen",
      main     = paste0("Boxplot after outlier strategy: '",
                        input$out_strategy, "'"),
      ylab     = "Standard deviations from mean",
      cex.axis = 0.82
    )
    
    mtext(
      text = paste0("IQR multiplier: ", input$out_iqr,
                    "  |  Variables modified: ",
                    length(input$out_vars)),
      side = 3, line = 3.2, cex = 0.82, col = "grey40"
    )
  })
  
  
  
  # ---- Strategy subtab: Recipe ----
  
  output$recipe_summary <- renderPrint({
    print(getRecipe())
  })
  
  output$recipe_baked_preview <- renderTable({
    rec   <- getRecipe()
    d     <- getCleanOutliers()
    train <- d[d$OBS_TYPE == "Train", ]
    baked <- recipes::bake(rec, new_data = train)
    
    show_cols <- intersect(
      c("DEATH_RATE", "POPULATION", "GDP", "INFANT_MORT",
        "VAX_RATE", "DOCS", "HEALTHCARE_COST",
        "GOVERN_TYPE_STABLE.DEM", "GOVERN_TYPE_Unknown",
        "HEALTHCARE_BASIS_INSURANCE"),
      names(baked)
    )
    round(head(baked[, show_cols, drop = FALSE], 6), 3)
  }, digits = 3)
  
  
  
  # ---- Modeling subtab: Hyper-parameter Tuning ----
  
  output$model_status <- renderPrint({
    if (input$train_btn == 0) {
      cat("Model not trained yet. Click 'Train model' to start.")
      return(invisible())
    }
    m <- getModel()
    cat("Training complete.\n")
    cat("Observations (train):", nrow(m$baked_train), "\n")
    cat("Observations (test) :", nrow(m$baked_test),  "\n")
    cat("Predictors after recipe:", ncol(m$baked_train) - 1, "\n")
    cat("Resampling: ", input$cv_folds, "-fold CV\n", sep = "")
    cat("Grid size : ", nrow(m$fit$results), " combinations\n", sep = "")
  })
  
  output$model_best <- renderPrint({
    if (input$train_btn == 0) {
      cat("Model not trained yet.")
      return(invisible())
    }
    m    <- getModel()
    best <- m$fit$bestTune
    res  <- m$fit$results
    br   <- res[res$alpha  == best$alpha &
                  res$lambda == best$lambda, ]
    
    cat("Best alpha       :", round(best$alpha,  4), "\n")
    cat("Best lambda      :", round(best$lambda, 5), "\n")
    cat("CV RMSE  (train) :", round(br$RMSE,     4), "\n")
    cat("CV R^2   (train) :", round(br$Rsquared, 4), "\n")
    cat("CV MAE   (train) :", round(br$MAE,      4), "\n")
  })
  
  output$model_tune_plot <- renderPlot({
    if (input$train_btn == 0) {
      plot.new(); title("Model not trained yet.")
      return(invisible())
    }
    m   <- getModel()
    res <- m$fit$results
    
    ggplot(res, aes(x = lambda, y = RMSE,
                    colour = factor(round(alpha, 2)))) +
      geom_line(linewidth = 0.6) +
      geom_point(size = 1.2) +
      scale_x_log10() +
      labs(title    = "Figure 10. glmnet tuning curve",
           subtitle = paste0("Best alpha = ", round(m$fit$bestTune$alpha, 3),
                             ", best lambda = ",
                             round(m$fit$bestTune$lambda, 4)),
           x        = "lambda (log scale)",
           y        = "RMSE (cross-validated)",
           colour   = "alpha") +
      theme_bw(base_size = 12)
  })
  
  
  
  # ---- Modeling subtab: Chart ----
  # Predicted vs actual on the test set with a y = x reference line.
  
  output$model_test_metrics <- renderPrint({
    if (input$train_btn == 0) {
      cat("Model not trained yet.")
      return(invisible())
    }
    m  <- getModel()
    pm <- caret::postResample(pred = m$pred_test, obs = m$actual_test)
    
    cat("Test RMSE :", round(pm["RMSE"],     4), "\n")
    cat("Test R^2  :", round(pm["Rsquared"], 4), "\n")
    cat("Test MAE  :", round(pm["MAE"],      4), "\n")
  })
  
  output$model_pred_plot <- renderPlot({
    if (input$train_btn == 0) {
      plot.new(); title("Model not trained yet.")
      return(invisible())
    }
    m  <- getModel()
    df <- data.frame(actual = m$actual_test, predicted = m$pred_test)
    
    ggplot(df, aes(x = actual, y = predicted)) +
      geom_point(alpha = 0.6, colour = "steelblue", size = 2) +
      geom_abline(slope = 1, intercept = 0,
                  colour = "red", linetype = "dashed") +
      labs(title    = "Figure 11. Predicted vs actual DEATH_RATE (test set)",
           subtitle = paste0("Test RMSE = ",
                             round(caret::RMSE(m$pred_test, m$actual_test), 3),
                             ", n = ", nrow(df)),
           x        = "Actual DEATH_RATE",
           y        = "Predicted DEATH_RATE") +
      theme_bw(base_size = 12)
  })
  
  
  
  # ---- Modeling subtab: Residuals ----
  # Boxplot of residuals on Train / Test / Combined.
  # IQR slider labels points beyond the fences by CODE.
  
  resid_df <- reactive({
    m <- getModel()
    rbind(
      data.frame(CODE     = m$code_train,
                 residual = m$actual_train - m$pred_train,
                 Set      = "Train"),
      data.frame(CODE     = m$code_test,
                 residual = m$actual_test  - m$pred_test,
                 Set      = "Test")
    )
  })
  
  # Helper: flag rows beyond the IQR fences for a given subset.
  flag_outliers <- function(df_subset, m) {
    x   <- df_subset$residual
    q1  <- quantile(x, 0.25, na.rm = TRUE)
    q3  <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lo  <- q1 - m * iqr
    hi  <- q3 + m * iqr
    df_subset[x < lo | x > hi, ]
  }
  
  output$res_box_plot <- renderPlot({
    if (input$train_btn == 0) {
      plot.new(); title("Model not trained yet.")
      return(invisible())
    }
    rdf <- resid_df()
    
    # Build a third "Both" set that combines train and test residuals.
    rdf_both     <- rdf
    rdf_both$Set <- "Both"
    plot_df      <- rbind(rdf, rdf_both)
    plot_df$Set  <- factor(plot_df$Set, levels = c("Train", "Test", "Both"))
    
    ggplot(plot_df, aes(x = Set, y = residual, fill = Set)) +
      geom_boxplot(coef = input$res_iqr, outlier.colour = "red") +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
      labs(title    = "Figure 12. Residual boxplots by observation set",
           subtitle = paste0("IQR multiplier: ", input$res_iqr),
           x        = "Observation set",
           y        = "Residual (actual - predicted)") +
      theme_bw(base_size = 12) +
      theme(legend.position = "none")
  })
  
  output$res_outlier_table <- renderTable({
    if (input$train_btn == 0) return(NULL)
    rdf <- resid_df()
    m   <- input$res_iqr
    
    tr_out <- flag_outliers(rdf[rdf$Set == "Train", ], m)
    te_out <- flag_outliers(rdf[rdf$Set == "Test",  ], m)
    tab    <- rbind(tr_out, te_out)
    if (nrow(tab) == 0) {
      return(data.frame(Message = "No residual outliers at this multiplier."))
    }
    tab$residual <- round(tab$residual, 3)
    tab[order(-abs(tab$residual)), c("CODE", "Set", "residual")]
  })
  
} # end server