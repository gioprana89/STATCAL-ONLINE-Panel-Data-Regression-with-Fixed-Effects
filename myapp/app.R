# ============================================================
# STATCAL ONLINE - Panel Data Fixed Effect Regression Analyzer
# R Shiny Version
# ============================================================
# Required packages:
# install.packages(c(
#   "shiny", "shinydashboard", "DT", "readxl", "dplyr", "plm",
#   "lmtest", "sandwich", "openxlsx", "ggplot2", "shinycssloaders"
# ))

required_packages <- c(
  "shiny", "shinydashboard", "DT", "readxl", "dplyr", "plm",
  "lmtest", "sandwich", "openxlsx", "ggplot2", "shinycssloaders"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Please install the following R packages first: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(openxlsx)
library(ggplot2)
library(shinycssloaders)
library(scales)

# ============================================================
# CONSTANTS
# ============================================================

APP_NAME <- "STATCAL ONLINE"
APP_TITLE <- "STATCAL ONLINE Panel Data Regression with Fixed Effects"
APP_UPDATED <- "Last updated on July 2, 2026"
WEBSITE_URL <- "https://statcal.com/"
STATCAL_ONLINE_URL <- "https://statcal.com/statcal%20online.html"
TRAINING_DATA_URL <- "https://drive.google.com/drive/folders/1s273Ad5FUElhzd5G16jWSBxbOtforzRR?usp=sharing"

# The app will use uploaded Excel data first. If no upload is provided,
# it will try these local sample data names.
SAMPLE_DATA_CANDIDATES <- c("dataset.xlsx", "dataset(1).xlsx", "data keuangan.xlsx")

DEFAULT_COLORS <- c(
  "#1F4E79", "#B2182B", "#1B7837", "#762A83", "#8C510A",
  "#F46D43", "#2F9EA9", "#4D4D4D", "#A6CEE3", "#FB9A99"
)

# ============================================================
# HELPER FUNCTIONS
# ============================================================

clean_dataframe <- function(df) {
  names(df) <- trimws(gsub("\\s+", " ", as.character(names(df))))
  df <- df[rowSums(is.na(df)) < ncol(df), , drop = FALSE]
  unnamed_cols <- grepl("^unnamed", tolower(names(df)))
  if (any(unnamed_cols)) {
    keep_unnamed <- vapply(df[unnamed_cols], function(x) !all(is.na(x)), logical(1))
    drop_names <- names(df)[unnamed_cols][!keep_unnamed]
    if (length(drop_names) > 0) df <- df[, !names(df) %in% drop_names, drop = FALSE]
  }
  rownames(df) <- NULL
  as.data.frame(df)
}

make_display_safe <- function(df) {
  df <- as.data.frame(df)
  for (nm in names(df)) {
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  }
  df
}

to_numeric_vector <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  txt <- as.character(x)
  txt <- gsub(",", "", txt, fixed = TRUE)
  txt <- gsub("−", "-", txt, fixed = TRUE)
  txt <- gsub("—", "", txt, fixed = TRUE)
  txt <- trimws(txt)
  txt[txt %in% c("", "nan", "NaN", "None", "NaT", "-", "N/A", "NA", "n/a", "na")] <- NA
  convert_one <- function(v) {
    if (is.na(v) || v == "") return(NA_real_)
    negative <- FALSE
    if (grepl("^\\(.*\\)$", v)) {
      negative <- TRUE
      v <- sub("^\\(", "", sub("\\)$", "", v))
    }
    multiplier <- 1
    last_char <- tolower(substr(v, nchar(v), nchar(v)))
    if (last_char == "k") {
      multiplier <- 1000
      v <- substr(v, 1, nchar(v) - 1)
    } else if (last_char == "m") {
      multiplier <- 1000000
      v <- substr(v, 1, nchar(v) - 1)
    } else if (last_char == "b") {
      multiplier <- 1000000000
      v <- substr(v, 1, nchar(v) - 1)
    } else if (last_char == "t") {
      multiplier <- 1000000000000
      v <- substr(v, 1, nchar(v) - 1)
    }
    v <- trimws(gsub("%", "", v, fixed = TRUE))
    out <- suppressWarnings(as.numeric(v))
    if (is.na(out)) return(NA_real_)
    out <- out * multiplier
    if (negative) out <- -out
    out
  }
  vapply(txt, convert_one, numeric(1))
}

detect_numeric_columns <- function(df, min_valid_ratio = 0.45) {
  numeric_cols <- character(0)
  for (nm in names(df)) {
    non_null <- sum(!is.na(df[[nm]]))
    if (non_null == 0) next
    numeric_v <- to_numeric_vector(df[[nm]])
    valid_ratio <- sum(!is.na(numeric_v)) / max(non_null, 1)
    if (valid_ratio >= min_valid_ratio) numeric_cols <- c(numeric_cols, nm)
  }
  numeric_cols
}

sorted_unique_values <- function(x) {
  vals <- unique(x[!is.na(x)])
  vals[order(as.character(vals))]
}

round_numeric_df <- function(df, digits = 4) {
  df <- as.data.frame(df)
  numeric_cols <- vapply(df, is.numeric, logical(1))
  df[numeric_cols] <- lapply(df[numeric_cols], round, digits = digits)
  df
}

find_first_existing <- function(candidates, choices) {
  found <- candidates[candidates %in% choices]
  if (length(found) > 0) found[1] else if (length(choices) > 0) choices[1] else NULL
}

format_p_value <- function(p) {
  ifelse(is.na(p), NA_character_, ifelse(p < 0.001, "< 0.001", sprintf("%.4f", p)))
}

significance_label <- function(p, alpha = 0.05) {
  ifelse(is.na(p), "tidak dapat ditentukan", ifelse(p < alpha, "signifikan", "tidak signifikan"))
}

coefficient_direction <- function(x) {
  ifelse(is.na(x), "tidak dapat ditentukan", ifelse(x > 0, "positif", ifelse(x < 0, "negatif", "nol")))
}

compact_number <- function(x, digits = 2) {
  out <- ifelse(is.na(x), NA_character_, as.character(round(x, digits)))
  absx <- abs(x)
  out <- ifelse(!is.na(x) & absx >= 1e12, paste0(round(x / 1e12, digits), "T"), out)
  out <- ifelse(!is.na(x) & absx >= 1e9 & absx < 1e12, paste0(round(x / 1e9, digits), "B"), out)
  out <- ifelse(!is.na(x) & absx >= 1e6 & absx < 1e9, paste0(round(x / 1e6, digits), "M"), out)
  out <- ifelse(!is.na(x) & absx >= 1e3 & absx < 1e6, paste0(round(x / 1e3, digits), "K"), out)
  out
}

effect_spec_settings <- function(cross_section_effect = "fixed", period_effect = "none") {
  cross_section_fixed <- identical(cross_section_effect, "fixed")
  period_fixed <- identical(period_effect, "fixed")
  
  if (cross_section_fixed && period_fixed) {
    model_type <- "Fixed Effect Model"
    plm_model <- "within"
    plm_effect <- "twoways"
    label <- "Cross-section: Fixed; Period: Fixed"
    short_label <- "Two-way Fixed Effects"
  } else if (cross_section_fixed && !period_fixed) {
    model_type <- "Fixed Effect Model"
    plm_model <- "within"
    plm_effect <- "individual"
    label <- "Cross-section: Fixed; Period: None"
    short_label <- "Cross-section Fixed Effects"
  } else if (!cross_section_fixed && period_fixed) {
    model_type <- "Fixed Effect Model"
    plm_model <- "within"
    plm_effect <- "time"
    label <- "Cross-section: None; Period: Fixed"
    short_label <- "Period Fixed Effects"
  } else {
    model_type <- "Pooled OLS"
    plm_model <- "pooling"
    plm_effect <- "none"
    label <- "Cross-section: None; Period: None"
    short_label <- "Pooled OLS / No Fixed Effects"
  }
  
  list(
    cross_section_fixed = cross_section_fixed,
    period_fixed = period_fixed,
    model_type = model_type,
    plm_model = plm_model,
    plm_effect = plm_effect,
    label = label,
    short_label = short_label
  )
}

effect_spec_rhs_text <- function(effect_spec, entity_col, time_col) {
  if (effect_spec$cross_section_fixed && effect_spec$period_fixed) {
    paste0("cross-section fixed effects based on ", entity_col, " and period fixed effects based on ", time_col)
  } else if (effect_spec$cross_section_fixed) {
    paste0("cross-section fixed effects based on ", entity_col)
  } else if (effect_spec$period_fixed) {
    paste0("period fixed effects based on ", time_col)
  } else {
    "no fixed effects"
  }
}

effect_spec_formula_symbols <- function(effect_spec) {
  if (effect_spec$cross_section_fixed && effect_spec$period_fixed) {
    list(prefix = "C + &alpha;<sub>i</sub> + &lambda;<sub>t</sub> + ")
  } else if (effect_spec$cross_section_fixed) {
    list(prefix = "C + &alpha;<sub>i</sub> + ")
  } else if (effect_spec$period_fixed) {
    list(prefix = "C + &lambda;<sub>t</sub> + ")
  } else {
    list(prefix = "C + ")
  }
}


# ============================================================
# MODEL FUNCTIONS
# ============================================================

build_panel_model_data <- function(df, entity_col, time_col, dependent_col, independent_cols, label_col = "None") {
  validate(need(entity_col %in% names(df), "Please select a valid entity variable."))
  validate(need(time_col %in% names(df), "Please select a valid time variable."))
  validate(need(dependent_col %in% names(df), "Please select a valid dependent variable."))
  validate(need(length(independent_cols) > 0, "Please select at least one independent variable."))
  validate(need(all(independent_cols %in% names(df)), "Please select valid independent variables."))
  
  model_df <- data.frame(
    .Entity = as.character(df[[entity_col]]),
    .Time = as.character(df[[time_col]]),
    .Y = to_numeric_vector(df[[dependent_col]]),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(independent_cols)) {
    model_df[[paste0(".X", i)]] <- to_numeric_vector(df[[independent_cols[i]]])
  }
  
  model_df$.Entity[trimws(model_df$.Entity) == ""] <- NA
  model_df$.Time[trimws(model_df$.Time) == ""] <- NA
  model_df <- model_df[complete.cases(model_df), , drop = FALSE]
  
  if (!is.null(label_col) && label_col != "None" && label_col %in% names(df)) {
    label_df <- data.frame(
      .Entity = as.character(df[[entity_col]]),
      Entity_Label = as.character(df[[label_col]]),
      stringsAsFactors = FALSE
    )
    label_df <- label_df[!is.na(label_df$.Entity) & trimws(label_df$.Entity) != "", , drop = FALSE]
    label_df <- label_df[!is.na(label_df$Entity_Label) & trimws(label_df$Entity_Label) != "", , drop = FALSE]
    label_df <- label_df[!duplicated(label_df$.Entity), , drop = FALSE]
  } else {
    label_df <- data.frame(.Entity = unique(model_df$.Entity), Entity_Label = unique(model_df$.Entity), stringsAsFactors = FALSE)
  }
  
  variable_map <- data.frame(
    Model_Variable = c(".Y", paste0(".X", seq_along(independent_cols))),
    Original_Variable = c(dependent_col, independent_cols),
    Role = c("Dependent variable", rep("Independent variable", length(independent_cols))),
    stringsAsFactors = FALSE
  )
  
  list(data = model_df, labels = label_df, variable_map = variable_map)
}

panel_structure_table <- function(model_df) {
  tab <- table(model_df$.Entity, model_df$.Time)
  balanced <- length(unique(as.vector(tab))) == 1 && all(tab > 0)
  data.frame(
    Item = c(
      "Number of entities",
      "Number of time periods",
      "Number of complete observations used",
      "Panel type",
      "Minimum observations per entity",
      "Maximum observations per entity"
    ),
    Value = c(
      length(unique(model_df$.Entity)),
      length(unique(model_df$.Time)),
      nrow(model_df),
      ifelse(balanced, "Balanced panel", "Unbalanced panel"),
      min(rowSums(tab)),
      max(rowSums(tab))
    ),
    stringsAsFactors = FALSE
  )
}

run_fixed_effect_model <- function(model_df, independent_cols_count,
                                   cross_section_effect = "fixed",
                                   period_effect = "none") {
  validate(need(nrow(model_df) > 0, "No complete observations are available for the selected variables."))
  
  effect_spec <- effect_spec_settings(cross_section_effect, period_effect)
  
  if (effect_spec$cross_section_fixed) {
    validate(need(length(unique(model_df$.Entity)) >= 2, "Cross-section fixed effects require at least two entities."))
  }
  if (effect_spec$period_fixed) {
    validate(need(length(unique(model_df$.Time)) >= 2, "Period fixed effects require at least two time periods."))
  }
  validate(need(length(unique(model_df$.Entity)) >= 1, "At least one entity is required."))
  validate(need(length(unique(model_df$.Time)) >= 1, "At least one time period is required."))
  
  rhs <- paste(paste0(".X", seq_len(independent_cols_count)), collapse = " + ")
  fml <- as.formula(paste(".Y ~", rhs))
  
  pdata <- pdata.frame(model_df, index = c(".Entity", ".Time"), drop.index = FALSE, row.names = TRUE)
  
  main_model <- if (effect_spec$plm_model == "pooling") {
    plm(
      formula = fml,
      data = pdata,
      model = "pooling"
    )
  } else {
    plm(
      formula = fml,
      data = pdata,
      model = "within",
      effect = effect_spec$plm_effect
    )
  }
  
  pooling <- plm(
    formula = fml,
    data = pdata,
    model = "pooling"
  )
  
  # LSDV model with sum-to-zero contrasts.
  # This model is used to display the common constant C and fixed-effect intercept components,
  # similar to the Effect Specification output style in EViews.
  lsdv_data <- as.data.frame(model_df)
  lsdv_data$.Entity <- factor(lsdv_data$.Entity)
  lsdv_data$.Time <- factor(lsdv_data$.Time)
  
  if (effect_spec$cross_section_fixed && nlevels(lsdv_data$.Entity) >= 2) {
    contrasts(lsdv_data$.Entity) <- contr.sum(nlevels(lsdv_data$.Entity))
  }
  if (effect_spec$period_fixed && nlevels(lsdv_data$.Time) >= 2) {
    contrasts(lsdv_data$.Time) <- contr.sum(nlevels(lsdv_data$.Time))
  }
  
  lsdv_fixed_terms <- character(0)
  if (effect_spec$cross_section_fixed) lsdv_fixed_terms <- c(lsdv_fixed_terms, ".Entity")
  if (effect_spec$period_fixed) lsdv_fixed_terms <- c(lsdv_fixed_terms, ".Time")
  
  rhs_lsdv <- rhs
  if (length(lsdv_fixed_terms) > 0) {
    rhs_lsdv <- paste(rhs_lsdv, paste(lsdv_fixed_terms, collapse = " + "), sep = " + ")
  }
  
  fml_lsdv <- as.formula(paste(".Y ~", rhs_lsdv))
  
  lsdv_common <- tryCatch({
    lm(fml_lsdv, data = lsdv_data)
  }, error = function(e) NULL)
  
  list(
    fem = main_model,
    pooling = pooling,
    pdata = pdata,
    formula = fml,
    lsdv_common = lsdv_common,
    lsdv_data = lsdv_data,
    formula_lsdv = fml_lsdv,
    effect_spec = effect_spec
  )
}


make_coefficient_table <- function(model, variable_map, digits = 4) {
  smry <- summary(model)
  co <- as.data.frame(smry$coefficients)
  if (nrow(co) == 0) return(data.frame())
  co$Model_Variable <- rownames(co)
  names(co) <- gsub("Pr\\(>\\|t\\|\\)", "P_Value", names(co))
  names(co) <- gsub("Std\\. Error", "Std_Error", names(co))
  names(co) <- gsub("t-value", "t_Statistic", names(co))
  names(co) <- gsub("Estimate", "Coefficient", names(co))
  out <- co %>%
    left_join(variable_map, by = "Model_Variable") %>%
    mutate(
      Original_Variable = ifelse(
        Model_Variable == "(Intercept)",
        "C",
        ifelse(is.na(Original_Variable) | Original_Variable == "", Model_Variable, Original_Variable)
      ),
      Model_Variable = ifelse(Model_Variable == "(Intercept)", "C", Model_Variable)
    ) %>%
    select(Original_Variable, Model_Variable, Coefficient, Std_Error, t_Statistic, P_Value)
  out$Significance_5_Percent <- significance_label(out$P_Value, 0.05)
  out$Direction <- coefficient_direction(out$Coefficient)
  round_numeric_df(out, digits)
}


make_robust_table <- function(model, variable_map, vcov_type = "HC1", cluster_type = "group", digits = 4) {
  # This version computes robust standard error manually from vcovHC.
  # It is safer when coeftest returns a structure with unexpected column dimensions.
  
  coef_vec <- tryCatch({
    stats::coef(model)
  }, error = function(e) NULL)
  
  vcov_mat <- tryCatch({
    plm::vcovHC(model, type = vcov_type, cluster = cluster_type)
  }, error = function(e) NULL)
  
  if (is.null(coef_vec) || is.null(vcov_mat)) {
    return(data.frame(Message = "Robust standard error could not be computed."))
  }
  
  robust_se <- tryCatch({
    sqrt(diag(vcov_mat))
  }, error = function(e) NULL)
  
  if (is.null(robust_se) || length(robust_se) == 0) {
    return(data.frame(Message = "Robust standard error could not be computed."))
  }
  
  if (is.null(names(robust_se)) || !all(names(coef_vec) %in% names(robust_se))) {
    robust_se <- robust_se[seq_along(coef_vec)]
    names(robust_se) <- names(coef_vec)
  }
  
  robust_se <- robust_se[names(coef_vec)]
  t_stat <- as.numeric(coef_vec) / as.numeric(robust_se)
  
  df_resid <- tryCatch({
    stats::df.residual(model)
  }, error = function(e) NA_real_)
  
  p_value <- if (!is.na(df_resid) && is.finite(df_resid) && df_resid > 0) {
    2 * stats::pt(abs(t_stat), df = df_resid, lower.tail = FALSE)
  } else {
    2 * stats::pnorm(abs(t_stat), lower.tail = FALSE)
  }
  
  out <- data.frame(
    Model_Variable = names(coef_vec),
    Coefficient = as.numeric(coef_vec),
    Robust_Std_Error = as.numeric(robust_se),
    t_Statistic = as.numeric(t_stat),
    P_Value = as.numeric(p_value),
    stringsAsFactors = FALSE
  )
  
  out <- out %>%
    left_join(variable_map, by = "Model_Variable") %>%
    mutate(
      Original_Variable = ifelse(
        Model_Variable == "(Intercept)",
        "C",
        ifelse(is.na(Original_Variable) | Original_Variable == "", Model_Variable, Original_Variable)
      ),
      Model_Variable = ifelse(Model_Variable == "(Intercept)", "C", Model_Variable)
    ) %>%
    select(Original_Variable, Model_Variable, Coefficient, Robust_Std_Error, t_Statistic, P_Value)
  
  out$Significance_5_Percent <- significance_label(out$P_Value, 0.05)
  out$Direction <- coefficient_direction(out$Coefficient)
  
  round_numeric_df(out, digits)
}



make_constant_c_table <- function(lsdv_common, lsdv_data, effect_spec = effect_spec_settings(),
                                  vcov_type = "HC1", cluster_type = "group", digits = 4) {
  # This function displays the EViews-style common constant C.
  # In a within estimator from plm, C is not printed directly.
  # With LSDV and sum-to-zero contrasts, C is the common intercept after applying
  # the selected effect specification: cross-section fixed, period fixed, or two-way fixed.
  
  if (is.null(lsdv_common)) {
    return(data.frame(Message = "Constant C could not be computed."))
  }
  
  smry <- tryCatch({
    summary(lsdv_common)$coefficients
  }, error = function(e) NULL)
  
  if (is.null(smry) || !("(Intercept)" %in% rownames(smry))) {
    return(data.frame(Message = "Constant C could not be computed."))
  }
  
  standard_coef <- smry["(Intercept)", , drop = FALSE]
  standard_estimate <- as.numeric(standard_coef[1, "Estimate"])
  standard_se <- as.numeric(standard_coef[1, "Std. Error"])
  standard_t <- as.numeric(standard_coef[1, "t value"])
  standard_p <- as.numeric(standard_coef[1, "Pr(>|t|)"])
  
  robust_se <- NA_real_
  robust_t <- NA_real_
  robust_p <- NA_real_
  
  robust_type_for_lm <- ifelse(vcov_type %in% c("HC0", "HC1", "HC2", "HC3"), vcov_type, "HC1")
  
  robust_vcov <- tryCatch({
    cluster_vec <- if (!is.null(cluster_type) && cluster_type == "time") {
      lsdv_data$.Time
    } else {
      lsdv_data$.Entity
    }
    sandwich::vcovCL(lsdv_common, cluster = cluster_vec, type = robust_type_for_lm)
  }, error = function(e) NULL)
  
  if (!is.null(robust_vcov) && "(Intercept)" %in% rownames(robust_vcov)) {
    robust_se <- sqrt(diag(robust_vcov))["(Intercept)"]
    robust_t <- standard_estimate / robust_se
    
    df_resid <- tryCatch({
      stats::df.residual(lsdv_common)
    }, error = function(e) NA_real_)
    
    robust_p <- if (!is.na(df_resid) && is.finite(df_resid) && df_resid > 0) {
      2 * stats::pt(abs(robust_t), df = df_resid, lower.tail = FALSE)
    } else {
      2 * stats::pnorm(abs(robust_t), lower.tail = FALSE)
    }
  }
  
  description <- if (effect_spec$cross_section_fixed && effect_spec$period_fixed) {
    "Common constant with cross-section and period fixed effects"
  } else if (effect_spec$cross_section_fixed) {
    "Common constant / average of cross-section fixed-effect intercepts"
  } else if (effect_spec$period_fixed) {
    "Common constant / average of period fixed-effect intercepts"
  } else {
    "Pooled OLS constant"
  }
  
  out <- data.frame(
    Variable = "C",
    Effect_Specification = effect_spec$label,
    Description = description,
    Coefficient = standard_estimate,
    Std_Error = standard_se,
    t_Statistic = standard_t,
    P_Value = standard_p,
    Robust_Std_Error = as.numeric(robust_se),
    Robust_t_Statistic = as.numeric(robust_t),
    Robust_P_Value = as.numeric(robust_p),
    Significance_5_Percent = significance_label(standard_p, 0.05),
    Robust_Significance_5_Percent = significance_label(robust_p, 0.05),
    Direction = coefficient_direction(standard_estimate),
    stringsAsFactors = FALSE
  )
  
  round_numeric_df(out, digits)
}


make_intercept_fe_table <- function(model, label_df, effect_spec = effect_spec_settings(), digits = 4) {
  rows <- list()
  
  if (effect_spec$cross_section_fixed) {
    fe_entity <- tryCatch({
      fixef(model, effect = "individual", type = "level")
    }, error = function(e) NULL)
    
    if (!is.null(fe_entity)) {
      entity_df <- data.frame(
        Effect_Type = "Cross-section fixed effect",
        Effect_ID = names(fe_entity),
        Intercept_Fixed_Effect = as.numeric(fe_entity),
        stringsAsFactors = FALSE
      )
      entity_df <- entity_df %>%
        left_join(label_df, by = c("Effect_ID" = ".Entity")) %>%
        mutate(Effect_Label = ifelse(is.na(Entity_Label) | Entity_Label == "", Effect_ID, Entity_Label)) %>%
        select(Effect_Type, Effect_ID, Effect_Label, Intercept_Fixed_Effect)
      rows[[length(rows) + 1]] <- entity_df
    }
  }
  
  if (effect_spec$period_fixed) {
    fe_time <- tryCatch({
      fixef(model, effect = "time", type = "level")
    }, error = function(e) NULL)
    
    if (!is.null(fe_time)) {
      time_df <- data.frame(
        Effect_Type = "Period fixed effect",
        Effect_ID = names(fe_time),
        Effect_Label = names(fe_time),
        Intercept_Fixed_Effect = as.numeric(fe_time),
        stringsAsFactors = FALSE
      )
      rows[[length(rows) + 1]] <- time_df
    }
  }
  
  if (length(rows) == 0) {
    return(data.frame(Message = "No fixed effect intercept is available because Cross-section and Period are both set to None."))
  }
  
  out <- bind_rows(rows) %>%
    arrange(Effect_Type, desc(Intercept_Fixed_Effect))
  
  out$Rank <- ave(out$Intercept_Fixed_Effect, out$Effect_Type, FUN = function(x) rank(-x, ties.method = "first"))
  out <- out %>% select(Rank, Effect_Type, Effect_ID, Effect_Label, Intercept_Fixed_Effect)
  round_numeric_df(out, digits)
}


make_model_fit_table <- function(fem, pooling, effect_spec = effect_spec_settings()) {
  smry <- summary(fem)
  r2 <- tryCatch(as.numeric(smry$r.squared["rsq"]), error = function(e) NA_real_)
  adj_r2 <- tryCatch(as.numeric(smry$r.squared["adjrsq"]), error = function(e) NA_real_)
  ftest <- if (effect_spec$plm_model == "pooling") {
    NULL
  } else {
    tryCatch(pFtest(fem, pooling), error = function(e) NULL)
  }
  
  data.frame(
    Item = c(
      "Model type",
      "Effect specification",
      "Cross-section effect",
      "Period effect",
      "Within R-squared",
      "Adjusted R-squared",
      "F-test for fixed effects: F statistic",
      "F-test for fixed effects: p-value"
    ),
    Value = c(
      effect_spec$model_type,
      effect_spec$label,
      ifelse(effect_spec$cross_section_fixed, "Fixed", "None"),
      ifelse(effect_spec$period_fixed, "Fixed", "None"),
      round(r2, 6),
      round(adj_r2, 6),
      ifelse(is.null(ftest), NA, round(as.numeric(ftest$statistic), 6)),
      ifelse(is.null(ftest), NA, format_p_value(as.numeric(ftest$p.value)))
    ),
    stringsAsFactors = FALSE
  )
}


make_model_equation_html <- function(dependent_col, independent_cols, entity_col, time_col,
                                     effect_spec = effect_spec_settings()) {
  beta_terms <- paste0("&beta;<sub>", seq_along(independent_cols), "</sub>", "(", independent_cols, ")<sub>it</sub>", collapse = " + ")
  symbols <- effect_spec_formula_symbols(effect_spec)
  
  fixed_explanation <- if (effect_spec$cross_section_fixed && effect_spec$period_fixed) {
    paste0(
      "&alpha;<sub>i</sub> adalah cross-section fixed effect untuk setiap <b>", entity_col, "</b>, ",
      "sedangkan &lambda;<sub>t</sub> adalah period fixed effect untuk setiap <b>", time_col, "</b>."
    )
  } else if (effect_spec$cross_section_fixed) {
    paste0("&alpha;<sub>i</sub> adalah cross-section fixed effect untuk setiap <b>", entity_col, "</b>.")
  } else if (effect_spec$period_fixed) {
    paste0("&lambda;<sub>t</sub> adalah period fixed effect untuk setiap <b>", time_col, "</b>.")
  } else {
    "Model tidak menggunakan fixed effect, sehingga model yang diestimasi setara dengan pooled OLS."
  }
  
  HTML(paste0(
    "<p><b>Effect specification:</b> ", effect_spec$label, "</p>",
    "<p><b>Model yang diestimasi:</b></p>",
    "<p style='font-size:18px;'>",
    dependent_col, "<sub>it</sub> = ", symbols$prefix, beta_terms, " + &epsilon;<sub>it</sub>",
    "</p>",
    "<p>",
    "C adalah common constant. ", fixed_explanation,
    " Indeks <i>i</i> menunjukkan entity/emiten, sedangkan indeks <i>t</i> menunjukkan waktu.",
    "</p>"
  ))
}


make_auto_interpretation <- function(coef_table, intercept_table, dep_col, indep_cols, entity_col, time_col,
                                     effect_spec = effect_spec_settings()) {
  if (nrow(coef_table) == 0 || "Message" %in% names(coef_table)) {
    return("Model belum dapat diinterpretasikan karena tabel koefisien belum tersedia.")
  }
  
  coef_for_text <- coef_table[coef_table$Model_Variable != "C", , drop = FALSE]
  
  coef_text <- paste(vapply(seq_len(nrow(coef_for_text)), function(i) {
    row <- coef_for_text[i, ]
    paste0(
      "Variabel ", row$Original_Variable, " memiliki koefisien ", row$Direction,
      " sebesar ", row$Coefficient, ". Nilai p-value sebesar ", format_p_value(as.numeric(row$P_Value)),
      " sehingga variabel tersebut ", row$Significance_5_Percent,
      " terhadap ", dep_col, " pada taraf signifikansi 5%."
    )
  }, character(1)), collapse = " ")
  
  c_text <- ""
  if ("C" %in% coef_table$Model_Variable) {
    c_row <- coef_table[coef_table$Model_Variable == "C", ][1, ]
    c_text <- paste0(
      " Nilai konstanta C sebesar ", c_row$Coefficient,
      " menunjukkan common constant dalam model dengan effect specification: ", effect_spec$label, "."
    )
  }
  
  intercept_text <- ""
  if (nrow(intercept_table) > 0 && !("Message" %in% names(intercept_table))) {
    effect_types <- unique(intercept_table$Effect_Type)
    parts <- vapply(effect_types, function(et) {
      sub <- intercept_table[intercept_table$Effect_Type == et, , drop = FALSE]
      top_row <- sub[which.max(sub$Intercept_Fixed_Effect), ]
      low_row <- sub[which.min(sub$Intercept_Fixed_Effect), ]
      paste0(
        " Pada ", et, ", nilai tertinggi terdapat pada ", top_row$Effect_ID,
        " sebesar ", top_row$Intercept_Fixed_Effect,
        ", sedangkan nilai terendah terdapat pada ", low_row$Effect_ID,
        " sebesar ", low_row$Intercept_Fixed_Effect, "."
      )
    }, character(1))
    
    intercept_text <- paste0(
      " Nilai fixed effect intercept menunjukkan adanya heterogenitas sesuai effect specification yang dipilih.",
      paste(parts, collapse = " "),
      " Perbedaan ini menunjukkan bahwa setiap cross-section dan/atau periode dapat memiliki posisi dasar ",
      dep_col, " yang berbeda setelah pengaruh ", paste(indep_cols, collapse = " dan "), " dikontrol."
    )
  }
  
  paste0(coef_text, c_text, intercept_text)
}


# ============================================================
# UI
# ============================================================

ui <- dashboardPage(
  dashboardHeader(title = APP_NAME, titleWidth = "100%"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(HTML("\n      .content-wrapper, .right-side { background-color: #f7f9fb; }\n      .box { border-radius: 10px; }\n      .statcal-title { font-size: 24px; font-weight: 700; color: #1F4E79; }\n      .statcal-subtitle { font-size: 18px; font-weight: 600; color: #333333; }\n      .statcal-note { line-height: 1.6; text-align: justify; }\n      .small-note { font-size: 12px; color: #666666; }\n      .formula-box { background: #ffffff; border-left: 5px solid #1F4E79; padding: 14px; border-radius: 8px; }\n    "))),
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = "STATCAL ONLINE Panel Data Regression with Fixed Effects",
          div(class = "statcal-title", APP_TITLE),
          div(class = "statcal-subtitle", APP_UPDATED),
          tags$p(class = "statcal-note",
                 "This R Shiny application is designed to estimate panel data regression models with flexible fixed effect specifications. Users can select cross-section effects, period effects, or two-way effects to account for unobserved heterogeneity across entities and/or time periods. The application also provides coefficient estimates, robust standard errors, fixed effect intercepts, model diagnostics, automatic interpretation, and exportable results."),
          tags$p(tags$b("Website: "), tags$a(href = WEBSITE_URL, target = "_blank", WEBSITE_URL), tags$br(),
                 tags$b("STATCAL ONLINE Page: "), tags$a(href = STATCAL_ONLINE_URL, target = "_blank", STATCAL_ONLINE_URL), tags$br(),
                 tags$b("Training Data: "), tags$a(href = TRAINING_DATA_URL, target = "_blank", "Open Google Drive Folder"))
      )
    ),
    tabsetPanel(
      id = "main_tabs",
      tabPanel("1. Data & Variables",
               br(),
               fluidRow(
                 box(width = 4, title = "Data Input", status = "primary", solidHeader = TRUE,
                     fileInput("uploaded_file", "Upload Excel file", accept = c(".xlsx", ".xls")),
                     uiOutput("sheet_ui"),
                     tags$p(class = "small-note", "If no file is uploaded, the app will try to read a local sample dataset.")),
                 box(width = 4, title = "Panel Structure", status = "primary", solidHeader = TRUE,
                     uiOutput("entity_ui"),
                     uiOutput("time_ui"),
                     uiOutput("label_ui")),
                 box(width = 4, title = "Regression Variables", status = "primary", solidHeader = TRUE,
                     uiOutput("dependent_ui"),
                     uiOutput("independent_ui"),
                     sliderInput("digits", "Decimal digits", min = 0, max = 8, value = 4, step = 1))
               ),
               fluidRow(
                 box(width = 12, title = "Filters", status = "info", solidHeader = TRUE, uiOutput("filter_ui"))
               ),
               fluidRow(
                 valueBoxOutput("metric_original_rows", width = 3),
                 valueBoxOutput("metric_filtered_rows", width = 3),
                 valueBoxOutput("metric_entities", width = 3),
                 valueBoxOutput("metric_times", width = 3)
               ),
               fluidRow(
                 box(width = 12, title = "Dataset Preview", status = "warning", solidHeader = TRUE,
                     shinycssloaders::withSpinner(DTOutput("data_preview")))
               ),
               fluidRow(
                 box(width = 12, title = "Detected Numeric Variables", status = "info", solidHeader = TRUE,
                     collapsible = TRUE, collapsed = TRUE, verbatimTextOutput("numeric_variables_text"))
               )
      ),
      tabPanel("2. Fixed Effect Model",
               br(),
               fluidRow(
                 box(width = 4, title = "Effect Specification and Model Settings", status = "primary", solidHeader = TRUE,
                     tags$p("Choose the fixed-effect structure similar to EViews Effect Specification."),
                     selectInput("cross_section_effect", "Cross-section", choices = c("None" = "none", "Fixed" = "fixed"), selected = "fixed"),
                     selectInput("period_effect", "Period", choices = c("None" = "none", "Fixed" = "fixed"), selected = "none"),
                     tags$hr(),
                     selectInput("robust_type", "Robust covariance type", choices = c("HC0", "HC1", "HC2", "HC3", "HC4"), selected = "HC1"),
                     selectInput("cluster_type", "Robust SE cluster", choices = c("group", "time"), selected = "group"),
                     checkboxInput("show_standard_summary", "Show raw plm summary", TRUE)),
                 box(width = 8, title = "Model Equation", status = "primary", solidHeader = TRUE,
                     div(class = "formula-box", uiOutput("model_equation")))
               ),
               fluidRow(
                 box(width = 12, title = "Panel Data Structure", status = "info", solidHeader = TRUE,
                     shinycssloaders::withSpinner(DTOutput("panel_structure_table")))
               ),
               fluidRow(
                 box(width = 6, title = "FEM Coefficient Table", status = "warning", solidHeader = TRUE,
                     shinycssloaders::withSpinner(DTOutput("coef_table"))),
                 box(width = 6, title = "FEM Coefficient Table with Robust Standard Error", status = "warning", solidHeader = TRUE,
                     shinycssloaders::withSpinner(DTOutput("robust_table")))
               ),
               fluidRow(
                 box(width = 12, title = "Common Constant C / EViews-style Intercept", status = "info", solidHeader = TRUE,
                     tags$p("This table displays the common constant C. In the plm within estimator, C is not printed directly. Here, C is computed from the equivalent LSDV model with sum-to-zero contrasts according to the selected Effect Specification."),
                     shinycssloaders::withSpinner(DTOutput("constant_c_table")))
               ),
               fluidRow(
                 box(width = 12, title = "Model Fit and Fixed Effect Test", status = "success", solidHeader = TRUE,
                     shinycssloaders::withSpinner(DTOutput("model_fit_table")))
               ),
               fluidRow(
                 box(width = 12, title = "Raw plm Summary", status = "info", solidHeader = TRUE,
                     collapsible = TRUE, collapsed = TRUE,
                     conditionalPanel(condition = "input.show_standard_summary == true", verbatimTextOutput("raw_summary")))
               )
      ),
      tabPanel("3. Intercept Fixed Effect",
               br(),
               fluidRow(
                 box(width = 12, title = "Fixed Effect Intercepts", status = "primary", solidHeader = TRUE,
                     tags$p("This table shows the selected fixed-effect intercepts based on the effect specification. If Cross-section is Fixed, entity-specific intercepts are displayed. If Period is Fixed, period-specific intercepts are displayed."),
                     shinycssloaders::withSpinner(DTOutput("intercept_table")))
               ),
               fluidRow(
                 box(width = 12, title = "Fixed Effect Intercepts Plot", status = "warning", solidHeader = TRUE,
                     shinycssloaders::withSpinner(plotOutput("intercept_plot", height = "650px")))
               )
      ),
      tabPanel("4. Interpretation",
               br(),
               fluidRow(
                 box(width = 12, title = "Automatic Interpretation", status = "success", solidHeader = TRUE,
                     tags$p(class = "small-note", "The interpretation below is generated automatically based on the selected model and should be reviewed before being used in a thesis, article, or book."),
                     verbatimTextOutput("auto_interpretation"))
               ),
               fluidRow(
                 box(width = 12, title = "Suggested Report Wording", status = "primary", solidHeader = TRUE,
                     htmlOutput("report_wording"))
               )
      ),
      tabPanel("5. Export",
               br(),
               fluidRow(
                 box(width = 6, title = "Export Tables to Excel", status = "success", solidHeader = TRUE,
                     tags$p("The Excel workbook contains data preview, variable mapping, panel structure, FEM coefficients, robust standard error, model fit, and Intercept Fixed Effect."),
                     downloadButton("download_excel", "Download FEM Results Excel")),
                 box(width = 6, title = "Export Intercept Fixed Effect", status = "success", solidHeader = TRUE,
                     tags$p("Download only the Intercept Fixed Effect table as CSV."),
                     downloadButton("download_intercept_csv", "Download Intercept Fixed Effect CSV"))
               )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  current_excel_path <- reactive({
    if (!is.null(input$uploaded_file)) {
      input$uploaded_file$datapath
    } else {
      sample_path <- SAMPLE_DATA_CANDIDATES[file.exists(SAMPLE_DATA_CANDIDATES)]
      if (length(sample_path) > 0) sample_path[1] else NULL
    }
  })
  
  output$sheet_ui <- renderUI({
    path <- current_excel_path()
    if (is.null(path)) return(helpText("Please upload an Excel file to start the analysis."))
    sheets <- readxl::excel_sheets(path)
    selectInput("sheet_name", "Worksheet", choices = sheets, selected = sheets[1])
  })
  
  data_raw <- reactive({
    path <- current_excel_path()
    req(path)
    sheets <- readxl::excel_sheets(path)
    sheet <- input$sheet_name
    if (is.null(sheet) || !(sheet %in% sheets)) sheet <- sheets[1]
    clean_dataframe(readxl::read_excel(path, sheet = sheet))
  })
  
  numeric_columns <- reactive({
    detect_numeric_columns(data_raw())
  })
  
  output$entity_ui <- renderUI({
    df <- data_raw()
    selected <- find_first_existing(c("Kode Emiten", "Company", "Emiten", "Kode_Emiten", "Entity"), names(df))
    selectInput("entity_col", "Entity variable", choices = names(df), selected = selected)
  })
  
  output$time_ui <- renderUI({
    df <- data_raw()
    selected <- find_first_existing(c("Tahun Laporan", "Tahun", "Year", "Date", "Time"), names(df))
    selectInput("time_col", "Time variable", choices = names(df), selected = selected)
  })
  
  output$label_ui <- renderUI({
    df <- data_raw()
    selected <- find_first_existing(c("Nama Perusahan", "Nama Perusahaan", "Company Name", "Nama_Perusahaan"), c("None", names(df)))
    selectInput("label_col", "Entity label / company name", choices = c("None", names(df)), selected = selected)
  })
  
  output$dependent_ui <- renderUI({
    nums <- numeric_columns()
    selected <- find_first_existing(c("Earning per Share", "Earnings per Share", "EPS", "Earning per Share/EPS (Rp per Saham)"), nums)
    selectInput("dependent_col", "Dependent variable", choices = nums, selected = selected)
  })
  
  output$independent_ui <- renderUI({
    nums <- numeric_columns()
    if (!is.null(input$dependent_col) && input$dependent_col %in% nums) {
      nums <- setdiff(nums, input$dependent_col)
    }
    default_vars <- intersect(c("Return on Asset (ROA)", "Return on Assets/ROA (%)", "ROA", "Debt to Asset Ratio (DAR)", "DAR"), nums)
    if (length(default_vars) == 0 && length(nums) > 0) default_vars <- nums[seq_len(min(2, length(nums)))]
    selectizeInput("independent_cols", "Independent variables", choices = nums, selected = default_vars, multiple = TRUE)
  })
  
  output$filter_ui <- renderUI({
    df <- data_raw()
    req(input$entity_col, input$time_col)
    ui_list <- list()
    
    if (input$entity_col %in% names(df)) {
      entities <- sorted_unique_values(as.character(df[[input$entity_col]]))
      ui_list <- c(ui_list, list(column(6, selectizeInput("filter_entity", paste("Filter by", input$entity_col), choices = entities, selected = entities, multiple = TRUE))))
    }
    
    if (input$time_col %in% names(df)) {
      times <- sorted_unique_values(as.character(df[[input$time_col]]))
      ui_list <- c(ui_list, list(column(6, selectizeInput("filter_time", paste("Filter by", input$time_col), choices = times, selected = times, multiple = TRUE))))
    }
    
    if (length(ui_list) == 0) return(helpText("Select entity and time variables to activate filters."))
    do.call(fluidRow, ui_list)
  })
  
  filtered_data <- reactive({
    df <- data_raw()
    if (!is.null(input$entity_col) && input$entity_col %in% names(df) && !is.null(input$filter_entity) && length(input$filter_entity) > 0) {
      df <- df[as.character(df[[input$entity_col]]) %in% input$filter_entity, , drop = FALSE]
    }
    if (!is.null(input$time_col) && input$time_col %in% names(df) && !is.null(input$filter_time) && length(input$filter_time) > 0) {
      df <- df[as.character(df[[input$time_col]]) %in% input$filter_time, , drop = FALSE]
    }
    df
  })
  
  model_input <- reactive({
    req(input$entity_col, input$time_col, input$dependent_col, input$independent_cols)
    build_panel_model_data(
      filtered_data(),
      entity_col = input$entity_col,
      time_col = input$time_col,
      dependent_col = input$dependent_col,
      independent_cols = input$independent_cols,
      label_col = input$label_col
    )
  })
  
  model_result <- reactive({
    mi <- model_input()
    run_fixed_effect_model(
      mi$data,
      length(input$independent_cols),
      cross_section_effect = input$cross_section_effect,
      period_effect = input$period_effect
    )
  })
  
  coef_table_data <- reactive({
    mr <- model_result()
    mi <- model_input()
    coef_df <- make_coefficient_table(mr$fem, mi$variable_map, input$digits)
    c_df <- make_constant_c_table(
      mr$lsdv_common,
      mr$lsdv_data,
      effect_spec = mr$effect_spec,
      vcov_type = input$robust_type,
      cluster_type = input$cluster_type,
      digits = input$digits
    )
    
    if (nrow(c_df) > 0 && !("Message" %in% names(c_df)) && !("C" %in% coef_df$Model_Variable)) {
      c_row <- data.frame(
        Original_Variable = "C",
        Model_Variable = "C",
        Coefficient = c_df$Coefficient[1],
        Std_Error = c_df$Std_Error[1],
        t_Statistic = c_df$t_Statistic[1],
        P_Value = c_df$P_Value[1],
        Significance_5_Percent = c_df$Significance_5_Percent[1],
        Direction = c_df$Direction[1],
        stringsAsFactors = FALSE
      )
      coef_df <- bind_rows(coef_df, c_row)
    }
    
    coef_df
  })
  
  robust_table_data <- reactive({
    mr <- model_result()
    mi <- model_input()
    robust_df <- make_robust_table(mr$fem, mi$variable_map, input$robust_type, input$cluster_type, input$digits)
    c_df <- make_constant_c_table(
      mr$lsdv_common,
      mr$lsdv_data,
      effect_spec = mr$effect_spec,
      vcov_type = input$robust_type,
      cluster_type = input$cluster_type,
      digits = input$digits
    )
    
    if (nrow(c_df) > 0 && !("Message" %in% names(c_df)) && !("C" %in% robust_df$Model_Variable)) {
      c_row <- data.frame(
        Original_Variable = "C",
        Model_Variable = "C",
        Coefficient = c_df$Coefficient[1],
        Robust_Std_Error = c_df$Robust_Std_Error[1],
        t_Statistic = c_df$Robust_t_Statistic[1],
        P_Value = c_df$Robust_P_Value[1],
        Significance_5_Percent = c_df$Robust_Significance_5_Percent[1],
        Direction = c_df$Direction[1],
        stringsAsFactors = FALSE
      )
      robust_df <- bind_rows(robust_df, c_row)
    }
    
    robust_df
  })
  
  constant_c_table_data <- reactive({
    mr <- model_result()
    make_constant_c_table(
      mr$lsdv_common,
      mr$lsdv_data,
      effect_spec = mr$effect_spec,
      vcov_type = input$robust_type,
      cluster_type = input$cluster_type,
      digits = input$digits
    )
  })
  
  intercept_table_data <- reactive({
    mr <- model_result()
    mi <- model_input()
    make_intercept_fe_table(mr$fem, mi$labels, mr$effect_spec, input$digits)
  })
  
  model_fit_table_data <- reactive({
    mr <- model_result()
    make_model_fit_table(mr$fem, mr$pooling, mr$effect_spec)
  })
  
  panel_structure_data <- reactive({
    mi <- model_input()
    panel_structure_table(mi$data)
  })
  
  output$metric_original_rows <- renderValueBox({
    valueBox(nrow(data_raw()), "Original rows", icon = icon("table"), color = "blue")
  })
  
  output$metric_filtered_rows <- renderValueBox({
    valueBox(nrow(filtered_data()), "Rows after filtering", icon = icon("filter"), color = "green")
  })
  
  output$metric_entities <- renderValueBox({
    df <- filtered_data()
    val <- if (!is.null(input$entity_col) && input$entity_col %in% names(df)) length(unique(df[[input$entity_col]])) else 0
    valueBox(val, "Entities", icon = icon("building"), color = "yellow")
  })
  
  output$metric_times <- renderValueBox({
    df <- filtered_data()
    val <- if (!is.null(input$time_col) && input$time_col %in% names(df)) length(unique(df[[input$time_col]])) else 0
    valueBox(val, "Time periods", icon = icon("calendar"), color = "purple")
  })
  
  output$data_preview <- renderDT({
    DT::datatable(make_display_safe(filtered_data()), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$numeric_variables_text <- renderPrint({
    print(numeric_columns())
  })
  
  output$model_equation <- renderUI({
    req(input$dependent_col, input$independent_cols, input$entity_col, input$time_col)
    make_model_equation_html(
      input$dependent_col,
      input$independent_cols,
      input$entity_col,
      input$time_col,
      model_result()$effect_spec
    )
  })
  
  output$panel_structure_table <- renderDT({
    DT::datatable(make_display_safe(panel_structure_data()), options = list(dom = "t", scrollX = TRUE))
  })
  
  output$coef_table <- renderDT({
    DT::datatable(make_display_safe(coef_table_data()), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$robust_table <- renderDT({
    DT::datatable(make_display_safe(robust_table_data()), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$constant_c_table <- renderDT({
    DT::datatable(make_display_safe(constant_c_table_data()), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$model_fit_table <- renderDT({
    DT::datatable(make_display_safe(model_fit_table_data()), options = list(dom = "t", scrollX = TRUE))
  })
  
  output$raw_summary <- renderPrint({
    mr <- model_result()
    cat("Effect specification:", mr$effect_spec$label, "\n\n")
    print(summary(mr$fem))
    cat("\n\nCommon Constant C / EViews-style Intercept:\n")
    print(constant_c_table_data())
    cat("\n\nFixed Effect Intercepts:\n")
    print(intercept_table_data())
  })
  
  output$intercept_table <- renderDT({
    DT::datatable(make_display_safe(intercept_table_data()), options = list(scrollX = TRUE, pageLength = 15))
  })
  
  output$intercept_plot <- renderPlot({
    intercept_df <- intercept_table_data()
    validate(need(nrow(intercept_df) > 0 && !("Message" %in% names(intercept_df)), "Fixed effect intercept table is not available."))
    
    ggplot(intercept_df, aes(x = reorder(Effect_ID, Intercept_Fixed_Effect), y = Intercept_Fixed_Effect, fill = Effect_Type)) +
      geom_col(alpha = 0.90, show.legend = TRUE) +
      geom_text(aes(label = compact_number(Intercept_Fixed_Effect, 2)), hjust = -0.10, size = 4) +
      coord_flip() +
      facet_wrap(~ Effect_Type, scales = "free_y", ncol = 1) +
      labs(
        title = "Fixed Effect Intercepts",
        subtitle = paste("Effect specification:", model_result()$effect_spec$label),
        x = "Effect ID",
        y = "Fixed Effect Intercept",
        fill = "Effect Type"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
      ) +
      expand_limits(y = max(intercept_df$Intercept_Fixed_Effect, na.rm = TRUE) * 1.12)
  })
  
  
  auto_interpretation_text <- reactive({
    make_auto_interpretation(
      coef_table_data(),
      intercept_table_data(),
      dep_col = input$dependent_col,
      indep_cols = input$independent_cols,
      entity_col = input$entity_col,
      time_col = input$time_col,
      effect_spec = model_result()$effect_spec
    )
  })
  
  output$auto_interpretation <- renderText({
    auto_interpretation_text()
  })
  
  output$report_wording <- renderUI({
    req(input$dependent_col, input$independent_cols, input$entity_col)
    HTML(paste0(
      "<p>Berdasarkan effect specification yang dipilih, yaitu <b>", model_result()$effect_spec$label, "</b>, model diestimasi dengan ",
      "<b>", model_result()$effect_spec$short_label, "</b>.</p>",
      "<p>Jika Cross-section dipilih <b>Fixed</b>, model memberikan intercept berbeda untuk setiap <b>", input$entity_col, "</b>. ",
      "Jika Period dipilih <b>Fixed</b>, model memberikan intercept berbeda untuk setiap periode pada <b>", input$time_col, "</b>. ",
      "Jika keduanya dipilih <b>Fixed</b>, maka model mengontrol heterogenitas antar-emiten dan antar-periode secara bersamaan.</p>",
      "<p>Nilai fixed effect intercept menggambarkan posisi dasar <b>", input$dependent_col, "</b> sesuai struktur efek yang dipilih setelah pengaruh variabel ",
      paste(paste0("<b>", input$independent_cols, "</b>"), collapse = " dan "),
      " dikontrol.</p>"
    ))
  })
  
  output$download_intercept_csv <- downloadHandler(
    filename = function() {
      paste0("Intercept_Fixed_Effect_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(intercept_table_data(), file, row.names = FALSE)
    }
  )
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("STATCAL_Fixed_Effect_Model_Results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      
      add_sheet <- function(sheet_name, df) {
        openxlsx::addWorksheet(wb, sheet_name)
        df <- make_display_safe(as.data.frame(df))
        openxlsx::writeData(wb, sheet_name, df)
        if (ncol(df) > 0) {
          openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = "auto")
          header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9EAF7", border = "Bottom")
          openxlsx::addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
          openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
        }
      }
      
      mi <- model_input()
      mr <- model_result()
      
      add_sheet("Data Used", mi$data)
      add_sheet("Variable Map", mi$variable_map)
      add_sheet("Panel Structure", panel_structure_data())
      add_sheet("FEM Coefficients", coef_table_data())
      add_sheet("Robust SE", robust_table_data())
      add_sheet("Constant C", constant_c_table_data())
      add_sheet("Model Fit", model_fit_table_data())
      add_sheet("Intercept FE", intercept_table_data())
      add_sheet("Interpretation", data.frame(Interpretation = auto_interpretation_text(), stringsAsFactors = FALSE))
      
      summary_lines <- capture.output(summary(mr$fem))
      add_sheet("Raw plm Summary", data.frame(Output = summary_lines, stringsAsFactors = FALSE))
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)
