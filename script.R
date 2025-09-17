options(warn = -1)
# Kiểm tra, cài đặt và load các package cần thiết

packages <- c(
  "MSwM",
  "forecast",
  "readxl",
  "tseries",
  "dplyr",
  "gridExtra",
  "ggplot2",
  "data.table",
  "zoo",
  "tidyverse",
  "moments",
  "rlang",
  "rugarch",
  "FinTS",
  "RColorBrewer",
  "tvReg",
  "reshape2",
  "quantreg",
  "lmtest",
  "nonlinearTseries",
  "scales"
)
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, quiet = TRUE)
  }
}
invisible(sapply(packages, install_if_missing))
invisible(sapply(packages, function(pkg) {
  suppressMessages(suppressWarnings(library(pkg, character.only = TRUE)))
}))

# =========================================================================

# Import và tiền xử lý data
df <- read_excel("~/KLTN/data.xlsx", sheet = "processed")

df <- df %>% arrange(DATE)

df$LNVOL <- log(df$VNINDEX_VOL)

df_clean <- df %>%
  arrange(DATE) %>%
  drop_na()

# =========================================================================

# Thống kê mô tả
desc_stats <- function(x) {
  c(
    Mean = mean(x),
    SD = sd(x),
    Min = min(x),
    Max = max(x),
    Skewness = skewness(x),
    Kurtosis = kurtosis(x),
    "JB p-value" = jarque.bera.test(x)$p.value
  )
}

desc_table <- sapply(df_clean[, c(
  "VNINDEX_R",
  "SP500_R",
  "USDVND_R",
  "BRENT_R",
  "LNVOL",
  "NFI_VOL"
)], desc_stats)
desc_table <- t(round(desc_table, 4))
desc_table

# Plot
vars <- c(
  "VNINDEX_R",
  "SP500_R",
  "USDVND_R",
  "BRENT_R",
  "LNVOL",
  "NFI_VOL"
)

## Line plot
p_list <- lapply(vars, function(v) {
  ggplot(df_clean, aes_string(x = "DATE", y = v)) +
    geom_line(color = "palegreen3") +
    labs(x = "Date", y = v) +
    theme_minimal()
})
grid.arrange(grobs = p_list, ncol = 2)

## Histogram + Density plot
hist_list <- lapply(vars, function(v) {
  ggplot(df_clean, aes_string(x = v)) +
    geom_histogram(
      aes(y = ..density..),
      bins = 60,
      fill = "palegreen3",
      color = "gray"
    ) +
    geom_density(color = "salmon", linewidth = 1) +
    labs(
      title = paste("Histogram & Density of", v),
      x = v,
      y = "Density"
    ) +
    theme_minimal()
})
grid.arrange(grobs = hist_list, ncol = 2)

## QQ plot
qq_list <- lapply(vars, function(v) {
  ggplot(df_clean, aes(sample = .data[[v]])) +
    stat_qq() +
    stat_qq_line(color = "salmon") +
    labs(
      title = paste("QQ Plot:", v),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal()
})
grid.arrange(grobs = qq_list, ncol = 2)

# =========================================================================

# Markov Switching Model

## Kiểm tra tính dừng
adf.test(df_clean$VNINDEX_R, alternative = "stationary")
adf.test(df_clean$SP500_R, alternative = "stationary")
adf.test(df_clean$USDVND_R, alternative = "stationary")
adf.test(df_clean$BRENT_R, alternative = "stationary")
adf.test(df_clean$LNVOL, alternative = "stationary")
adf.test(df_clean$NFI_VOL, alternative = "stationary")

## Kiểm định tuyến tính (Tsay test)
tsay <- tsayTest(df_clean$VNINDEX_R, order = 2)

cat(
  paste0(
    "\n\tTsay Test for Nonlinearity\n\n",
    "data:  df_clean$VNINDEX_R\n",
    "Tsay statistic = ",
    round(tsay$test.stat, 4),
    " , AR order = ",
    tsay$order,
    " , p-value = ",
    format.pval(tsay$p.value, digits = 5),
    "\n",
    "alternative hypothesis: nonlinearity in the time series\n"
  )
)

## ACF, PACF của VNINDEX_R
acf(df$VNINDEX_R, main = "ACF of VNINDEX_R")
pacf(df$VNINDEX_R, main = "PACF of VNINDEX_R")

numeric_vars <- c("SP500_R", "USDVND_R", "BRENT_R", "LNVOL", "NFI_VOL")
df_clean[numeric_vars] <- scale(df_clean[numeric_vars])

## Lựa chọn p, k tối ưu dựa trên AIC, BIC và Log-likelihood
model_base <- lm(VNINDEX_R ~ SP500_R + USDVND_R + BRENT_R + LNVOL + NFI_VOL, data = df_clean)
car::vif(model_base)

ms_results <- data.frame(
  k = integer(),
  p = integer(),
  AIC = numeric(),
  BIC = numeric(),
  logLik = numeric(),
  stringsAsFactors = FALSE
)

for (k in 2:3) {
  for (p in 0:2) {
    cat("Running model with k =", k, "and p =", p, "\n")
    tryCatch(
      {
        ms_model_try <- msmFit(
          model_base,
          k = k,
          p = p,
          sw = rep(TRUE, length(model_base$coefficients) + 1 + p)
        )
        swi <- ms_model_try@switch[-length(ms_model_try@switch)
        ]
        np <- ms_model_try["k"] * sum(swi) + sum(!swi)
        
        aic_val <- 2 * ms_model_try["Fit"]["logLikel"] + 2 * np
        bic_val <- 2 * ms_model_try["Fit"]["logLikel"] + 2 * np * log(nrow(ms_model_try@model$model))
        logLik_val <- -ms_model_try["Fit"]["logLikel"]
        
        cat(
          "  -> Success: AIC =",
          aic_val,
          ", BIC =",
          bic_val,
          ", logLike =",
          logLik_val,
          "\n"
        )
        ms_results <- rbind(
          ms_results,
          data.frame(
            k = k,
            p = p,
            AIC = aic_val,
            BIC = bic_val,
            logLik = logLik_val
          )
        )
      },
      error = function(e) {
        cat("  -> Error:", conditionMessage(e), "\n")
      }
    )
  }
}

ms_results <- arrange(ms_results, AIC, BIC, desc(logLik))
print(ms_results)

k_optimal <- ms_results$k[1]
p_optimal <- ms_results$p[1]

cat("Best-fit model: MS(", k_optimal, ")-AR(", p_optimal, ")-X", sep = "")

## Chạy MS model với p, k tối ưu
### Lưu ý: Mỗi lần chạy mô hình sẽ cho ra các thứ tự regime khác nhau
ms_model <- msmFit(
  model_base,
  k = k_optimal,
  p = p_optimal,
  sw = rep(TRUE, length(model_base$coefficients) + 1 + p_optimal)
)
summary(ms_model)

## Xử lý data để thống kê và plot
model_data <- model.frame(model_base)
model_data$Date <- df_clean$DATE
model_data$Date <- as.Date(model_data$Date)
model_data <- model_data[(-p_optimal + 1), ]
model_data$Regime <- apply(ms_model@Fit@smoProb, 1, which.max)
model_data$max_prob <- apply(ms_model@Fit@smoProb, 1, max)
model_data$VNINDEX <- df_clean$VNINDEX[p_optimal:nrow(df_clean)]

model_data_dt <- as.data.table(model_data)
model_data_dt[, regime_group := rleid(Regime)]

## Thống kê
regime_blocks <- model_data_dt[, .(
  Regime = unique(Regime),
  Start = min(Date),
  End = max(Date),
  Duration = .N
), by = regime_group]

sojourn_stats <- regime_blocks %>%
  group_by(Regime) %>%
  summarise(
    Sojourns = n(),
    Mean_Duration = round(mean(Duration), 2),
    Median_Duration = median(Duration),
    Min_Duration = min(Duration),
    Max_Duration = max(Duration),
    SD_Duration = round(sd(Duration), 2)
  )

print(sojourn_stats)

## Plot

par(mar = c(3, 3, 3, 3))

plot(ms_model)
plotDiag(ms_model, "all", which = 1)
plotDiag(ms_model, "all", which = 2)
plotDiag(ms_model, "all", which = 3)
plotProb(ms_model, which = c(1:(k_optimal + 1)))

# =========================================================================

# Kiểm định ARCH effect
ArchTest(df_clean$VNINDEX_R, lags = 5)

# GARCH(1,1)
garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "jsu"
)

garch_fit <- ugarchfit(spec = garch, data = df_clean$VNINDEX_R)

show(garch_fit)

# AR(1)-GARCH(1,1)
ar_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "jsu"
)

ar_garch_fit <- ugarchfit(spec = ar_garch, data = df_clean$VNINDEX_R)

show(ar_garch_fit)

# AR(1)-GARCH(1,1)-X
ar_garch_x <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(
    armaOrder = c(1, 0),
    include.mean = TRUE,
    external.regressors = as.matrix(df_clean[, c("SP500_R", "USDVND_R", "BRENT_R", "LNVOL", "NFI_VOL")])
  ),
  distribution.model = "jsu"
)

ar_garch_x_fit <- ugarchfit(spec = ar_garch_x, data = df_clean$VNINDEX_R)

show(ar_garch_x_fit)

# AR(1)-EGARCH(1,1)-X
ar_egarch_x <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model = list(
    armaOrder = c(1, 0),
    include.mean = TRUE,
    external.regressors = as.matrix(df_clean[, c("SP500_R", "USDVND_R", "BRENT_R", "LNVOL", "NFI_VOL")])
  ),
  distribution.model = "jsu"
)

ar_egarch_x_fit <- ugarchfit(spec = ar_egarch_x, data = df_clean$VNINDEX_R)

show(ar_egarch_x_fit)

# AR(1)-GJR-GARCH(1,1)-X
ar_gjr_garch_x <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model = list(
    armaOrder = c(1, 0),
    include.mean = TRUE,
    external.regressors = as.matrix(df_clean[, c("SP500_R", "USDVND_R", "BRENT_R", "LNVOL", "NFI_VOL")])
  ),
  distribution.model = "jsu"
)

ar_gjr_garch_x_fit <- ugarchfit(spec = ar_gjr_garch_x, data = df_clean$VNINDEX_R)

show(ar_gjr_garch_x_fit)

model_summary <- function(model) {
  c(
    AIC = infocriteria(model)[1],
    BIC = infocriteria(model)[2],
    logLik = likelihood(model)
  )
}

garch_results <- rbind(
  "GARCH(1,1)" = model_summary(garch_fit),
  "AR(1)-GARCH(1,1)" = model_summary(ar_garch_fit),
  "AR(1)-GARCH(1,1)-X" = model_summary(ar_garch_x_fit),
  "AR(1)-EGARCH(1,1)-X" = model_summary(ar_egarch_x_fit),
  "AR(1)-GJR-GARCH(1,1)-X" = model_summary(ar_gjr_garch_x_fit)
)

garch_results <- as.data.frame(garch_results)
garch_results <- arrange(garch_results, AIC, BIC, desc(logLik))
print(garch_results)
cat("Best-fit model:", rownames(garch_results)[1])

## Plot
par(mfrow = c(1, 1))
plot(
  df_clean$DATE,
  sigma(ar_egarch_x_fit),
  type = "l",
  col = "palegreen3",
  main = "Conditional Volatility (EGARCH)",
  xlab = "Date",
  ylab = ""
)
plot(
  df_clean$DATE,
  residuals(ar_egarch_x_fit, standardize = TRUE),
  type = "l",
  col = "palegreen3",
  main = "Standardized Residuals",
  xlab = "Date",
  ylab = ""
)
qqnorm(residuals(ar_egarch_x_fit, standardize = TRUE))
qqline(residuals(ar_egarch_x_fit, standardize = TRUE), col = "red")
hist(
  residuals(ar_egarch_x_fit, standardize = TRUE),
  breaks = 50,
  main = "Histogram of Standardized Residuals",
  probability = TRUE,
  xlab = ""
)

ni <- newsimpact(ar_egarch_x_fit)
ni_df <- data.frame(z = ni$zx, h = ni$zy)
ggplot(ni_df, aes(x = z, y = h)) +
  geom_line(color = "palegreen3", size = 1.2) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "gray50"
  ) +
  labs(title = "News Impact Curve – AR(1)-EGARCH(1,1)-X", x = "Shock", y = "Conditional Variance") +
  theme_minimal(base_size = 14)

# =========================================================================

# Kiểm định phương sai thay đổi (Heteroskedasticity)
ols_model <- lm(VNINDEX_R ~ SP500_R + USDVND_R + BRENT_R + LNVOL + NFI_VOL, data = df_clean)
bptest(ols_model)

# Hồi quy Phân vị
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
qr_models <- lapply(taus, function(tau) {
  rq(
    VNINDEX_R ~ SP500_R + USDVND_R + BRENT_R + LNVOL + NFI_VOL,
    data = df_clean,
    tau = tau
  )
})
for (i in seq_along(taus)) {
  cat("\n\n--- Quantile:", taus[i], "---\n")
  print(summary(qr_models[[i]], se = "boot"))
}

# Plot
coef_data <- do.call(rbind, lapply(seq_along(taus), function(i) {
  cbind(tau = taus[i], as.data.frame(t(coef(qr_models[[i]]))))
}))

coef_melted <- melt(coef_data, id.vars = "tau")

variable_order <- c(
  "(Intercept)",
  "SP500_R",
  "USDVND_R",
  "BRENT_R",
  "LNVOL",
  "NFI_VOL"
)
coef_melted$variable <- factor(coef_melted$variable, levels = variable_order)

custom_colors <- brewer.pal(6, "Set2")

ggplot(
  coef_melted,
  aes(
    x = tau,
    y = value,
    color = variable,
    group = variable
  )
) +
  geom_hline(
    yintercept = 0,
    color = "gray50",
    linetype = "dashed",
    size = 0.6
  ) +
  geom_line(size = 1) +
  geom_point(
    size = 2,
    shape = 21,
    fill = "white",
    stroke = 1
  ) +
  scale_color_manual(values = custom_colors) +
  scale_x_continuous(
    breaks = unique(coef_melted$tau),
    labels = function(x) {
      paste0(x * 100, "%")
    }
  ) +
  scale_y_continuous(breaks = pretty_breaks(n = 8)) +
  labs(
    title = "Quantile Regression Coefficients",
    x = "Quantile (τ)",
    y = "Coefficient Value",
    color = "Variable"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92"),
    axis.line = element_line(color = "gray30", size = 0.6),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(
      face = "italic",
      size = 12,
      hjust = 0.5,
      margin = margin(b = 15)
    ),
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", color = "gray90"),
    legend.margin = margin(
      t = 0,
      r = 5,
      b = 0,
      l = 5
    ),
    legend.key.size = unit(1.2, "cm"),
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold", margin = margin(
      t = 10,
      r = 10,
      b = 10,
      l = 10
    )),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(
      color = "gray80",
      fill = NA,
      size = 0.6
    )
  )

plot_list <- list()
for (i in seq_along(taus)) {
  p <- ggplot(data.frame(residuals = resid(qr_models[[i]])), aes(x = residuals)) +
    geom_density(
      fill = "palegreen3",
      alpha = 0.7,
      color = "black"
    ) +
    labs(
      title = paste("τ =", taus[i]),
      x = "Residuals",
      y = "Density"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  plot_list[[i]] <- p
}
title <- grid::textGrob(
  "Distribution of Residuals Across Quantiles",
  gp = grid::gpar(fontface = "bold", fontsize = 16)
)
combined_plot <- grid.arrange(grobs = plot_list, ncol = 2, top = title)
print(combined_plot)
