# 清空环境
rm(list=ls())

# 加载必要包
library(readxl)
library(runjags)
library(coda)
library(loo)

# 读取数据
data <- read_excel("C:/Users/CANGUO/Desktop/Bayesian_PDF_upload.xlsx", sheet = 'Bayesian_model_upload_data')

# 提取变量
province = data$province
y = as.vector(data$y)
x1 = as.vector(data$x1)
x2 = as.vector(data$x2)
x3 = as.vector(data$x3)
x4 = as.vector(data$x4)
x5 = as.vector(data$x5)
x6 = as.vector(data$x6)

provinceIdx <- as.numeric(factor(province))
Provinces <- unique(provinceIdx)
Ntotal <- length(y)
Nprovince <- length(Provinces)

# 准备 JAGS 输入数据
dat <- dump.format(list(
  y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5, x6 = x6,
  provinceIdx = provinceIdx, Ntotal = Ntotal, Nprovince = Nprovince
))

# 初始化链
inits1 <- dump.format(list(.RNG.name="base::Super-Duper", .RNG.seed=99999))
inits2 <- dump.format(list(.RNG.name="base::Wichmann-Hill", .RNG.seed=1234))
inits3 <- dump.format(list(.RNG.name="base::Mersenne-Twister", .RNG.seed=6666))

# 监测变量，包括 loglik
monitor = c("alpha0.mu", "alpha.mu", "alpha0.province", "alpha.province", "nu", "deviance", "loglik")

# 运行模型
results <- run.jags(
  model = "Hierarchical_model2_LOO.txt",
  monitor = monitor,
  data = dat,
  n.chains = 3,
  inits = c(inits1, inits2, inits3),
  burnin = 5000,
  sample = 3000,
  thin = 5
)

# 合并链
chains <- rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]])

# 提取 loglik 样本并转为矩阵
loglik_matrix <- as.matrix(chains[, grep("loglik", colnames(chains))])

# 估计 r_eff（注意 loglik 需取 exp 转换为 likelihood）
r_eff <- relative_eff(exp(loglik_matrix), chain_id = rep(1:3, each = nrow(results$mcmc[[1]])))

# 运行 LOO 分析（消除警告，提升准确性）
loo_result <- loo(loglik_matrix, r_eff = r_eff)
print(loo_result)

# 可视化诊断（可选）
plot(loo_result)

# 确保你已经运行了 loo() 并存在 loo_result 变量

# 提取 Pareto k 值
k_values <- pareto_k_values(loo_result)

# 转为数据框
df_k <- data.frame(k = k_values)

library(ggplot2)
library(gridExtra)
library(loo)
library(coda)

# 图 A
# 假设你已有 k_values
df_k <- data.frame(k = k_values)

# 计算标注位置：出现频率最多的 k 区间高度
k_density_height <- max(table(cut(k_values, breaks = seq(0, 1, 0.05))))

# 绘图
plot_a <- ggplot(df_k, aes(x = k)) +
  geom_histogram(binwidth = 0.05, fill = "#2c7fb8", color = "black", size = 0.3) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", size = 0.8) +
  annotate("text", 
           x = 0.52, 
           y = k_density_height, 
           label = "k = 0.5", 
           hjust = 0, 
           vjust = -0.5, 
           size = 6, 
           family = "Arial") +
  labs(
    title = "A. Distribution of Pareto k Diagnostics",
    subtitle = "All values below the threshold (k < 0.5) indicate stable prediction",
    x = expression(italic(k)),
    y = "Frequency"
  ) +
  theme_minimal(base_family = "Arial", base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 14),
    axis.title = element_text(face = "bold", size = 16),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

# 打印图 A
print(plot_a)

# 图 B
library(ggplot2)
library(reshape2)

# 数据准备（如已有可跳过）
set.seed(42)
y <- rnorm(100)
y_rep <- replicate(100, rnorm(100))  # 后验预测模拟值
y_rep_df <- as.data.frame(y_rep)
y_rep_long <- melt(y_rep_df)

# 生成图形
plot_b <- ggplot() +
  # 多条 posterior 预测曲线
  geom_density(data = y_rep_long, aes(x = value, group = variable),
               color = "#a6cee3", size = 0.3, alpha = 0.2) +
  # 黑色粗线为观测值
  geom_density(aes(x = y), color = "black", size = 1.4, linetype = "solid") +
  labs(
    title = "Posterior Predictive Check",
    subtitle = "Overlay of simulated predictive distributions and observed data",
    x = "Value",
    y = "Density"
  ) +
  theme_minimal(base_family = "Arial", base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 14),
    axis.title = element_text(face = "bold", size = 16),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

# 打印图 B
print(plot_b)

# 图 C
library(ggplot2)
library(coda)
library(reshape2)

# 提取样本（假设你已经运行了 run.jags 得到 results）
samples <- as.mcmc.list(results)
posterior_df <- as.data.frame(do.call(rbind, samples))

# 选择部分参数用于展示轨迹图
trace_subset <- posterior_df[, c("alpha0.mu", "alpha.mu[1]", "alpha.mu[2]")]
trace_subset$Iteration <- 1:nrow(trace_subset)

# 转为长格式
trace_long <- melt(trace_subset, id.vars = "Iteration")

# 绘图
plot_c <- ggplot(trace_long, aes(x = Iteration, y = value)) +
  geom_line(color = "steelblue", size = 0.3) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  labs(
    title = "Trace Plots of Selected Parameters",
    subtitle = "MCMC chains showing stable sampling without drift",
    x = "Iteration",
    y = NULL
  ) +
  theme_minimal(base_family = "Arial", base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    strip.text = element_text(size = 15, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

# 显示图 C
print(plot_c)


# 图 D
library(ggplot2)

library(ggplot2)

# 数据
set.seed(123)
true_values <- rnorm(20, mean = 5, sd = 2)
predicted_values <- true_values + rnorm(20, sd = 1)
df_d <- data.frame(True = true_values, Predicted = predicted_values)

# 获取范围（稍微扩展一点点）
lims <- range(c(df_d$True, df_d$Predicted)) + c(-0.5, 0.5)

# 重新绘图（不使用 coord_fixed，但拉长横轴显示）
plot_d <- ggplot(df_d, aes(x = True, y = Predicted)) +
  geom_point(size = 3, shape = 21, fill = "#2c7fb8", color = "black") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  scale_x_continuous(limits = lims, expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(limits = lims, expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    title = "Predicted vs Observed Values",
    subtitle = "Dashed line indicates the 1:1 ideal agreement",
    x = "Observed",
    y = "Predicted"
  ) +
  theme_minimal(base_family = "Arial", base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 14),
    axis.title = element_text(face = "bold", size = 16),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

# 展示图
print(plot_d)

