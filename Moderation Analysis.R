# 1. 加载必要的包
library(dplyr)
# 查看表格的前几行和列名
head(X9_4_mt_acc)
head(X9_4_ct_acc)

# 查看数据的整体结构（检查每一列是数字、文字还是因子）
str(X9_4_mt_acc)

# 查看描述性统计量（均值、最大最小值等）
summary(X9_4_mt_acc)

# 统一并计算两组的总体平均正确率 (overall_acc)
mt_clean <- X9_4_mt_acc %>%
  rename(subject_id = any_of(c("sub_mt", "sub_ct", "sub"))) %>%
  mutate(overall_acc = (s1_acc + s2_acc) / 2)

ct_clean <- X9_4_ct_acc %>%
  rename(subject_id = any_of(c("sub_ct", "sub_mt", "sub"))) %>%
  mutate(overall_acc = (s1_acc + s2_acc) / 2)

# 筛选出正确率 >= 60% 的有效被试白名单
valid_mt <- mt_clean %>% filter(overall_acc >= 0.60) %>% pull(subject_id)
valid_ct <- ct_clean %>% filter(overall_acc >= 0.60) %>% pull(subject_id)

# 打印看看各被剔除了多少人
print(paste("MT组剔除人数:", nrow(mt_clean) - length(valid_mt)))
print(paste("CT组剔除人数:", nrow(ct_clean) - length(valid_ct)))

# 画出 MT 组和 CT 组的正确率分布直方图
hist(mt_clean$overall_acc, main="MT Group Accuracy Distribution", xlab="Accuracy")
hist(ct_clean$overall_acc, main="CT Group Accuracy Distribution", xlab="Accuracy")


# 加载必要的包
library(tidyverse)
library(readxl)
library(interactions)

# 使用绝对路径直接读取
mt_acc <- read_excel("/Users/nebula/Desktop/9.4_mt_acc.xlsx")
ct_acc <- read_excel("/Users/nebula/Desktop/9.4_ct_acc.xlsx")

# 检查是否成功读取
head(mt_acc)
head(ct_acc)

mt_clean <- mt_acc %>%
  rename(subject_id = any_of(c("sub_mt", "sub_ct", "sub"))) %>%
  mutate(overall_acc = (s1_acc + s2_acc) / 2)

ct_clean <- ct_acc %>%
  rename(subject_id = any_of(c("sub_ct", "sub_mt", "sub"))) %>%
  mutate(overall_acc = (s1_acc + s2_acc) / 2)

# 提取正确率 >= 60% 的有效被试白名单（共 110 人）
valid_subjects <- bind_rows(mt_clean, ct_clean) %>%
  filter(overall_acc >= 0.60) %>%
  pull(subject_id)


# =====================================================================
# 通过白名单精准过滤掉那 9 名异常被试
# =====================================================================
df_clean <- mt_ct_119 %>%
  filter(subject %in% valid_subjects)

# 检查清洗后的样本量是否正确（应该正好是 110 人左右）
print(paste("清洗后的最终样本量 N =", nrow(df_clean)))


library(dplyr)
library(haven)
# 将含有标签的变量强制转换为纯数字
df_clean <- mt_ct_119 %>%
  mutate(
    Zconsc = as.numeric(Zconsc),
    Zneuroticism = as.numeric(Zneuroticism),
    cond = as.numeric(cond),
    conf_chg = as.numeric(conf_chg)  # 冲突变化分数
  )

# =====================================================================
#运行线性回归模型
# =====================================================================
# 跑尽责性模型
model_consc <- lm(conf_chg ~ cond * Zconsc, data = df_clean)

# 跑神经质模型
model_neuro <- lm(conf_chg ~ cond * Zneuroticism, data = df_clean)

library(interactions)

# =====================================================================
# 4. 生成最终版交互效应图 (Simple Slopes Plots)
# =====================================================================
# 最终版 Figure 1 (尽责性)
interact_plot(model_consc, 
              pred = Zconsc, 
              modx = cond, 
              plot.points = TRUE,
              colors = "Dark2",
              main.title = "Interaction: Conscientiousness & Meditation (N = 110)",
              x.label = "Conscientiousness (Z-score)",
              y.label = "Conflict Change Score (ms)")

# 最终版 Figure 2 (神经质)
interact_plot(model_neuro, 
              pred = Zneuroticism, 
              modx = cond, 
              plot.points = TRUE,
              colors = "Dark2",
              main.title = "Interaction: Neuroticism & Meditation (N = 110)",
              x.label = "Neuroticism (Z-score)",
              y.label = "Conflict Change Score (ms)")

install.packages("afex")
library(afex)

# 1. 将宽格式数据转换为长格式
df_long <- df_clean %>%
  pivot_longer(cols = c(con_mean, icon_mean), 
               names_to = "trial_type", 
               values_to = "rt")

# 2. 运行重复测量方差分析
flanker_anova <- aov_ez(
  id = "subject", 
  dv = "rt",
  within = "trial_type",
  data = df_long
)

print(flanker_anova)