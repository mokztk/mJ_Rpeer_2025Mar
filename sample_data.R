# mJOHNSNOW R解析ピア勉強会用 サンプルの10年生存データ

library(tidyverse)
library(survival)
library(ggsurvfit)

# まず違和感の少ない生存データを作ってから、背景因子を付け足していく

# step1: ベースの生存曲線データ ----------------------------------------------------------

# 死亡症例の観察期間はワイブル分布、
# 生存打ち切り例はベータ分布 Beta(2, 1) から取る
# Fujisawa, et al. Eur Respir J. 2019 の生存曲線をイメージ

# 乱数で生成した観察期間を指定範囲に収める処理
fn_scale_time <- function(time, min = 0, max = 1) {
  return(min + time * (max - min) / max(time))
}

# 結果再現のために乱数シードを固定
set.seed(54321)

# IPF 想定
# n = 250、10年(120ヶ月)後の想定生存率 20-30%
data_ipf <-
  # 死亡例
  tibble(
    # だんだん死亡リスクは上がる
    time = rweibull(n = 500, shape = 1.2, scale = 1) %>%
             # 多めに [1, 120] で作ってサンプリングする
             fn_scale_time(min = 1, max = 120) %>%
             sample(size = 175, replace = FALSE),
    died = 1
  ) %>% 
  # 生存例
  rbind(
    tibble(
      # rbeta() の結果は [0, 1] なので 120倍＋アルファ
      time = rbeta(n = 75, shape1 = 2, shape2 = 1) * 125,
      died = 0
    )
  ) %>%
  # シャッフル
  mutate(dx = "IPF", .before = everything()) %>% 
  slice_sample(n = 250, replace = FALSE) %>%
  mutate(no = row_number(), .before = everything())

# NSIP 想定
# n = 100、10年(120ヶ月)後の想定生存率 70-80%
data_nsip <-
  # 死亡例
  tibble(
    # 死亡リスクは一定
    time = rweibull(n = 500, shape = 1.0, scale = 1) %>%
             # 多めに [1, 120] で作ってサンプリングする
             fn_scale_time(min = 1, max = 120) %>%
             sample(size = 27, replace = FALSE),
    died = 1
  ) %>% 
  # 生存例
  rbind(
    tibble(
      # rbeta() の結果は [0, 1] なので 120倍＋アルファ
      time = rbeta(n = 73, shape1 = 2, shape2 = 1) * 125,
      died = 0
    )
  ) %>%
  # シャッフル
  mutate(dx = "NSIP", .before = everything()) %>% 
  slice_sample(n = 100, replace = FALSE)  %>%
  mutate(no = row_number(), .before = everything())

# COP 想定
# n = 50、10年(120ヶ月)後の想定生存率 90%-
data_cop <-
  # 死亡例
  tibble(
    # だんだん死亡リスクは下がる
    time = rweibull(n = 500, shape = 0.8, scale = 1) %>%
             # 多めに [1, 120] で作ってサンプリングする
             fn_scale_time(min = 1, max = 120) %>%
             sample(size = 3, replace = FALSE),
    died = 1
  ) %>% 
  # 生存例
  rbind(
    tibble(
      # rbeta() の結果は [0, 1] なので 120倍＋アルファ
      time = rbeta(n = 47, shape1 = 2, shape2 = 1) * 125,
      died = 0
    )
  ) %>%
  # シャッフル
  mutate(dx = "COP", .before = everything()) %>% 
  slice_sample(n = 50, replace = FALSE)  %>%
  mutate(no = row_number(), .before = everything())

# test
data_ipf %>% 
  rbind(
    data_nsip,
    data_cop
  ) %>% 
  # 120ヶ月で打ち切り（死亡例は 120ヶ月以内にしてあるので生存群のみ）
  mutate(time = if_else(time > 120, 120, time)) %>%
  # 作図
  ggsurvfit::survfit2(Surv(time, died) ~ dx, data = .) %>% 
  ggsurvfit::ggsurvfit(size = 1) +
    ggsurvfit::add_censor_mark() +
    ggsurvfit::add_confidence_interval(type = "ribbon") +
    ggsurvfit::add_risktable(risktable_stats = "n.risk") +
    labs(
      x = "Observation time (mo)"
    ) +
    scale_x_continuous(limits = c(0, 120), expand = c(0.03, 0.01),
                       breaks = seq(0, 120, 12)) +
    scale_y_continuous(limits = c(0, 1.0), expand = c(0.01, 0.01),
                       labels = scales::percent) +
    theme_classic(base_size = 14) +
    theme(
      axis.text  = element_text(colour = "black"),
      panel.grid.major = element_line(colour = "lightgray")
    )

# step 2: 背景因子 ----------------------------------------------------------------

