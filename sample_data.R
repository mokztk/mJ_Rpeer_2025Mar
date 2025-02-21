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

# 施設は "大学病院", "総合病院A", "総合病院B", "市立病院X", "市立病院Y"
# 症例数はそれぞれ 200, 80, 70, 30, 20
# 男女比は IPFで 3:2、NSIPで 2:3、COPは 1:1 とする
# 年齢 mean(SD) は、IPF 70(7)、60(7)、60(10) とする
# 喫煙歴ありは IPFの男性で 80%、その他の男性は 40%、女性 30% とする
# 基礎疾患は「高血圧」「糖尿病」「不整脈」をそれぞれランダムで付加

data_background <-
  # 3つの群を結合しシャッフル
  data_ipf %>% 
  rbind(
    data_nsip,
    data_cop
  ) %>% 
  slice_sample(n = nrow(.), replace = FALSE) %>% 
  # 施設を追加してまたシャッフル
  mutate(
    facility = c(rep("大学病院" , 200),
                 rep("総合病院A",  80),
                 rep("総合病院B",  70),
                 rep("市立病院X",  30),
                 rep("市立病院Y",  20))
  ) %>% 
  slice_sample(n = nrow(.), replace = FALSE) %>% 
  # 性別、年齢を追加してまたシャッフル
  mutate(
    # no（疾患ごとの症例番号）がランダム抽出した中にあれば男性、なければ女性
    sex = case_when(
      dx == "IPF"  ~ if_else(no %in% sample(1:250, 150, replace = F), "男", "女"),
      dx == "NSIP" ~ if_else(no %in% sample(1:100,  40, replace = F), "男", "女"),
      dx == "COP"  ~ if_else(no %in% sample(1:50 ,  25, replace = F), "男", "女"),
    )
  ) %>% 
  # 年齢は1行ずつ処理して生成
  rowwise() %>% 
  mutate(
    age = case_when(
      dx == "IPF"  ~ rnorm(1, mean = 70, sd = 5),
      dx == "NSIP" ~ rnorm(1, mean = 60, sd = 7),
      dx == "COP"  ~ rnorm(1, mean = 60, sd = 10)
    )
  ) %>%
  ungroup() %>% 
  slice_sample(n = nrow(.), replace = FALSE) %>% 
  # IPF男性、IPF以外男性、女性の3つに分けて喫煙歴をつける
  mutate(
    sm_group = case_when(dx == "IPF" & sex == "男" ~ "a",
                         sex == "男"               ~ "b",
                         sex == "女"               ~ "c")
  ) %>% 
  group_by(sm_group) %>% 
  mutate(sm_id = row_number()) %>% 
  ungroup() %>% 
  mutate(
    smoking = case_when(
      sm_group == "a" ~ if_else(sm_id %in% sample(150, 105, F), "あり", "なし"),
      sm_group == "b" ~ if_else(sm_id %in% sample( 65,  25, F), "あり", "なし"),
      sm_group == "c" ~ if_else(sm_id %in% sample(185,  55, F), "あり", "なし")
    )
  ) %>% 
  # 喫煙歴の作業用変数を削除してシャッフル
  select(-sm_group, -sm_id) %>% 
  slice_sample(n = nrow(.), replace = FALSE) %>% 
  # 合併症は有病率を決めてランダム生成
  mutate(
    com_diabetes     = if_else(runif(400, 0, 1) < 0.4, "あり", "なし"),
    com_arrythmia    = if_else(runif(400, 0, 1) < 0.3, "あり", "なし"),
    com_hypertention = if_else(runif(400, 0, 1) < 0.6, "あり", "なし"),
  ) %>% 
  # 不要な変数を整理
  select(
    no, facility, age, sex, dx, time, died, smoking,
    com_diabetes, com_arrythmia, com_hypertention
  )

