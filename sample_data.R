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
    age = case_when(dx == "IPF"  ~ rnorm(1, mean = 70, sd = 5),
                    dx == "NSIP" ~ rnorm(1, mean = 60, sd = 7),
                    dx == "COP"  ~ rnorm(1, mean = 60, sd = 10))
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

# step 3: 登録順 ------------------------------------------------------------------

# 研究期間は、2013-10-01 から 2024-03-31 の10年半とする
# 登録日は、2013-10-01 から　2024-03-31 - time (mo.) の間でランダムに決定
# それを起点に打ち切り日（と生年月日）を決める

data_indexed <-
  data_background %>% 
  # 登録日を計算
  rowwise() %>% 
  mutate(
    date_limit   = ymd("2024-03-31") - time * 365.25 / 12,
    date_enroll  = runif(1, ymd("2013-10-01"), date_limit) %>% as_date(),
    date_outcome = date_enroll + time * 365.25 / 12,
    date_birth   = date_enroll - age * 365.25
  ) %>% 
  ungroup() %>% 
  # 年齢は整数、観察期間は小数点以下1桁にする
  mutate(
    age  = floor(age) %>% as.integer(),
    time = round(time, 1)
  ) %>% 
  # 登録日順に並べ替えて全体の症例登録番号を振る
  arrange(date_enroll) %>% 
  mutate(index = row_number(), .before = everything()) %>% 
  # 患者IDを追加（あえて形式を不揃いにする）
  #   大学病院と市立病院Xは施設内通し番号
  #   総合病院A, B は整数6桁
  #   市立病院Y は整数4桁
  group_nest(facility) %>% 
  mutate(
    data2 = map2(
      facility, data,
      function(f, d) {
        mutate(d,
               Pt_ID = case_when(
                 f == "大学病院"  ~ 1:nrow(d),
                 f == "総合病院A" ~ sample(1:500000, nrow(d), replace = F),
                 f == "総合病院B" ~ sample(1:300000, nrow(d), replace = F),
                 f == "市立病院X" ~ 1:nrow(d),
                 f == "市立病院Y" ~ sample(1:8000  , nrow(d), replace = F))
        )
      })
  ) %>% 
  select(-data) %>% 
  unnest(cols = data2) %>% 
  # 並べ直し
  select(
    index, facility, Pt_ID, date_enroll, sex, age, date_birth,
    dx, date_outcome, time, died,
    com_diabetes, com_arrythmia, com_hypertention
  ) %>% 
  arrange(index)

# step 4: 検査データ ---------------------------------------------------------------

# 結果再現のために乱数シードを固定
set.seed(12345)

# KL-6 は対数正規分布とする
# 　登録時（0y) で 平均を IPF 1000, NSIP 800, COP 1200 で求める
#   1年後 (1y) 、3年後 (3y)、5年後 (5y) は登録時に適当な係数＋ノイズを掛けていく
# FVCとFEV1.0 は相関係数 0.7 の2次元正規分布とする
#   登録時 (0y) 平均を IPF FVC 2.5, FEV1.0 2.0、NSIPとCOPは FVC 3.0, FEV1.0 2.5 とする
#   FVCとFEV1.0が逆転した場合は入れ替える
#   1年後 (1y) 、3年後 (3y)、5年後 (5y) は登録時から適当な値＋ノイズを引いていく
# DLco は %DLco として、Beta(5, 3) の形状で 100% までのレンジに拡大する
#   登録時 (0y) に IPFは 50 - 95%、他は 70 - 110% とする
#   1年後 (1y) 、3年後 (3y)、5年後 (5y) は登録時に適当な係数＋ノイズを掛けていく

# 呼吸機能検査の値を先に作ってストックしておく
sigma <- matrix(c(0.5^2, 0.7 * 0.5 * 0.4, 0.7 * 0.5 * 0.4, 0.4^2), 2, 2)

fvc_stock_ipf <- 
  MASS::mvrnorm(n = 250, mu = c(2.5, 2.0), Sigma = sigma) %>% 
  as_tibble(.name_repair = "unique") %>% 
  rename_with(~ c("a", "b")) %>% 
  mutate(
    FVC_0y      = if_else(a < b, b, a),
    FEV1.0_0y   = if_else(a < b, a, b),
    pct_DLco_0y = 50 + 45 * rbeta(250, 5, 3)
  ) %>% 
  select(FVC_0y, FEV1.0_0y, pct_DLco_0y)

fvc_stock_nonipf <- 
  MASS::mvrnorm(n = 150, mu = c(3.0, 2.5), Sigma = sigma) %>% 
  as_tibble(.name_repair = "unique") %>% 
  rename_with(~ c("a", "b")) %>% 
  mutate(
    FVC_0y      = if_else(a < b, b, a),
    FEV1.0_0y   = if_else(a < b, a, b),
    pct_DLco_0y = 70 + 40 * rbeta(150, 5, 3)
  ) %>% 
  select(FVC_0y, FEV1.0_0y, pct_DLco_0y)

data_followup <-
  data_indexed %>% 
  # KL-6
  rowwise() %>% 
  mutate(
    KL6_0y = case_when(dx == "IPF"  ~ rlnorm(1, meanlog = log(1000), sd = 0.3),
                       dx == "NSIP" ~ rlnorm(1, meanlog = log(800) , sd = 0.2),
                       dx == "COP"  ~ rlnorm(1, meanlog = log(1200), sd = 0.4)),
    KL6_1y = case_when(dx == "COP" & died == 0 ~ KL6_0y * rnorm(1, 0.6, 0.2),
                       died == 0               ~ KL6_0y * rnorm(1, 1.0, 0.2),
                       died == 1               ~ KL6_0y * rnorm(1, 1.2, 0.2)),
    KL6_3y = case_when(dx == "COP" & died == 0 ~ KL6_1y * rnorm(1, 0.8, 0.2),
                       died == 0               ~ KL6_1y * rnorm(1, 0.9, 0.3),
                       died == 1               ~ KL6_1y * rnorm(1, 1.2, 0.2)),
    KL6_5y = case_when(dx == "COP" & died == 0 ~ KL6_3y * rnorm(1, 1.0, 0.1),
                       died == 0               ~ KL6_3y * rnorm(1, 0.9, 0.3),
                       died == 1               ~ KL6_3y * rnorm(1, 1.2, 0.2)),
    # 小数点以下を丸める
    across(starts_with("KL6_"), round),
    # 観察期間がそれぞれの時点に満たないものは除外
    KL6_1y = if_else(time < 12 * 1, NA_real_, KL6_1y),
    KL6_3y = if_else(time < 12 * 3, NA_real_, KL6_3y),
    KL6_5y = if_else(time < 12 * 5, NA_real_, KL6_5y)
  ) %>% 
  ungroup() %>% 
  # 呼吸機能
  # 0y はあらかじめ作っておいた呼吸機能検査を結合する
  group_nest(dx == "IPF") %>% 
  rename_with(~ c("ipf", "data")) %>% 
  mutate(
    fvc   = map(ipf, \(x) if (x) fvc_stock_ipf else fvc_stock_nonipf),
    data2 = map2(data, fvc, \(d, f) bind_cols(d, f))
  ) %>% 
  select(data2) %>% 
  unnest(cols = "data2") %>% 
  arrange(index) %>% 
  # 経過データ
  rowwise() %>% 
  mutate(
    # FVC, FEV1.0 の 1y, 3y, 5y は順にある程度の値を引いていく
    FVC_1y    = case_when(dx == "IPF" & died == 1 ~ FVC_0y    - rnorm(1, 0.40, 0.2),
                          dx == "IPF" & died == 0 ~ FVC_0y    - rnorm(1, 0.20, 0.1),
                          died == 1               ~ FVC_0y    - rnorm(1, 0.25, 0.2),
                          died == 0               ~ FVC_0y    - rnorm(1, 0.10, 0.1)),
    FEV1.0_1y = case_when(dx == "IPF" & died == 1 ~ FEV1.0_0y - rnorm(1, 0.40, 0.2),
                          dx == "IPF" & died == 0 ~ FEV1.0_0y - rnorm(1, 0.20, 0.1),
                          died == 1               ~ FEV1.0_0y - rnorm(1, 0.25, 0.2),
                          died == 0               ~ FEV1.0_0y - rnorm(1, 0.10, 0.1)),
    FEV1.0_1y = if_else(FEV1.0_1y > FVC_1y, FVC_1y - 0.1, FEV1.0_1y),
    FVC_3y    = case_when(dx == "IPF" & died == 1 ~ FVC_1y    - rnorm(1, 0.60, 0.4),
                          dx == "IPF" & died == 0 ~ FVC_1y    - rnorm(1, 0.30, 0.3),
                          died == 1               ~ FVC_1y    - rnorm(1, 0.40, 0.3),
                          died == 0               ~ FVC_1y    - rnorm(1, 0.20, 0.2)),
    FEV1.0_3y = case_when(dx == "IPF" & died == 1 ~ FEV1.0_1y - rnorm(1, 0.50, 0.4),
                          dx == "IPF" & died == 0 ~ FEV1.0_1y - rnorm(1, 0.30, 0.3),
                          died == 1               ~ FEV1.0_1y - rnorm(1, 0.40, 0.3),
                          died == 0               ~ FEV1.0_1y - rnorm(1, 0.20, 0.2)),
    FEV1.0_3y = if_else(FEV1.0_3y > FVC_3y, FVC_3y - 0.1, FEV1.0_3y),
    FVC_5y    = case_when(dx == "IPF" & died == 1 ~ FVC_3y    - rnorm(1, 1.00, 0.3),
                          dx == "IPF" & died == 0 ~ FVC_3y    - rnorm(1, 0.30, 0.2),
                          died == 1               ~ FVC_3y    - rnorm(1, 0.40, 0.2),
                          died == 0               ~ FVC_3y    - rnorm(1, 0.20, 0.1)),
    FEV1.0_5y = case_when(dx == "IPF" & died == 1 ~ FEV1.0_3y - rnorm(1, 0.50, 0.3),
                          dx == "IPF" & died == 0 ~ FEV1.0_3y - rnorm(1, 0.30, 0.2),
                          died == 1               ~ FEV1.0_3y - rnorm(1, 0.40, 0.2),
                          died == 0               ~ FEV1.0_3y - rnorm(1, 0.20, 0.1)),
    FEV1.0_5y = if_else(FEV1.0_5y > FVC_5y, FVC_5y - 0.1, FEV1.0_5y),
    # %DLco の 1y, 3y, 5y は順にある程度の係数を掛けていく
    pct_DLco_1y = case_when(dx == "IPF" & died == 1 ~ pct_DLco_0y * rnorm(1, 0.8, 0.1),
                            dx == "IPF" & died == 0 ~ pct_DLco_0y * rnorm(1, 0.9, 0.1),
                            died == 1               ~ pct_DLco_0y * rnorm(1, 0.9, 0.05),
                            died == 0               ~ pct_DLco_0y * rnorm(1, 1.0, 0.05)),
    pct_DLco_3y = case_when(dx == "IPF" & died == 1 ~ pct_DLco_1y * rnorm(1, 0.8, 0.2),
                            dx == "IPF" & died == 0 ~ pct_DLco_1y * rnorm(1, 0.8, 0.1),
                            died == 1               ~ pct_DLco_1y * rnorm(1, 0.8, 0.1),
                            died == 0               ~ pct_DLco_1y * rnorm(1, 0.9, 0.05)),
    pct_DLco_5y = case_when(dx == "IPF" & died == 1 ~ pct_DLco_3y * rnorm(1, 0.7, 0.1),
                            dx == "IPF" & died == 0 ~ pct_DLco_3y * rnorm(1, 0.8, 0.1),
                            died == 1               ~ pct_DLco_3y * rnorm(1, 0.8, 0.1),
                            died == 0               ~ pct_DLco_3y * rnorm(1, 0.9, 0.05)),
    # 呼吸機能は小数点以下2桁で丸める
    across(starts_with("FVC_")   , round, 2),
    across(starts_with("FEV1.0_"), round, 2),
    # %DLco は小数点以下1桁に丸める
    across(starts_with("pct_DLco_"), round, 1),
    # 観察期間がそれぞれの時点に満たないものは除外
    FVC_1y      = if_else(time < 12 * 1, NA_real_, FVC_1y),
    FVC_3y      = if_else(time < 12 * 3, NA_real_, FVC_3y),
    FVC_5y      = if_else(time < 12 * 5, NA_real_, FVC_5y),
    FEV1.0_1y   = if_else(time < 12 * 1, NA_real_, FEV1.0_1y),
    FEV1.0_3y   = if_else(time < 12 * 3, NA_real_, FEV1.0_3y),
    FEV1.0_5y   = if_else(time < 12 * 5, NA_real_, FEV1.0_5y),
    pct_DLco_1y = if_else(time < 12 * 1, NA_real_, pct_DLco_1y),
    pct_DLco_3y = if_else(time < 12 * 3, NA_real_, pct_DLco_3y),
    pct_DLco_5y = if_else(time < 12 * 5, NA_real_, pct_DLco_5y),
    # 低下しすぎた場合は欠測
    across(starts_with("FVC_")   , \(x) if_else(x < 0.7, NA_real_, x)),
    across(starts_with("FEV1.0_"), \(x) if_else(x < 0.4, NA_real_, x)),
    # FVC欠測の場合は、FEV1, DLcoとも欠測
    FEV1.0_0y   = if_else(is.na(FVC_0y), NA_real_, FEV1.0_0y),
    FEV1.0_1y   = if_else(is.na(FVC_1y), NA_real_, FEV1.0_1y),
    FEV1.0_3y   = if_else(is.na(FVC_3y), NA_real_, FEV1.0_3y),
    FEV1.0_5y   = if_else(is.na(FVC_5y), NA_real_, FEV1.0_5y),
    pct_DLco_0y = if_else(is.na(FVC_0y), NA_real_, pct_DLco_0y),
    pct_DLco_1y = if_else(is.na(FVC_1y), NA_real_, pct_DLco_1y),
    pct_DLco_3y = if_else(is.na(FVC_3y), NA_real_, pct_DLco_3y),
    pct_DLco_5y = if_else(is.na(FVC_5y), NA_real_, pct_DLco_5y),
    # FVC低値の場合はDLco欠測
    pct_DLco_0y = if_else(FVC_0y < 1.5, NA_real_, pct_DLco_0y),
    pct_DLco_1y = if_else(FVC_1y < 1.5, NA_real_, pct_DLco_1y),
    pct_DLco_3y = if_else(FVC_3y < 1.5, NA_real_, pct_DLco_3y),
    pct_DLco_5y = if_else(FVC_5y < 1.5, NA_real_, pct_DLco_5y),
  ) %>% 
  ungroup() %>% 
  relocate(ends_with("_0y"), .after = everything()) %>% 
  relocate(ends_with("_1y"), .after = everything()) %>% 
  relocate(ends_with("_3y"), .after = everything()) %>% 
  relocate(ends_with("_5y"), .after = everything()) 

# test
data_followup %>% 
  mutate(delta_FVC_1y = FVC_1y - FVC_0y) %>% 
  coxph(Surv(time, died) ~ dx + sex + age + delta_FVC_1y,
        data = .) %>%
  gtsummary::tbl_regression(
    exponentiate = TRUE,
    conf.level   = 0.95
  )

# 完成品として一旦CSV保存
write.csv(data_followup, file = "ip_10yrs_data.csv", fileEncoding = "utf-8", na = "")
