library(tidyverse)

# インストールされていなければ systemfonts パッケージをインストール
# よくある !require() はインストール済の場合パッケージをロードするが、
# 今回は sytemfonts をロードする程ではないので別の方法で未インストール検知
if (!("systemfonts" %in% installed.packages()[,"Package"])) {
  install.packages("systemfonts")
}

# グラフに使う sans 系（ゴシック体）のフォント候補
# 一番最後の "sans" はフェイルセーフ
sans_candidate <- c(
  "Yu Gothic",
  "YuGothic",
  "Hiragino Sans",
  "Hiragino Kaku Gothic ProN",
  "HiraKakuProN-W3",
  "Noto Sans CJK JP",
  "Noto Sans JP",
  "MS Gothic",
  "sans"
)

# システムで認識しているフォントファミリー名と候補を突き合わせる
# 候補で先にあるものが優先、どれも該当しない場合は最後の "sans"
font_sans <- 
  # システムで認識しているフォントのファミリー名を抽出
  systemfonts::system_fonts() %>% 
  pull(family) %>% 
  unique() %>% 
  # 上のリストと合致した中で、リスト内での登場順が一番早いものを採用
  match(sans_candidate, nomatch = length(sans_candidate)) %>%
  min(na.rm = TRUE) %>% 
  sans_candidate[.]

# フォントとOSの情報（macOS は Darwin ？）
font_info <- sprintf("[Font: %s, System: %s]",
                     font_sans,
                     Sys.info()["sysname"])

# sample
diamonds %>% 
  ggplot(aes(x = carat, y = price, colour = cut)) +
    geom_point() +
    ggtitle(paste("ダイヤモンドのサイズと価格", font_info)) +
    theme_gray(base_family = font_sans)

# conclusion:
# font_sans <- switch(Sys.info()["sysname"],
#                     "Windows" = "Yu Gothic",
#                     "Darwin"  = "Hiragino Sans",
#                     "Noto Sans CJK JP")
