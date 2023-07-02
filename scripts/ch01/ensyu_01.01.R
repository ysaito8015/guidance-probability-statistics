# ガイダンス確率統計(サイエンス社)
# 演習1.1
# 作成者：古川洸佑，守屋羽雪 (監修：石谷謙介)
# 作成日：2023-05-14
# 改訂日：2023-05-31 (改訂：石谷謙介)

n <- 300000  # 試行回数
# A, B, C, Dの4人とも自分の名刺が入った封筒を選んだ状態
individual_cards <- c('card_A', 'card_B', 'card_C', 'card_D')

x <- rep(0, n)  # 全員が他人の名刺が入った封筒を選んだ場合は1，そうでない場合は0
for (i in 1:n) {
  selected_cards <- sample(individual_cards, 4, replace = FALSE)  # カードを4つ選ぶ
  if (all(selected_cards != individual_cards)) {
    x[i] <- 1  # 全員が他人の名刺が入った封筒を選んだ場合
  }
}

# 確率の表示
sum(x == 1) / n  # モンテカルロ法
3/8  # 理論値
