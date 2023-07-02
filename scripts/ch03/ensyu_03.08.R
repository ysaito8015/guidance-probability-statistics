# ガイダンス確率統計(サイエンス社)
# 演習3.8 (クーポンコレクター問題)
# 作成者：小川航世・小城一翔 (監修：石谷謙介)
# 作成日：2023-02-03
# 改訂日：2023-05-31 (改訂：石谷謙介)

m <- 100000         # 試行回数
n <- 5              # 景品の種類
Yn <- integer(m)    # n種類の景品をすべて集めるまでの購入回数を格納するベクトル

for (i in 1:m) {
  num_of_purchases <- 0  # 購入回数
  coupon_vec <- integer(n)  # n次元ベクトル(初期値は(0,0,...,0))

  while (sum(coupon_vec) != n) {
    k <- sample.int(n, 1)   # 1からnまでの整数からランダムに1つ取得
    coupon_vec[k] <- 1      # k番目の要素を1とし，k番目の景品を取得済みと見做す
    num_of_purchases <- num_of_purchases + 1
  }

  Yn[i] <- num_of_purchases
}

# 平均と分散を計算(理論値)
t <- sum(1 / 1:n)
mean_theoretical <- n * t
u <- sum((1:(n-1)) / (n - (1:(n-1)))^2)
var_theoretical <- n * u

# 平均を表示
mean(Yn)  # モンテカルロ法
mean_theoretical  # 理論値

# 分散を表示
var(Yn)  # モンテカルロ法
var_theoretical  # 理論値


