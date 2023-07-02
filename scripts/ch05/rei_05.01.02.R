# ガイダンス確率統計(サイエンス社)
# 例5.1.2 (定積分の計算)
# 作成者：小川航世 (監修：石谷謙介)
# 作成日：2023-02-03
# 改訂日：2023-05-31 (改訂：石谷謙介)

a <- pi  # 円周率π

# f(x)の定義（f(x)=sin(x)）
f <- function(x) {
  sin(x)
}

# モンテカルロ法による積分計算
monte_carlo <- function(n) {
  sigma <- 0
  for (i in 1:n) {
    Z <- runif(1, 0, a)  # U(0,a)に従う乱数をZに代入
    sigma <- sigma + f(Z)
  }
  sigma / n
}

# プロット関数
mc_plot <- function(n){
  y <- c()
  for(i in 1:n){
    y <- c(y, monte_carlo(i)) # ベクトルyにmonte(i)(i=1,...,n)を順に代入
  }
  plot(y, main = paste("n =", n), xlab = "評価点の数", ylab = "モンテカルロ積分", type="l") # (i,y[i])をプロットし，直線で結ぶ
}

mc_plot(1000)

sekibun <- 2  # ∫[0,a]f(x)dx(=∫[0,π]sin(x))dxの理論値

# モンテカルロ法で計算した積分値が，積分の理論値(青字)に収束する様子を描画
abline(h = sekibun / a, col = "blue")
