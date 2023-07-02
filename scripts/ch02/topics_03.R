# ガイダンス確率統計(サイエンス社)
# トピックス3 (逆関数法)
# 作成者：小川航世・小城一翔 (監修：石谷謙介)
# 作成日：2023-02-03
# 改訂日：2023-05-31 (改訂：石谷謙介)

rm(list = ls())
gc()

source(here::here("scripts", "environments.R"))
.libPaths(libDir)

n <- 100000  # 試行回数
ξ <- runif(n)  # U(0,1)に従う乱数をn個生成

# 指数分布
print("exponential distribution")
λ <- 1
X <- -1/λ * log(1 - ξ)

# ヒストグラムと密度関数の比較
pdf(
    file = file.path(figDir, "ch02", "fig02-01.pdf"),
)
  hist(
    x = X,
    main = expression(paste("Exp(", lambda, ")", sep = "")),
    freq = FALSE
  )
  curve(dexp(x, λ), from = 0, add = TRUE)
dev.off()

# 一様分布
print("uniform distribution")
c <- -1
d <- 2
X <- (d - c) * ξ + c

# ヒストグラムと密度関数の比較
pdf(
    file = file.path(figDir, "ch02", "fig02-02.pdf"),
)
  hist(X, main = "U(c,d)", freq = FALSE)
  curve(dunif(x, c, d), from = 0, add = TRUE)
dev.off()
