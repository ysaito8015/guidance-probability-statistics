# ガイダンス確率統計(サイエンス社)
# 演習2.10
# 作成者：古川洸佑，守屋羽雪 (監修：石谷謙介)
# 作成日：2023-06-02
# 改訂日：2023-06-05 (改訂：石谷謙介)

n <- 100000              # 試行回数
triangleAreas <- c()     # 三角形OABの面積を格納するベクトル

for (i in 1:n) {
  argA <- runif(1, 0, 2 * pi)  # 線分OAとx軸とのなす角
  argB <- runif(1, 0, 2 * pi)  # 線分OBとx軸とのなす角

  # 三角形OABの面積 = |sin(argA - argB)| / 2
  triangleAreas[i] <- abs(sin(argA - argB)) / 2  
}

# 三角形OABの面積の期待値を表示
mean(triangleAreas)  # モンテカルロ法
1 / pi  # 理論値

