# ガイダンス確率統計(サイエンス社)
# 例題3.2.1 (4枚のカード)
# 作成者：古川洸佑，守屋羽雪 (監修：石谷謙介)
# 作成日：2023-06-09
# 改訂日：2023-06-12 (改訂：石谷謙介)

n <- 200000  # 試行回数
X <- numeric(n)  # A君のカードの数字
Y <- numeric(n)  # B君のカードの数字

for (i in 1:n) {
  cards <- c(-2, -1, -1, 2)  # 4枚のカードの情報
  k_a <- sample(1:4, 1)  # A君が抜き出すカードの添え字を{1,2,3,4}から無作為に選ぶ
  X[i] <- cards[k_a]
  cards <- cards[-k_a]  # cardsからcards[k_a]を抜く
  k_b <- sample(1:3, 1)  # B君が抜き出すカードの添え字を{1,2,3}から無作為に選ぶ
  Y[i] <- cards[k_b]
}

# E(XY)を表示
mean(X * Y)  # モンテカルロ法
-1/2  # 理論値

# E(X)E(Y)を表示
mean(X) * mean(Y)  # モンテカルロ法
1/4  # 理論値

# V(X+Y)を表示
var(X + Y)  # モンテカルロ法
3  # 理論値

# V(X)+V(Y)を表示
var(X) + var(Y)  # モンテカルロ法 
9/2  # 理論値
