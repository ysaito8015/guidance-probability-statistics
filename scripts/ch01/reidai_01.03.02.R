# ガイダンス確率統計(サイエンス社)
# 例題1.3.2
# 作成者：古川洸佑，守屋羽雪 (監修：石谷謙介)
# 作成日：2023-05-31
# 改訂日：2023-05-31 (改訂：石谷謙介)

n <- 100000  # 試行回数
b1_denominator <- 0
b1_numerator <- 0
b2_denominator <- 0
b2_numerator <- 0
b3_denominator <- 0
b3_numerator <- 0

for(i in 1:n){
  x <- sample(c(0,1),1)  # 上の子のデータ (0は男, 1は女)
  y <- sample(c(0,1),1)  # 下の子のデータ (0は男, 1は女)

  # (1)
  if(x == 0){  # 上の子が男
    b1_denominator <- b1_denominator + 1
    if(y == 0){  # 下の子も男
      b1_numerator <- b1_numerator + 1
    }
  }

  # (2)
  if(x * y == 0){  # 少なくとも1人が男
    b2_denominator <- b2_denominator + 1
    if(x + y == 0){  # どちらも男
      b2_numerator <- b2_numerator + 1
    }
  }

  # (3)
  z <- sample(c(0,1),1)  # 0なら無作為に選んだ子が上の子, 1なら下の子
  if((z == 0 && x == 0)||(z == 1 && y == 0)){
    b3_denominator <- b3_denominator + 1
    if(z == 0 && y == 0){
      b3_numerator <- b3_numerator + 1
    }
    if(z == 1 && x == 0){
      b3_numerator <- b3_numerator + 1
    }
  }
}

# (1) 確率を表示
b1_numerator / b1_denominator  # モンテカルロ法
1/2  # 理論値

# (2) 確率を表示
b2_numerator / b2_denominator  # モンテカルロ法
1/3  # 理論値

# (3) 確率を表示
b3_numerator / b3_denominator  # モンテカルロ法
1/2  # 理論値
