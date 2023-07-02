# ガイダンス確率統計(サイエンス社)
# トピックス1(モンティ・ホール問題)
# 作成者：古川洸佑，守屋羽雪 (監修：石谷謙介)
# 作成日：2023-05-14
# 改訂日：2023-05-31 (改訂：石谷謙介)

n <- 100000  # 試行回数
k <- 3  # 扉の枚数(k枚の扉は1,2,...,kで表現)
n_sw <- 0  # スイッチしたときの当選回数

for(i in 1:n){

  # 商品が隠された扉
  true_prize <- sample(1:k, 1)
  # 挑戦者が最初に選ぶ扉
  original_choice <- sample(1:k, 1)

  # 司会者が開ける扉
  open_vec <- c(1:k)

  # 挑戦者が最初に当たりを選んだ場合
  if(original_choice == true_prize){
    # open_vec(k次元)からoriginal_choiceを除く
    open_vec <- setdiff(open_vec, original_choice)
    # open_vec(k-1次元)のうち，閉めておく1つを選ぶ
    unopened_vec <- sample(open_vec, 1)
    # unopened_vec = 閉めたままの2つの扉
    unopened_vec <- c(unopened_vec, original_choice)
  } else {  # 挑戦者が最初にはずれを選んだ場合
    # unopened_vec = 最初に選んだ扉と当たりの扉
    unopened_vec <- c(original_choice, true_prize)
  }

  # スイッチした後の扉
  the_other_unopened <- setdiff(unopened_vec, original_choice)

  # スイッチした後の扉が当たり
  if(the_other_unopened == true_prize) {
    n_sw <- n_sw + 1
  }
}

# スイッチして当選する確率を表示
n_sw / n  # モンテカルロ
(k - 1) / k  # 理論解
