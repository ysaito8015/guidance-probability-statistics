# ガイダンス確率統計(サイエンス社)
# 例7.3.1 
# ※ 書籍では，正規母集団の母分散の両側検定において，簡便的な考え方に基づき棄却域(7.39)を求めたが，本プログラムでは，(7.7)の考え方に基づき厳密に棄却域を求める．
# 作成者：小川航世・小城一翔 (監修：石谷謙介)
# 作成日：2023-02-03
# 改訂日：2023-05-31 (改訂：石谷謙介)

# 書籍の(7.28)式
f_s0sq <- function(m, x) {
    1 / (2^(m/2) * gamma(m/2)) * x^(m/2-1) * exp(-x/2)
}

# 書籍の(7.28)式の関数を区間[r1,r2]上で積分(分割数はmesh_size)
Integrate_f_s0sq <- function(m, r1, r2, mesh_size) {
    Intgf <- 0
    delta <- (r2 - r1)/mesh_size
    
    for(i in 1:mesh_size){  # forループ
        x <- r1 + (i * delta)
        Intgf <- Intgf + (f_s0sq(m, x) * delta)
    }
    
    return(Intgf)
}

# 書籍のhm(x)
hm <- function(m, x) {
    (x/m)^(m/2) * exp((m-x)/2)
}

# 0<c<1に対し，2分探索法を用いてhm(x)=cをみたすx(<m)を求める関数
hm_inv_left <- function(m, c) {
    rmin <- 0
    rmax <- m
    rmid <- (rmin + rmax) / 2
    
    for (i in 1:25) { # 2分探索法:開始
        if (hm(m, rmid) < c) {
            rmin <- rmid
        } else {
            rmax <- rmid
        }
        rmid <- (rmin + rmax) / 2
    } # 2分探索法:終了
    
    return(rmid)
}

# 0<c<1に対し，2分探索法を用いてhm(x)=cをみたすx(>m)を求める関数
hm_inv_right <- function(m, c) {
    rmin <- m
    rmax <- 2 * m
    
    while (hm(m, rmax) > c) { # hm(x)=cをみたすxが含まれる区間[k*m,(k+1)*m]を探す
        rmin <- rmax 
        rmax <- rmax + m
    }
    
    rmid <- (rmin + rmax) / 2
    
    for (i in 1:25) { # 2分探索法:開始
        if (hm(m, rmid) < c) {
            rmax <- rmid
        } else {
            rmin <- rmid
        }
        rmid <- (rmin + rmax) / 2
    } # 2分探索法:終了
    
    return(rmid)
}

# αとmに対し，2分探索法を用いて，hm(r1)=hm(r2)かつ
# ∫[0,r1]f_s0^2(x)dx+∫[r2,∞]f_s0^2(x)dx=α
# をみたすr1,r2の表を出力（m=1,...,30）
α <- 0.1 # 有意水準
meshsize1 <- 20000 # [0, r1]の分割数
meshsize2 <- 10000 # [r2, 30 * r2]の分割数
emu <- c()
rone <- c()
rtwo <- c()

for(m in 1:30){ # forループ
    cmin <- 0
    cmax <- 1
    
    for(i in 1:20){ # 2分探索法
        cmid <- (cmin+cmax)/2
        r1 <- hm_inv_left(m, cmid)
        r2 <- hm_inv_right(m, cmid)
        r3 <- 30 * r2
        I1 <- Integrate_f_s0sq(m, 0, r1, meshsize1) # I1=∫[0,r1]f_s0^2(x)dx
        I2 <- Integrate_f_s0sq(m, r2, r3, meshsize2) # I2=∫[r2,∞]f_s0^2(x)dx
        
        if(I1+I2>α){
            cmax <- cmid
        }else{
            cmin <- cmid
        }
    } # 2分探索法：終了
    
    emu <- c(emu, m)
    rone <- c(rone, r1)
    rtwo <- c(rtwo, r2)
} # forループ:終了

data.frame(m=emu, r1=rone, r2=rtwo)


# 参考：hm(r1)=hm(r2)=i/N (i=1,...,N)をみたすr1,r2に対し，
# I=∫[0,r1]f_s0^2(x)dx+∫[r2,∞]f_s0^2(x)dxのグラフを描画
m <- 1
N <- 100 # 横軸cの分割数
I1_graph <- 0
I2_graph <- 0
I_graph <- 0
meshsize1 <- 10000 # [0, r1]の分割数
meshsize2 <- 10000 # [r2, 50 * r2]の分割数
for(i in 1:N){ # forループ
    c <- i/N
    r1 <- hm_inv_left(m, c)
    r2 <- hm_inv_right(m, c)
    r3 <- 50 * r2
    I1 <- Integrate_f_s0sq(m, 0, r1, meshsize1)   # I1=∫[0,r1]f_s0^2(x)dx
    I2 <- Integrate_f_s0sq(m, r2, r3, meshsize2)  # I2=∫[r2,∞]f_s0^2(x)dx
    I1_graph <- c(I1_graph, I1)
    I2_graph <- c(I2_graph, I2)
    I_graph <- c(I_graph, I1+I2)
}

x <- seq(0, 1, by=1/N) # x=(0,1/N,2/N,...,1)

# 積分の値を順にプロットし，直線で結ぶ
plot(x = x, I_graph, main = "m = 1", xlab = "c", type = "l", col = 1, xlim = c(0, 1), ylim = c(0, 1))
par(new = T)
plot(x = x, I1_graph, main = "m = 1", xlab = "c", type = "l", col = 2, xlim = c(0, 1), ylim = c(0, 1), ann = F)
par(new = T)
plot(x = x, I2_graph, main = "m = 1", xlab = "c", type = "l", col = 3, xlim = c(0, 1), ylim = c(0, 1), ann = F)