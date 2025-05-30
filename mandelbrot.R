# マンデルブロ集合を描画する関数
mandelbrot <- function(xmin, xmax, ymin, ymax, width, height, max_iter) {
  # xとyの値を生成
  x <- seq(xmin, xmax, length.out = width)
  y <- seq(ymin, ymax, length.out = height)

  # 結果を格納する行列
  m <- matrix(0, nrow = height, ncol = width)

  # 各ピクセルについて計算
  for (i in 1:height) {
    for (j in 1:width) {
      c <- complex(real = x[j], imaginary = y[i])
      z <- 0 + 0i
      iter <- 0
      while (Mod(z) <= 2 && iter < max_iter) {
        z <- z^2 + c
        iter <- iter + 1
      }
      m[i, j] <- iter
    }
  }
  return(m)
}

# パラメータ設定
xmin <- -2.0
xmax <- 0.7
ymin <- -1.2
ymax <- 1.2
width <- 800
height <- 800
max_iter <- 100

# マンデルブロ集合の計算
m_set <- mandelbrot(xmin, xmax, ymin, ymax, width, height, max_iter)

# 描画
image(m_set, col = rev(rainbow(max_iter)), axes = FALSE,
      main = "Mandelbrot Set",
      sub = paste("Iterations:", max_iter))
