#' Estimasi Maximum Likelihood untuk Distribusi Normal
#'
#' @description
#' Fungsi ini menghitung *Maximum Likelihood Estimator* (MLE) untuk parameter
#' distribusi Normal (rata-rata dan varians) dari sebuah vektor numerik.
#'
#' @details
#' Formula yang digunakan adalah:
#' \itemize{
#'   \item Estimator untuk rata-rata: \eqn{\hat{\mu} = \frac{1}{n}\sum_{i=1}^{n}x_i = \bar{x}}
#'   \item Estimator untuk varians: \eqn{\hat{\sigma}^2 = \frac{1}{n}\sum_{i=1}^{n}(x_i - \bar{x})^2}
#' }
#' Ini adalah estimator MLE standar untuk distribusi Normal.
#'
#' @param x Sebuah vektor numerik berisi data observasi.
#' @param na.rm Sebuah nilai logis yang menunjukkan apakah nilai `NA` harus
#'   dihilangkan sebelum perhitungan. Standarnya adalah `TRUE`.
#'
#' @return
#' Sebuah vektor numerik dengan nama yang berisi dua elemen:
#' \itemize{
#'   \item `mu_hat`: Estimasi rata-rata (\eqn{\hat{\mu}}).
#'   \item `sigma2_hat`: Estimasi varians (\eqn{\hat{\sigma}^2}).
#' }
#'
#' @export
#'
#' @examples
#' # Bangkitkan data sampel dari distribusi Normal
#' set.seed(123)
#' data_normal <- rnorm(100, mean = 10, sd = 2)
#'
#' # Hitung MLE
#' mle_normal(data_normal)
#'
#' # Contoh dengan data yang mengandung nilai NA
#' data_with_na <- c(data_normal, NA, NA)
#' mle_normal(data_with_na, na.rm = TRUE)

mle_normal <- function(x, na.rm = TRUE) {
  # Validasi input
  if (!is.numeric(x)) {
    stop("Input 'x' harus berupa vektor numerik.")
  }

  # Menghilangkan nilai NA jika diminta
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  # Cek jika data kosong setelah menghilangkan NA
  if (length(x) == 0) {
    stop("Tidak ada data valid yang tersedia untuk dihitung.")
  }

  n <- length(x)

  # MLE untuk rata-rata adalah rata-rata sampel
  mu_hat <- mean(x)

  # MLE untuk varians
  sigma2_hat <- sum((x - mu_hat)^2) / n

  # Mengembalikan hasil dalam bentuk vektor dengan nama
  return(c(mu_hat = mu_hat, sigma2_hat = sigma2_hat))
}
