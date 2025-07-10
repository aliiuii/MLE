#' Estimasi Maximum Likelihood untuk Distribusi Eksponensial
#'
#' @description
#' Fungsi ini menghitung *Maximum Likelihood Estimator* (MLE) untuk parameter
#' *rate* (\eqn{\lambda}) dari distribusi Eksponensial berdasarkan sebuah vektor numerik.
#'
#' @details
#' Formula yang digunakan adalah estimator MLE untuk parameter rate:
#' \eqn{\hat{\lambda} = \frac{1}{\bar{x}} = \frac{n}{\sum_{i=1}^{n}x_i}}
#'
#' @param x Sebuah vektor numerik berisi data observasi. Nilai harus non-negatif.
#' @param na.rm Sebuah nilai logis yang menunjukkan apakah nilai `NA` harus
#'   dihilangkan sebelum perhitungan. Standarnya adalah `TRUE`.
#'
#' @return
#' Sebuah vektor numerik dengan nama yang berisi satu elemen:
#' \itemize{
#'   \item `lambda_hat`: Estimasi parameter *rate* (\eqn{\hat{\lambda}}).
#' }
#'
#' @export
#'
#' @examples
#' # Bangkitkan data sampel dari distribusi Eksponensial
#' set.seed(456)
#' data_exp <- rexp(100, rate = 2)
#'
#' # Hitung MLE
#' mle_exp(data_exp)
#'
#' # Contoh dengan data yang mengandung nilai NA dan nol
#' data_with_na_and_zero <- c(data_exp, NA, 0)
#' mle_exp(data_with_na_and_zero, na.rm = TRUE)
mle_exp <- function(x, na.rm = TRUE) {
  # Validasi input
  if (!is.numeric(x)) {
    stop("Input 'x' harus berupa vektor numerik.")
  }

  # Menghilangkan nilai NA jika diminta
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  # Cek nilai non-negatif
  if (any(x < 0)) {
    stop("Semua nilai dalam 'x' harus non-negatif untuk distribusi Eksponensial.")
  }

  # Cek jika data kosong setelah menghilangkan NA
  if (length(x) == 0) {
    stop("Tidak ada data valid yang tersedia untuk dihitung.")
  }

  # MLE untuk lambda adalah kebalikan dari rata-rata sampel
  sample_mean <- mean(x)

  # Hindari pembagian dengan nol jika semua observasi adalah 0
  if (sample_mean == 0) {
    stop("Tidak dapat menghitung lambda: rata-rata sampel adalah nol.")
  }

  lambda_hat <- 1 / sample_mean

  # Mengembalikan hasil dalam bentuk vektor dengan nama
  return(c(lambda_hat = lambda_hat))
}
