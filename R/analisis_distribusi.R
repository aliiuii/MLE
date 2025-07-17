#' Analisis dan Visualisasi Kecocokan Distribusi
#'
#' @description
#' Fungsi ini menganalisis sebuah set data untuk menentukan apakah distribusi
#' Normal atau Eksponensial lebih cocok. Penentuan ini didasarkan pada
#' Akaike's Information Criterion (AIC). Fungsi ini juga menghasilkan plot
#' visual berupa histogram dengan kurva densitas dari kedua distribusi.
#'
#' @details
#' Proses yang dilakukan fungsi ini adalah:
#' 1. Menghitung MLE untuk parameter kedua distribusi menggunakan fungsi
#'    `mle_normal()` dan `mle_exp()`.
#' 2. Menghitung nilai *log-likelihood* (\eqn{\ell}) untuk setiap model.
#' 3. Menghitung AIC untuk setiap model dengan formula: \eqn{AIC = 2k - 2\ell},
#'    di mana \eqn{k} adalah jumlah parameter model (2 untuk Normal, 1 untuk Eksponensial).
#' 4. Model dengan nilai AIC yang **lebih rendah** dianggap lebih cocok.
#' 5. Membuat histogram data dan menempatkan kurva densitas dari kedua model
#'    di atasnya untuk perbandingan visual.
#'
#' @param x Sebuah vektor numerik berisi data observasi.
#' @param na.rm Sebuah nilai logis untuk menghilangkan `NA`. Standarnya `TRUE`.
#'
#' @return
#' Sebuah `list` yang berisi hasil analisis:
#' \itemize{
#'   \item `hasil_normal`: List berisi `estimators`, `log_likelihood`, dan `AIC` untuk model Normal.
#'   \item `hasil_eksponensial`: List berisi `estimators`, `log_likelihood`, dan `AIC` untuk model Eksponensial. (Akan bernilai NA jika data mengandung nilai negatif).
#'   \item `rekomendasi_model`: String karakter yang menyatakan model terbaik berdasarkan AIC.
#' }
#' Fungsi ini juga akan menghasilkan sebuah plot pada *graphics device* aktif.
#'
#' @export
#'
#' @examples
#' # Contoh 1: Data yang lebih cocok dengan distribusi Normal
#' set.seed(1)
#' data_norm <- rnorm(200, mean = 10, sd = 2)
#' hasil_analisis_1 <- analisis_distribusi(data_norm)
#' print(hasil_analisis_1$rekomendasi_model)
#'
#' # Contoh 2: Data yang lebih cocok dengan distribusi Eksponensial
#' set.seed(2)
#' data_exp <- rexp(200, rate = 0.5)
#' hasil_analisis_2 <- analisis_distribusi(data_exp)
#' print(hasil_analisis_2$rekomendasi_model)

analisis_distribusi <- function(x, na.rm = TRUE) {
  # === Validasi Input ===
  if (!is.numeric(x)) stop("Input 'x' harus berupa vektor numerik.")
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0) stop("Tidak ada data valid yang tersedia.")

  # === Analisis Distribusi Normal ===
  params_norm <- mle_normal(x)
  mu_hat <- params_norm["mu_hat"]
  sigma_hat <- sqrt(params_norm["sigma2_hat"])

  loglik_norm <- sum(dnorm(x, mean = mu_hat, sd = sigma_hat, log = TRUE))
  k_norm <- 2 # Jumlah parameter (mu dan sigma^2)
  aic_norm <- 2 * k_norm - 2 * loglik_norm

  hasil_normal <- list(estimators = params_norm, log_likelihood = loglik_norm, AIC = aic_norm)

  # === Analisis Distribusi Eksponensial ===
  # Hanya dijalankan jika semua data non-negatif
  if (all(x >= 0)) {
    params_exp <- mle_exp(x)
    lambda_hat <- params_exp["lambda_hat"]

    loglik_exp <- sum(dexp(x, rate = lambda_hat, log = TRUE))
    k_exp <- 1 # Jumlah parameter (lambda)
    aic_exp <- 2 * k_exp - 2 * loglik_exp

    hasil_eksponensial <- list(estimators = params_exp, log_likelihood = loglik_exp, AIC = aic_exp)
  } else {
    # Jika ada data negatif, analisis eksponensial tidak valid
    hasil_eksponensial <- list(estimators = NA, log_likelihood = NA, AIC = NA)
    aic_exp <- Inf # Set AIC ke tak hingga agar Normal selalu terpilih
  }

  # === Penentuan Model Terbaik ===
  if (aic_norm < aic_exp) {
    rekomendasi <- "Normal"
  } else if (aic_exp < aic_norm) {
    rekomendasi <- "Eksponensial"
  } else {
    rekomendasi <- "Keduanya memiliki AIC yang sama"
  }

  # === Membuat Plot ===
  hist(x, freq = FALSE, breaks = "Sturges",
       main = paste("Rekomendasi Model:", rekomendasi),
       xlab = "Nilai Data", ylab = "Densitas",
       col = "lightgray", border = "white")

  # Kurva Normal
  curve(dnorm(x, mean = mu_hat, sd = sigma_hat), col = "blue", lwd = 2, add = TRUE)

  # Kurva Eksponensial (jika valid)
  if (all(x >= 0)) {
    curve(dexp(x, rate = lambda_hat), col = "red", lwd = 2, add = TRUE)
    legend_labels <- c("Normal", "Eksponensial")
    legend_colors <- c("blue", "red")
  } else {
    legend_labels <- "Normal"
    legend_colors <- "blue"
  }

  legend("topright", legend = legend_labels, col = legend_colors, lwd = 2, bty = "n")

  # === Struktur Output ===
  output <- list(
    hasil_normal = hasil_normal,
    hasil_eksponensial = hasil_eksponensial,
    rekomendasi_model = rekomendasi
  )

  return(invisible(output)) # Mengembalikan output tanpa mencetaknya otomatis
}
