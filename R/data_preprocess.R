#' Normalize Fluorescence
#'
#' normalize() reads in raw_data. This function normalizes data by standardizing
#'     them according to maximum and minimum fluorescence per well, with maximum
#'     set to 1 and minimum set to 0. It also reformats data types by checking
#'     for potential error. i.e. a string specifying 100,000 will be read in
#'     as number, 100000, without issue.
#'     Function is applicable only to data of a single well, do not call on
#'     an entire data frame of all 96 well data. It is intended for single well
#'     screening purposes.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
#'
#' @param raw_data data frame; raw dataset input, should be of only one well.
#'     If multiple wells need to be normalized, use \code{\link{gam_analysis}}()
#'     for 96 well application. If only preliminary screening is needed, use
#'     \code{\link[TSAR]{screen}}().
#' @param fluo integer; the Fluorescence variable column id (e.g. fluo = 5
#'     when 5th column of the data frame is the Fluorescence value)
#'     if fluorescence variable is named exactly as "Fluorescence", fluo does
#'     not need to be specified. i.e. fluo is set to NA by default,
#'     suggesting the variable is named "Fluorescence".
#' @param selected list of character strings;
#'     variables from the original data set users intend to keep.
#'     Variable default set to c("Well.Position", "Temperature", "Fluorescence",
#'     "Normalized") if not otherwise specified. If data frame variables are
#'     named differently, user needs to specify what column variables to keep.
#' @return cleaned up data framed with selected columns
#' @examples
#' data("qPCR_data1")
#' test <- subset(qPCR_data1, Well.Position == "A01")
#' normalize(test)
#'
#' @family data_preprocess
#' @export
normalize <- function(
    raw_data,
    fluo = NA,
    selected = c(
        "Well.Position",
        "Temperature",
        "Fluorescence",
        "Normalized"
    )) {
    #format inputs and cast them into desired types
    norm_data <- raw_data %>%
        transform(
            Fluorescence =
                as.double(gsub(",", "", raw_data$Fluorescence))
        ) %>%
        transform(
            Temperature =
                as.double(gsub(",", "", raw_data$Temperature))
        ) %>%
        transform(Well.Position = paste(
            substr(raw_data$Well.Position, 1, 1),
            sprintf("%02s", substr(
                raw_data$Well.Position, 2,
                nchar(raw_data$Well.Position)
            )),
            sep = ""
        ))

    # normalize fluorescence by min max
    if (is.na(fluo)) {
        min_f <- min(norm_data$Fluorescence)
        max_f <- max(norm_data$Fluorescence)
        norm_data <- norm_data %>%
            mutate(
                Normalized =
                    (norm_data$Fluorescence - min_f) / (max_f - min_f)
            ) %>%
            dplyr::select(all_of(selected))
    } else {
        norm_data <- norm_data %>%
            mutate(
                Normalized =
                    (norm_data[[fluo]] - min(norm_data[[fluo]])) /
                        (max(norm_data[[fluo]]) - min(norm_data[[fluo]]))
            ) %>%
            dplyr::select(all_of(selected))
    }
}


#' Numerical Tm by treating Tm as inflection point
#'
#' Estimates the inflection point (Tm) as the (x, y) coordinate where the
#' discrete second derivative changes sign (zero-crossing). Among all
#' zero-crossings, chooses the one with the largest absolute first derivative
#' (steepest slope). Uses linear interpolation between adjacent points to refine
#' the zero of the second derivative.
#'
#' @param data data.frame with numeric columns x and y (specified by names).
#'   Data will be sorted by x; duplicated x's are averaged.
#' @param x,y character; names of the predictor and response columns.
#' @return Named numeric vector \code{c(x = x_Tm, y = y_Tm)}; \code{NA}s on failure.
#' @examples
#' # tm_xy <- numerical_Tm(df, x = "Temperature", y = "Normalized")
numerical_Tm <- function(data, x, y) {
  stopifnot(x %in% names(data), y %in% names(data))
  
  # keep finite values only
  df <- data[is.finite(data[[x]]) & is.finite(data[[y]]), c(x, y)]
  names(df) <- c("x", "y")
  if (nrow(df) < 3L) return(c(x = NA_real_, y = NA_real_))
  
  # sort by x and average duplicates to avoid zero spacing
  df <- df[order(df$x), , drop = FALSE]
  if (anyDuplicated(df$x)) {
    # aggregate duplicates by mean y
    df <- aggregate(y ~ x, data = df, FUN = mean)
  }
  
  n <- nrow(df)
  if (n < 3L) return(c(x = NA_real_, y = NA_real_))
  
  # spacing and finite differences
  dx <- diff(df$x)
  if (any(dx <= 0)) return(c(x = NA_real_, y = NA_real_))  # safety (sorted unique should prevent)
  
  # central first derivative (dy/dx) at interior points i=2..n-1
  dy_c <- (df$y[3:n] - df$y[1:(n - 2)]) / (df$x[3:n] - df$x[1:(n - 2)])
  # central second derivative at interior i=2..n-1:
  # d2 ~ ( (y_{i+1}-y_i)/h2 - (y_i - y_{i-1})/h1 ) / ((h1 + h2)/2)
  h1 <- df$x[2:(n - 1)] - df$x[1:(n - 2)]
  h2 <- df$x[3:n]       - df$x[2:(n - 1)]
  d1 <- (df$y[2:(n - 1)] - df$y[1:(n - 2)]) / h1
  d2 <- (df$y[3:n]       - df$y[2:(n - 1)]) / h2
  d2_c <- (d2 - d1) / ((h1 + h2) / 2)
  
  # indices for interior points
  ii <- 2:(n - 1)
  
  # zero-crossings of d2_c between consecutive interior indices
  sgn <- sign(d2_c)
  zc_idx <- which(sgn[-length(sgn)] * sgn[-1L] < 0)  # sign change between (i,i+1) in interior space
  if (!length(zc_idx)) {
    # fallback: steepest slope among interior central differences
    k <- which.max(abs(dy_c))
    # Tm at the interior point corresponding to k (index ii[k])
    i_star <- ii[k]
    return(c(x = df$x[i_star], y = df$y[i_star]))
  }
  
  # For each zero-crossing between interior indices (ii[zc] and ii[zc+1]),
  # linearly interpolate root of d2_c and evaluate y on the segment between raw x's.
  cand_x <- numeric(length(zc_idx))
  cand_y <- numeric(length(zc_idx))
  cand_slope <- numeric(length(zc_idx))
  
  for (j in seq_along(zc_idx)) {
    k <- zc_idx[j]               # in 1..length(d2_c)-1
    iL <- ii[k]                  # left raw index (in 2..n-2)
    iR <- ii[k + 1L]             # right raw index
    
    # interpolate zero of d2_c between k and k+1 in "interior grid":
    # d2_c[k] at x ~ df$x[iL]; d2_c[k+1] at x ~ df$x[iR]
    yL2 <- d2_c[k]; yR2 <- d2_c[k + 1L]
    xL  <- df$x[iL]; xR <- df$x[iR]
    if (!is.finite(yL2) || !is.finite(yR2) || yL2 == yR2) {
      x_star <- (xL + xR) / 2
    } else {
      x_star <- xL - yL2 * (xR - xL) / (yR2 - yL2)
    }
    x_star <- max(min(x_star, xR), xL)
    
    # interpolate y along the raw segment that contains x_star
    # choose the immediate raw segment that brackets x_star
    # here, use the segment [iL, iR] (they are adjacent or nearly adjacent)
    yL <- df$y[iL]; yR <- df$y[iR]
    y_star <- yL + (yR - yL) * (x_star - xL) / (xR - xL)
    
    # slope proxy: central first derivative magnitude near k
    slope_k <- abs(dy_c[k])
    
    cand_x[j] <- x_star
    cand_y[j] <- y_star
    cand_slope[j] <- slope_k
  }
  
  j_best <- which.max(cand_slope)
  c(x = cand_x[j_best], y = cand_y[j_best])
}

#' Beta-knot natural cubic spline model (Tm-centered via numerical_Tm)
#'
#' Fits a natural cubic spline to \code{y} as a function of \code{x} using
#' interior knots at Beta(a,a) quantiles, centered at the numerically estimated
#' Tm from \code{\link{numerical_Tm}}.
#'
#' @inheritParams numerical_Tm
#' @param beta_shape numeric > 0; Beta(a,a) shape parameter. Default 4.
#' @param beta_knots_frac numeric in (0,1); fraction of unique x used for
#'   interior knots if \code{beta_n_knots} is NULL. Default 0.008.
#' @param beta_n_knots integer or NULL; overrides fraction when provided.
#' @param use_natural logical; if TRUE (default) uses \code{splines::ns()},
#'   else \code{splines::bs(..., degree = 3)}.
#' @return \code{"lm"} fit with spline basis; attributes store knots and settings.
model_beta <- function(norm_data, x, y,
                       beta_shape = 4,
                       beta_knots_frac = 0.008,
                       beta_n_knots = NULL,
                       use_natural = TRUE) {
  # --- resolve names if user passed bare symbols
  .resolve_name <- function(arg) {
    if (is.character(arg) && length(arg) == 1) return(arg)
    deparse(substitute(arg))
  }
  x <- .resolve_name(x); y <- .resolve_name(y)
  
  xvec <- norm_data[[x]]
  xmin <- min(xvec, na.rm = TRUE)
  xmax <- max(xvec, na.rm = TRUE)
  if (!is.finite(xmin) || !is.finite(xmax) || xmax <= xmin) {
    stop("Invalid x range for model_beta().")
  }
  
  # --- center at numerical Tm (no GAM)
  tm_xy <- numerical_Tm(norm_data, x = x, y = y)
  tm_val <- tm_xy[["x"]]
  if (!is.finite(tm_val)) tm_val <- (xmin + xmax) / 2
  c_frac <- (tm_val - xmin) / (xmax - xmin)
  c_frac <- max(0, min(1, c_frac))
  s <- 2 * min(c_frac, 1 - c_frac)
  
  # --- number of interior knots
  if (is.null(beta_n_knots)) {
    S <- length(unique(stats::na.omit(xvec)))
    m <- max(1L, floor(beta_knots_frac * S))
  } else {
    m <- as.integer(beta_n_knots)
  }
  
  # --- build spline formula
  if (!is.finite(m) || m <= 0) {
    frm <- stats::as.formula(paste0(
      y, " ~ splines::ns(", x,
      ", df = 3, Boundary.knots = c(", xmin, ",", xmax, "))"
    ))
  } else {
    u <- seq_len(m) / (m + 1)
    q_beta <- stats::qbeta(u, shape1 = beta_shape, shape2 = beta_shape)
    q_star <- c_frac + (q_beta - 0.5) * s
    knots_x <- xmin + q_star * (xmax - xmin)
    
    if (isTRUE(use_natural)) {
      frm <- stats::as.formula(paste0(
        y, " ~ splines::ns(", x,
        ", knots = knots_x, Boundary.knots = c(", xmin, ",", xmax, "), intercept = FALSE)"
      ))
    } else {
      frm <- stats::as.formula(paste0(
        y, " ~ splines::bs(", x,
        ", knots = knots_x, degree = 3, Boundary.knots = c(", xmin, ",", xmax, "), intercept = FALSE)"
      ))
    }
  }
  
  fit <- stats::lm(frm, data = norm_data, x = TRUE, y = TRUE)
  
  # ---- augment: fitted and derivative on original rows
  # keep original order, but compute derivative on x-sorted rows
  df_aug <- norm_data
  # predict at original x
  df_aug$fitted <- stats::predict(fit, newdata = df_aug)
  
  # derivative from fitted vs x (central differences on sorted-by-x)
  ord <- order(df_aug[[x]])
  xo <- as.numeric(df_aug[[x]][ord])
  yo <- as.numeric(df_aug$fitted[ord])
  
  n <- length(xo)
  deriv <- rep(NA_real_, n)
  if (n >= 3) {
    # central difference: (y_{i+1}-y_{i-1})/(x_{i+1}-x_{i-1})
    deriv[2:(n - 1)] <- (yo[3:n] - yo[1:(n - 2)]) / (xo[3:n] - xo[1:(n - 2)])
    # optional: one-sided at ends
    deriv[1] <- (yo[2] - yo[1]) / (xo[2] - xo[1])
    deriv[n] <- (yo[n] - yo[n - 1]) / (xo[n] - xo[n - 1])
  }
  # put derivative back to original row order
  df_aug$norm_deriv <- NA_real_
  df_aug$norm_deriv[ord] <- deriv
  
  # If your view expects the response column named "Normalized" specifically,
  # ensure the column exists (duplicate y if named differently)
  if (y != "Normalized" && "Normalized" %in% names(df_aug) == FALSE) {
    df_aug$Normalized <- df_aug[[y]]
  }
  # Ensure Temperature column exists similarly
  if (x != "Temperature" && "Temperature" %in% names(df_aug) == FALSE) {
    df_aug$Temperature <- df_aug[[x]]
  }
  
  # attach some metadata if useful
  attr(df_aug, "beta_model") <- fit
  attr(df_aug, "beta_knots") <- if (exists("knots_x")) knots_x else numeric()
  attr(df_aug, "beta_shape") <- beta_shape
  attr(df_aug, "beta_center_frac") <- c_frac
  attr(df_aug, "Boundary.knots") <- c(xmin, xmax)
  
  # return augmented data frame so view_model() can consume it
  df_aug
}




#' Generalized Addidtive Modeling on TSA data
#'
#' Function finds fitted fluorescence values by imposing generalized
#'     additive model on fluorescence data by temperature. Model assumes
#'     method = "GACV.Cp" and sets to \code{formula = y ~ s(x, bs = "ad")}.
#'     Function inherits function from gam package, \code{\link{gam}}().
#'
#'
#' @importFrom magrittr %>%
#' @importFrom mgcv gam
#' @importFrom dplyr mutate select
#'
#' @param norm_data data frame input of only one well's reading, preferably
#'   normalized using \code{\link{normalize}}.
#' @param x temperature column
#' @param y normalized fluorescence column
#' @return data frame containing gam model fitted values
#'
#' @family data_preprocess
#' @examples
#' data("qPCR_data1")
#' test <- subset(qPCR_data1, Well.Position == "A01")
#' test <- normalize(test, fluo = 5, selected = c(
#'     "Well.Position", "Temperature",
#'     "Fluorescence", "Normalized"
#' ))
#' model_gam(test, x = test$Temperature, y = test$Normalized)
#'
#' @export
model_gam <- function(norm_data, x, y) {
    #model data with gam
    mgcv::gam(
        formula = y ~ s(x, bs = "ad"),
        data = norm_data,
        method = "GACV.Cp"
    )
}

#' Boltzmann Modeling on TSA data
#'
#' Function finds fitted fluorescence values by imposing Boltzmann function.
#'
#'
#' @importFrom minpack.lm nlsLM
#'
#' @param norm_data data frame input, preferably normalized using
#'     \code{\link{normalize}}.
#' @return dtaa frame containing gam model fitted values
#'
#' @family data_preprocess
#' @examples
#' data("qPCR_data1")
#' A01 <- subset(qPCR_data1, Well.Position == "A01")
#' A01 <- normalize(A01)
#' model_boltzmann(A01)
#'
#' @export
model_boltzmann <- function(norm_data) {
    #check for pre-written min max
    if (is.null(norm_data$minB) && is.null(norm_data$maxB)) {
        maxfluo <- 0
        minfluo <- 100
        temp <- norm_data
        trimmed <- norm_data
        stock <- norm_data
        while (maxfluo <= minfluo) {
            maxfluo <- temp$Temperature[which.max(temp$Fluorescence)]
            temp <- subset(norm_data, Temperature <= maxfluo)
            minfluo <- temp$Temperature[which.min(temp$Fluorescence)]
            temp <- subset(trimmed, Temperature >=
                min(trimmed$Temperature) + 10 &
                Temperature <= max(trimmed$Temperature) - 10)
            trimmed <- temp
        }
        norm_data$minB <- minfluo
        norm_data$maxB <- maxfluo + 5
        norm_data <- subset(norm_data, Temperature >= minfluo &
            Temperature <= maxfluo + 5)
    } else {
        norm_data <- subset(norm_data, Temperature >= norm_data$minB[1] &
            Temperature <= norm_data$maxB[1] + 5)
    }
    #normalize
    norm_data <- TSAR::normalize(norm_data)
    #fit boltzmann equation
    f <- minpack.lm::nlsLM(y ~ 1 / (1 + exp(-k * (x - x2))),
        data = data.frame(
            x = norm_data$Temperature,
            y = norm_data$Normalized
        ),
        start = list(k = 100, x2 = maxfluo)
    )
    #pack fitted numbers and summaries into one variable
    fitted <- unlist(f$m$fitted())
    fitted <- append(
        rep(0, which(stock$Temperature ==
            min(norm_data$Temperature))),
        fitted
    )
    fitted <- append(fitted, rep(1, (length(stock$Temperature) -
        length(fitted))))
    f$fitted.values <- fitted
    return(f)
}

#' Run Boltzmann Modeling
#'
#' Function runs function \code{model_boltzmann()} and raises warning when
#'     modeling generates error or warnings.
#'
#' @param norm_data data frame input, preferably normalized using
#'     \code{\link{normalize}}.
#' @return data frame containing gam model fitted values
#'
#' @family data_preprocess
#' @examples
#' data("qPCR_data1")
#' A01 <- subset(qPCR_data1, Well.Position == "A01")
#' A01 <- normalize(A01)
#' run_boltzmann(A01)
#'
#' @export
run_boltzmann <- function(norm_data) {
    tryCatch(
        {
            model <- model_boltzmann(norm_data)
            return(model)
        },
        warning = function(w) {
            condition <- conditionMessage(w)
            warning(
                "Warning: Error caught while imposing boltzmann fit on well ",
                unique(norm_data$Well.Position), "Derivative Model is applied
            instead. To override, specify a min and max of modeling manually. ",
                condition, "\n"
            )
            model <- model_gam(norm_data,
                x = norm_data$Temperature,
                y = norm_data$Normalized
            )
            return(model)
        },
        error = function(e) {
            condition <- conditionMessage(e)
            warning(
                "Warning: Error caught while imposing boltzmann fit on well ",
                unique(norm_data$Well.Position), "Derivative Model is applied
            instead. To override, specify a min and max of modeling manually. ",
                condition, "\n"
            )
            model <- model_gam(norm_data,
                x = norm_data$Temperature,
                y = norm_data$Normalized
            )
            return(model)
        }
    )
}

#' Refit and calculate derivative function
#'
#' Model_fit calculates derivatives by refitting model onto data. Only runs
#'     on data of a single well.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
#'
#' @param norm_data data frame; the raw data set input
#' @param model fitted model containing fitted values
#' @param smoothed inform whether data already contains a smoothed model; Input
#'     the column name of the smoothed data to override values of gam model
#'     fitting. For example, existing "Fluorescence" column contains data
#'     already smoothed, set \code{smoothed = "Flourescence"} to calculate
#'     derivative function upon the called smoothed data.
#' @return data frame; with calculated derivative columns
#'
#' @family data_preprocess
#'
#' @examples
#' data("qPCR_data1")
#' test <- subset(qPCR_data1, Well.Position == "A01")
#' test <- normalize(test, fluo = 5, selected = c(
#'     "Well.Position", "Temperature",
#'     "Fluorescence", "Normalized"
#' ))
#' gammodel <- model_gam(test, x = test$Temperature, y = test$Normalized)
#' model_fit(test, model = gammodel)
#' # if data come smoothed, run ...
#' model_fit(test, smoothed = "Fluorescence")
#'
#' @export
#'
model_fit <- function(norm_data, model, smoothed) {
    if (missing(smoothed)) {
        norm_data <- norm_data %>%
            # pull in the fitted values into data frame
            mutate(fitted = model$fitted.values)
        # calculate derivative using fitted values
        norm_data <- mutate(norm_data,
            norm_deriv =
                c(diff(norm_data$fitted) /
                    diff(norm_data$Temperature), NA)
        )
    } else {
        norm_data <- mutate(norm_data,
            norm_deriv =
                c(diff(norm_data[, smoothed]) /
                    diff(norm_data$Temperature), NA)
        )
    }
}
