make_dummy_dat_lencomp <- function(fleets, years, len_bins, nsex = 1) {
  dummy_dat_list <- lapply(fleets, function(fleet) {
    data.frame(
      "year" = years, "month" = 1, "Flt" = fleet,
      "sex" = ifelse(nsex == 1, 0, 3), "part" = 0, "Nsamp" = 10,
      stringsAsFactors = FALSE
    )
  })
  dummy_dat <- as.data.frame(do.call("rbind", dummy_dat_list))
  dummy_df <- data.frame(matrix(1,
    nrow = nrow(dummy_dat),
    ncol = nsex * length(len_bins)
  ))
  names(dummy_df) <- paste0("l", len_bins)
  cbind(dummy_dat, dummy_df)
}

make_dummy_dat_agecomp <- function(fleets, years, age_bins, nsex = 1) {
  dummy_dat_list <- lapply(fleets, function(fleet) {
    data.frame(
      "year" = years, "month" = 1, "Flt" = fleet,
      "sex" = ifelse(nsex == 1, 0, 3),
      "part" = 0, "AgeErr" = 1, "Lbin_lo" = -1, "Lbin_hi" = -1,
      "Nsamp" = length(age_bins), stringsAsFactors = FALSE
    )
  })
  dummy_dat <- as.data.frame(do.call("rbind", dummy_dat_list))
  dummy_df <- data.frame(matrix(1,
    nrow = nrow(dummy_dat),
    ncol = nsex * length(age_bins)
  ))
  names(dummy_df) <- paste0("a", age_bins)
  cbind(dummy_dat, dummy_df)
}

make_dummy_dat_calcomp <- function(fleets, years, age_bins,
                                   len_bins, nsex = 1) {
  Lbin_lo <- len_bins
  # to select only 1 bin, Lbin_lo and Lbin_hi should be the same (according to
  # Rick Methot and Ian Taylor)
  Lbin_hi <- len_bins
  dummy_dat_list <- lapply(fleets, function(fleet) {
    lapply(years, function(yr) {
      data.frame(
        "year" = yr, "month" = 1, "Flt" = fleet,
        # TODO: for a 2 sex model, may not want to use 3, but rather
        # separate lines for each Sex. This loosens restrictions on
        # the sex ratio (in the context of an EM)
        "sex" = ifelse(nsex == 1, 0, 3),
        "part" = 0, "AgeErr" = 1, "Lbin_lo" = Lbin_lo, "Lbin_hi" = Lbin_hi,
        "Nsamp" = length(age_bins), stringsAsFactors = FALSE
      )
    })
  })
  dummy_dat_list <- unlist(dummy_dat_list, recursive = FALSE)
  dummy_dat <- as.data.frame(do.call("rbind", dummy_dat_list))
  # add the age columns to dummy_dat.
  dummy_df <- data.frame(matrix(1,
    nrow = nrow(dummy_dat),
    ncol = nsex * length(age_bins)
  ))
  names(dummy_df) <- paste0("a", age_bins)
  cbind(dummy_dat, dummy_df)
}

make_dummy_dat_mlacomp <- function(fleets, years, age_bins) {
  dummy_dat_list <- lapply(fleets, function(fleet) {
    data.frame(
      "year" = years, "month" = 1, "Flt" = fleet, "sex" = 0,
      "part" = 0, "AgeErr" = 1, "Nsamp" = 10, stringsAsFactors = FALSE
    )
  })
  dummy_dat <- as.data.frame(do.call("rbind", dummy_dat_list))
  dummy_df <- data.frame(matrix(1, nrow = nrow(dummy_dat), ncol = length(age_bins) * 2))
  names(dummy_df) <- c(paste0("a", c(age_bins)), paste0("N", c(age_bins)))
  cbind(dummy_dat, dummy_df)
}
