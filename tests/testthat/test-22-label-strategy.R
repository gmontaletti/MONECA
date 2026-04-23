# Tests for the label_strategy parameter in segment.membership.dataframe().
#
# Verifies that the default "strength" strategy preserves backward compatibility,
# the new "mass" strategy picks the largest-volume member per segment, and
# invalid strategy values error via match.arg.

test_that("default label_strategy = 'strength' preserves backward compatibility", {
  set.seed(1)
  mx <- generate_mobility_data(n_classes = 10, seed = 1)
  seg <- moneca_fast(mx, segment.levels = 3, progress = FALSE)

  df_default <- segment.membership.dataframe(seg)
  df_explicit <- segment.membership.dataframe(seg, label_strategy = "strength")

  expect_identical(df_default, df_explicit)
})

test_that("mass strategy picks the member with largest row+col margin", {
  set.seed(2)
  mx <- generate_mobility_data(n_classes = 10, seed = 2)
  seg <- moneca_fast(mx, segment.levels = 3, progress = FALSE)

  df_mass <- segment.membership.dataframe(seg, label_strategy = "mass")

  # Numeric segment codes must be identical to the default — only the label
  # representative (inside name_level_X) changes across strategies.
  df_strength <- segment.membership.dataframe(seg, label_strategy = "strength")
  lvl_cols <- grep("^level_", names(df_strength), value = TRUE)
  expect_identical(df_mass[, lvl_cols], df_strength[, lvl_cols])

  # Hand-verify mass: pick a multi-member segment at level 2 and confirm the
  # label's representative is the member with the largest (row + col) margin.
  level_2_codes <- df_mass$level_2
  multi_entities <- names(table(level_2_codes))[table(level_2_codes) > 1]
  skip_if(
    length(multi_entities) == 0,
    "no multi-member segment at level 2 in this seed"
  )

  L <- nrow(mx)
  margin_total <- as.numeric(mx[-L, L]) + as.numeric(mx[L, -L])

  for (code in multi_entities) {
    members <- which(df_mass$level_2 == code)
    expected_rep_idx <- members[which.max(margin_total[members])]
    expected_name <- df_mass$name[expected_rep_idx]
    labels <- df_mass$name_level_2[members]
    expect_true(all(startsWith(labels, paste0(expected_name, " "))))
  }
})

test_that("invalid label_strategy is rejected by match.arg", {
  set.seed(3)
  mx <- generate_mobility_data(n_classes = 6, seed = 3)
  seg <- moneca_fast(mx, segment.levels = 2, progress = FALSE)

  # match.arg error message is locale-dependent; match on the arg names instead
  expect_error(
    segment.membership.dataframe(seg, label_strategy = "medoid"),
    regexp = "strength"
  )
})
