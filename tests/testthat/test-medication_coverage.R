
test_case <- tibble::tibble(
  ID = c("A", "A", "A", "B", "B", "B", "C", "C", "C", "C"),
  MEDICATION = rep("Med1", 10),
  DATE_FILL = as.Date(c("2024-01-01", "2024-01-15", "2024-02-01",
                        "2024-01-01", "2024-01-10", "2024-01-25",
                        "2024-01-01", "2024-01-10", "2024-01-15", "2024-02-20")),
  DAYS_SUPPLY = c(10, 10, 10,
                  10, 10, 10,
                  10, 10, 10, 10),
  expected_start = as.Date(c("2024-01-01", "2024-01-15", "2024-02-01",
                             "2024-01-01", "2024-01-11", "2024-01-25",
                             "2024-01-01", "2024-01-11", "2024-01-21", "2024-02-20")),
  expected_stop = as.Date(c("2024-01-10", "2024-01-24", "2024-02-10",
                            "2024-01-10", "2024-01-20", "2024-02-03",
                            "2024-01-10", "2024-01-20", "2024-01-30", "2024-02-29")),
  expected_carry_days = c(0, 0, 0,
                          0, 1, 0,
                          0, 1, 6, 0)
)

dt <- dplyr::select(test_case, ID:DAYS_SUPPLY)

answer <- dt %>%
  dt_add_coverage_dates(
    id_col = 'ID',
    med_col = "MEDICATION",
    fill_date_col = "DATE_FILL",
    supply_col = "DAYS_SUPPLY",
    max_carry = Inf
  )

smry <- answer[, .(start = start[1],
                   stop = stop[.N],
                   days_covered = sum(stop - start + 1)),
               by = .(ID, MEDICATION)] %>%
  .[, days_active := stop - start]



expect_equal(answer$start, test_case$expected_start)
expect_equal(answer$stop, test_case$expected_stop)

set.seed(329)

x <- as.integer(round(runif(n=1000, min = -4, max=4)))

count_leftovers_r <- function(x, min_value = 0){

  out <- vector('numeric', length(x))
  out[1L] <- max(x[1], min_value)

  if(length(x) == 1) return(out)

  for(i in seq(2, length(x))){

    out[i] <- max(out[i-1] + x[i], min_value)

  }

  out

}

expect_equal(count_leftovers_r(x), count_leftovers(x))

# microbenchmark::microbenchmark(
#   r = count_leftovers_r(x),
#   cpp = count_leftovers(x)
# )

