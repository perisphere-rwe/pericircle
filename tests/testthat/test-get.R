
test_that(
  desc = "get unknowns has expected output as request",
  code = {
    expect_snapshot(get_unknowns(iris, as_request = TRUE))
  }
)

test_that(
  desc = "get unknowns has expected output as code",
  code = {
    expect_snapshot(get_unknowns(iris, as_code = TRUE))
  }
)

