test_that("get_param returns matching value from params", {
    f <- get_param
    environment(f) <- list2env(
        list(params = list(alpha = 123)),
        parent = environment(get_param)
    )

    expect_equal(f("alpha"), 123)
})

test_that("get_param uses default_fun when param is missing", {
    expect_equal(get_param("missing", function() 10), 10)
    expect_equal(get_param("missing", function(x, y) x + y, 2, 3), 5)
})

test_that("get_param treats NULL value as missing", {
    f <- get_param
    environment(f) <- list2env(
        list(params = list(alpha = NULL)),
        parent = environment(get_param)
    )

    expect_equal(f("alpha", function() "fallback"), "fallback")
})

test_that("get_param validates inputs", {
    expect_error(get_param(1), "is.character")
    expect_error(get_param(c("a", "b")), "length")
    expect_error(get_param("a", default_fun = 1), "is.function")
})
