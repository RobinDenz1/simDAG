
nested_list2 <- list(A=list(c(1, 2, 10), 2, NULL, 4, 5),
                    B=list(5, c(1, 10), NULL, 4, NULL))
true_merged2 <- list(c(1, 2, 10, 5),
                    c(2, 1, 10),
                    NULL,
                    c(4, 4),
                    5)

nested_list3 <- list(A=list(c(1, 2, 10), 2, NULL, 4, 5),
                     B=list(5, c(1, 10), NULL, 4, NULL),
                     c=list(c(3, 4), 1, NULL, 17, NULL))
true_merged3 <- list(c(1, 2, 10, 5, 3, 4),
                     c(2, 1, 10, 1),
                     NULL,
                     c(4, 4, 17),
                     5)

test_that("example case 2 lists", {
  merged <- merge_nested_lists(nested_list=nested_list2)

  expect_equal(merged, true_merged2)
})

test_that("example case 3 lists", {
  merged <- merge_nested_lists(nested_list=nested_list3)

  expect_equal(merged, true_merged3)
})

test_that("works with empty list", {
  merged <- merge_nested_lists(list(list(NULL, NULL), list(NULL, NULL)))

  expect_equal(merged, list(NULL, NULL))
})
