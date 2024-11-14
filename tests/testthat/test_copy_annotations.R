test_that("Copy annotations works", {
  skip_if_no_synapseclient()
  skip_if_no_login()

  PARENT_TEST_PROJECT <- "syn26462036"
  # Create some folder objects with some annotations
  entity_a <- synapseclient$Folder("Entity A",
                                   parent = PARENT_TEST_PROJECT,
                                   annotations = list(foo = "bar", favorites = c("raindrops", "whiskers")))
  entity_a <- .syn$store(entity_a)

  entity_b <- synapseclient$Folder("Entity B",
                                   parent = PARENT_TEST_PROJECT,
                                   annotations = list(favorites = c("kettles", "mittens"), after_a = TRUE))
  entity_b <- .syn$store(entity_b)

  copy_annotations(entity_from = entity_a$properties$id,
                   entity_to = entity_b$properties$id,
                   select = NULL,
                   update = TRUE)
  result <- .syn$get_annotations(entity_b)
  .syn$delete(entity_a)
  .syn$delete(entity_b)
  testthat::expect_equal(result$foo, "bar")
  testthat::expect_equal(result$favorites, c("raindrops", "whiskers"))
  testthat::expect_equal(result$after_a, TRUE)

})
