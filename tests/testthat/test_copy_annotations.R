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

  entity_c <- synapseclient$Folder("Entity C",
                                   parent = PARENT_TEST_PROJECT)
  entity_c <- .syn$store(entity_c)

  # when copying all annotations from A->B (default)
  copy_annotations(entity_from = entity_a$properties$id,
                   entity_to = entity_b$properties$id,
                   select = NULL,
                   update = TRUE)

  # when copying selective annotations from A->C
  copy_annotations(entity_from = entity_a$properties$id,
                   entity_to = entity_c$properties$id,
                   select = c("favorites", "key_not_on_a"),
                   update = TRUE)

  result_b <- .syn$get_annotations(entity_b)
  result_c <- .syn$get_annotations(entity_c)
  .syn$delete(entity_a)
  .syn$delete(entity_b)
  .syn$delete(entity_c)
  testthat::expect_equal(result_b$foo, "bar")
  testthat::expect_equal(result_b$favorites, c("raindrops", "whiskers"))
  testthat::expect_equal(result_b$after_a, TRUE)
  testthat::expect_error(result_c$foo) # Expect KeyError since key should not be present
  testthat::expect_equal(result_c$favorites, c("raindrops", "whiskers"))
  testthat::expect_error(result_c$key_not_on_a)

})


