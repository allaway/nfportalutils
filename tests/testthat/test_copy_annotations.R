test_that("Copy annotations works to apply an immediate copy of annotations from one entity to another (update=TRUE)
          as well as when simply getting a copy of the annotations present as a default R list to work with (update=FALSE)", {
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

  # when getting a copy of all annotations from A->B (default)
  copy_of_a_b <- copy_annotations(entity_from = entity_a$properties$id,
                                  entity_to = entity_b$properties$id,
                                  select = NULL,
                                  update = FALSE)

  # when immediately copying all annotations from A->B (default)
  copy_annotations(entity_from = entity_a$properties$id,
                   entity_to = entity_b$properties$id,
                   select = NULL,
                   update = TRUE)

  # when getting a copy of selective annotations from A->C
  copy_of_a_c <- copy_annotations(entity_from = entity_a$properties$id,
                                  entity_to = entity_c$properties$id,
                                  select = c("favorites", "key_not_on_a"),
                                  update = FALSE)

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
  testthat::expect_equal(copy_of_a_b, list(after_a = TRUE, favorites = c("raindrops", "whiskers"), foo = "bar"))
  testthat::expect_equal(copy_of_a_c, list(favorites = c("raindrops", "whiskers")))
  testthat::expect_equal(result_b$foo, "bar")
  testthat::expect_equal(result_b$favorites, c("raindrops", "whiskers"))
  testthat::expect_equal(result_b$after_a, TRUE)
  testthat::expect_error(result_c$foo) # Expect KeyError since key should not be present
  testthat::expect_equal(result_c$favorites, c("raindrops", "whiskers"))
  testthat::expect_error(result_c$key_not_on_a)

})

