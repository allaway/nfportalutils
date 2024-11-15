test_that("Annotate with manifest works", {
  skip_if_no_synapseclient()
  skip_if_no_login()

  PARENT_TEST_PROJECT <- "syn26462036"
  # Use some folders to represent objects to annotate
  objs <- make_folder(parent = PARENT_TEST_PROJECT, folders = c("mock_file_1", "mock_file_2", "mock_file_3"))
  ids <- sapply(objs, function(x) x$properties$id)
  # Partial manifest as a data.table with list columns
  manifest <- data.table(
    entityId = ids,
    assay = "drugScreen",
    experimentalTimepoint = c(1L, 3L, 7L),
    experimentalTimepointUnit = "days",
    cellType = list(c("schwann", "macrophage"), c("schwann", "macrophage"), c("schwann", "macrophage"))
  )
  annotate_with_manifest(manifest)
  remanifested <- list()
  for(i in ids) {
    remanifested[[i]] <- .syn$get_annotations(i)
  }
  for(i in ids) .syn$delete(i)
})
