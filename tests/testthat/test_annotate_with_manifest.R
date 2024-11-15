test_that("annotate_with_manifest works by annotating files with `data.table` manifest", {

  modify_annotation

  testthat::expect_identical(update_items(current, update),
                             expected)

})

