test_that("import local txt file", {
  sua_txt <- system.file("extdata", "suaorder.txt", package = "rairspace")
  sua_pdf <- system.file("extdata", "suaorder.pdf", package = "rairspace")

  # successfully reads text file as chr vector
  expect_output(str(sua_import(testthat::test_path("ref",
                                                   "suaorder.txt"))),
                "chr ")

  # successfully imports pdf file as chr vector
  expect_output(str(sua_import(testthat::test_path("ref",
                                                   "suaorder.pdf"))),
                "chr ")

  # successfully downloads online file as chr vector
  expect_output(str(sua_import()),
                "chr ")

  # ignores anything without txt/pdf extension
  expect_error(str(sua_import("file_ext.bad")),
               "path does not point")

  # rejects filename that doesn't exist
  expect_error(str(sua_import("no_file.txt")),
               ".does not exist")

  # rejects anything empty or without txt/pdf extension
  expect_error(expect_warning(str(sua_import("http://bad_url_duh.pdf")),
                              "could not be resolved"),
               "cannot open URL")
})
