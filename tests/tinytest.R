
if ( requireNamespace("tinytest", quietly=TRUE) ){
  options(lifecycle_verbosity = "quiet")
  tinytest::test_package("cartography")
}

