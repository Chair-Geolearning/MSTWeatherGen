all_parameters <- c(".a",".b",".c",".d",".e",".Ai",".Aj",".aii",".ajj",".nuii",".nujj",".rho1ij",".r2ii",".r2jj",".r1ii",".r1jj",".rho2ij")
test_that("test parameters indices and values", {
  
  sapply(all_parameters, function(param){
    expect_true(.lower[get(param)] <= .init[get(param)])
    expect_true(.init[get(param)] <= .upper[get(param)])
  })
})
