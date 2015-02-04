context("First time to use tacs on testthat.")

test_that("tacs example: to be failure",{
  
  
  expect_equal(1,1)
})

test_that("Make_data_frame_to_store_ic",{
  context("Proper size of data frame")
  data(dream4)
  gene_name <- colnames(dream4$exp)
  sample_name <- rownames(dream4$exp)
  res <- make_data_frame_to_store_ic(gene_name,sample_name,TRUE,dream4$segment)
  b <- expect_equal(nrow(res),100)
  if(b$passed)  print("DF with segment size OK.") else print("DF size BAD")

  res <- make_data_frame_to_store_ic(gene_name,sample_name,FALSE,length(sample_name))
  b <- expect_equal(nrow(res),105)
  if(b$passed)  print("DF w/o segmentsize OK.") else print("DF size BAD")
  
  })

test_that("set markov exp",{
  context("generate exp data without segment_end or segment_start")
  data(dream4)
  upper <- set_upper_exp(dream4$exp,dream4$segment)
  for(gene in 1:10) {
    for(i in 1:20)
      expect_equal(dream4$exp[i,gene],upper[i,gene])
    for(i in 21:40)
      expect_equal(dream4$exp[i+1,gene],upper[i,gene])
    for(i in 41:60)
      b <- expect_equal(dream4$exp[i+2,gene],upper[i,gene])
    for(i in 61:80)
      b <- expect_equal(dream4$exp[i+3,gene],upper[i,gene])
    for(i in 81:100)
      b <- expect_equal(dream4$exp[i+4,gene],upper[i,gene])
  }
  lower <- set_lower_exp(dream4$exp,dream4$segment)
  for(gene in 1:10) {
    for(i in 1:20)
      expect_equal(dream4$exp[i+1,gene],lower[i,gene])
    for(i in 21:40)
      b <- expect_equal(dream4$exp[i+2,gene],lower[i,gene])
    for(i in 41:60)
      b <- expect_equal(dream4$exp[i+3,gene],lower[i,gene])
    for(i in 61:80)
      b <- expect_equal(dream4$exp[i+4,gene],lower[i,gene])
    for(i in 81:100)
      b <- expect_equal(dream4$exp[i+5,gene],lower[i,gene])
  }})

test_that("IC without edge",{
  context("Calculate information content for each edge with each time")
  data(dream4)
  gene_name <- colnames(dream4$exp)
  sample_name <- rownames(dream4$exp)
  ic <- calc_ic_without_edge(dream4$exp,"aic-g",FALSE,105)
  b <- expect_equal(floor(ic[1,1]),6)
  b <- expect_equal(floor(ic[105,10]),48)
  
  ic <- calc_ic_without_edge(dream4$exp,"bic-g",FALSE,105)
  b <- expect_equal(floor(ic[1,1]),5)
  b <- expect_equal(floor(ic[105,10]),47)
  
  ic <- calc_ic_without_edge(dream4$exp,"aic-g",TRUE,dream4$segment)
  b <- expect_equal(floor(ic[1,1]),5)
  b <- expect_equal(floor(ic[100,10]),44)
  
  ic <- calc_ic_without_edge(dream4$exp,"bic-g",TRUE,dream4$segment)
  b <- expect_equal(floor(ic[1,1]),3)
  b <- expect_equal(floor(ic[100,10]),42)
  })

test_that("IC withedge",{
  data(dream4)
  #IC of node without in edge is NA
  icwo <- calc_ic_without_edge(dream4$exp,"aic-g",FALSE,105) 
  icw  <- calc_ic_with_edges(dream4$net,dream4$exp,"aic-g",FALSE,105,icwo) 
  
  
})