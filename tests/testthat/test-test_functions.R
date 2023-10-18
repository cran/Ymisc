
# test the 'compute_AI()' function

test_that("test the compute_AI() function", {

  # built-in data
  data(sample_data)

  # using the compute_AI() function
  results <-compute_AI(data = sample_data,
                        left_hemisphere = "lh",
                        right_hemisphere = "rh",
                        separator="_",
                        ID="ID",
                        hemisphere="prefix",
                        start="lh_Thalamus",
                        end="rh_AccumbensArea")

  # Check if AIs were calculated correctly.

  results$AI2Putamen <- (results$lh_Putamen - results$rh_Putamen)/(results$lh_Putamen + results$rh_Putamen)
  results$AI2Amygdala <- (results$lh_Amygdala - results$rh_Amygdala)/(results$lh_Amygdala + results$rh_Amygdala)


  expect_equal(results$AI_Putamen, results$AI2Putamen)
  expect_equal(results$AI_Amygdala, results$AI2Amygdala)

})

# test the 'compute_total()' function

test_that("test the compute_total() function", {

  # built-in data
  data(sample_data)

  # using the compute_total() function
  results2 <-compute_total(data = sample_data,
                       left_hemisphere = "lh",
                       right_hemisphere = "rh",
                       separator="_",
                       ID="ID",
                       hemisphere="prefix",
                       start="lh_Thalamus",
                       end="rh_AccumbensArea")

  # Check if the bilateral (L+R) measures were calculated correctly.

  results2$total2Thalamus <- results2$lh_Thalamus + results2$rh_Thalamus
  results2$total2Caudate <- results2$lh_Caudate + results2$rh_Caudate



  expect_equal(results2$total_Thalamus, results2$total2Thalamus)
  expect_equal(results2$total_Caudate, results2$total2Caudate)

})

# test long2wide() and wide2long() functions


test_that("test long2wide() and wide2long() functions", {

  # built-in data
  data(sample_data)

   long<-wide2long(data=sample_data,
                  ID="ID",
                  separator="_",
                  hemisphere="prefix",
                  start="lh_Thalamus",
                  end="rh_AccumbensArea")

   wide<-long2wide(data=long,
                   ID="ID",
                   separator="_",
                   start="region",
                   end="rh")
   # The order of the columns may be changed
   # reorder columns

   names<-colnames(sample_data)
  reorder_wide <- wide[,names]

   expect_equal(sample_data, reorder_wide)
})

