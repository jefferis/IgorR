# Tests for reading of Igor Binary Wave (.ibw) files
# 
# Author: jefferis
###############################################################################

context("Verify reading of Igor ibw files")

test_that("Read Igor v5 file", {
      expect_that (w5<-read.ibw('../igor/version5.ibw'),
          is_a("numeric"),'read igor v5 wave without error')
      expect_that(attr(w5,'BinHeader')$version,
          equals(5))
      expect_that(tsp.igorwave(w5),
          equals(structure(c(0, 4, 1), .Names = c("start", "end", "frequency"))))
      
    })

test_that("Read Igor v2 file", {
      expect_that (w2<-read.ibw('../igor/version2.ibw'),
          is_a("numeric"),'read igor v2 wave without error')
      expect_that(attr(w2,'BinHeader')$version,
          equals(2))
      expect_that(tsp.igorwave(w2),
          equals(structure(c(0, 4, 1), .Names = c("start", "end", "frequency"))))
    })
