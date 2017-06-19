context("genome_join")

library(dplyr)

x1 <- data_frame(id = 1:4,
                 chromosome = c("chr1", "chr1", "chr2", "chr2"),
                 start = c(100, 200, 300, 400),
                 end = c(150, 250, 350, 450))

x2 <- data_frame(id = 1:4,
                 chromosome = c("chr1", "chr2", "chr2", "chr1"),
                 start = c(140, 210, 400, 300),
                 end = c(160, 240, 415, 320))

test_that("Can join genomes on chromosomes and intervals", {
  j <- genome_inner_join(x1, x2, by = c("chromosome", "start", "end"))

  expect_equal(j$chromosome.x, j$chromosome.y)
  expect_equal(j$chromosome.x, c("chr1", "chr2"))
  expect_equal(j$id.x, c(1, 4))
  expect_equal(j$id.y, c(1, 3))
  expect_equal(colnames(j), c("id.x", "chromosome.x", "start.x", "end.x",
                              "id.y", "chromosome.y", "start.y", "end.y"))

  # if they were all the same chromosome, everything would get joined
  x3 <- x1
  x3$chromosome <- "chr1"
  x4 <- x2
  x4$chromosome <- "chr1"

  j2 <- genome_inner_join(x3, x4, by = c("chromosome", "start", "end"))

  expect_equal(nrow(j2), 4)
  expect_equal(j2$id.x, 1:4)
  expect_equal(j2$id.y, c(1, 2, 4, 3))
})
