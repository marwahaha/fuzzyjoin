context("geo_join")

set.seed(2016)
latlong1 <- data_frame(index1 = 1:500,
                       latitude = rnorm(500, 40),
                       longitude = rnorm(500, 40))

latlong2 <- data_frame(index2 = 1:500,
                       latitude = rnorm(500, 40),
                       longitude = rnorm(500, 40))

ll1 <- as.matrix(latlong1[c("longitude", "latitude")])
ll2 <- as.matrix(latlong2[c("longitude", "latitude")])

test_that("geo_inner_join works", {
  j <- latlong1 %>%
    geo_inner_join(latlong2, max_dist = 1, distance_col = "distance")

  expect_true(nrow(j) > 0)

  d <- geosphere::distHaversine(ll1[j$index1, ], ll2[j$index2, ]) / 1609.344

  expect_true(all(d <= 1))
  expect_true(any(d >= .5))
  expect_equal(d, j$distance)

  # test it works even when there are no matches
  j2 <- latlong1 %>%
    geo_inner_join(latlong2, max_dist = .00001, distance_col = "distance")

  expect_equal(nrow(j2), 0)
  expect_true(all(c("latitude.x", "latitude.y",
                    "longitude.x", "longitude.y") %in% colnames(j2)))

  # try other methods
  # skip vincentyellipsoid since it's very slow
  for (m in c("geo", "cosine", "meeus", "vincentysphere")) {
    j3 <- latlong1 %>%
      geo_inner_join(latlong2, method = m,
                     max_dist = 1,
                     distance_col = "distance")

    # it should be pretty close to Haversine method
    expect_true(all(j3$distance <= 1))
    expect_lt(abs(nrow(j3) - nrow(j)), 2)
  }
})

test_that("geo_inner_join works when lat/lon columns have different names", {
  j <- latlong1 %>%
    geo_inner_join(latlong2, max_dist = 1, distance_col = "distance")

  j2 <- latlong1 %>%
    select(index1, longitude, latitude) %>%
    geo_inner_join(latlong2, max_dist = 1, distance_col = "distance")

  expect_equal(j, j2[colnames(j)])

  l1 <- latlong1 %>%
    select(index1, Lat = latitude, Lon = longitude)

  l2 <- latlong2 %>%
    select(index2, Lon = longitude, Lat = latitude)

  j3 <- geo_inner_join(l1, l2, max_dist = 1, distance_col = "distance")

  expect_equal(j2$index1, j3$index1)
  expect_equal(j2$index2, j3$index2)
})
