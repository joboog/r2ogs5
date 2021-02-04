d1_path <- "../../data/benchmarks_ref/mcwt_domain_quad_0.tec"
d2_path <- "../../data/benchmarks_ref/trans_bd_homo_domain_GROUNDWATER_FLOW_quad.tec"

context("read_tecplot")
test_that("geotype domain read correctly from tec files",
          {
              testthat::expect_silent({
                  d1 <- ogs5_read_tecplot_domain(filepath = d1_path)

                  d2 <- ogs5_read_tecplot_domain(filepath = d2_path)
              })
              testthat::expect_equal(dim(d1), c(420, 13))
              testthat::expect_equal(dim(d2), c(4935, 5))
              testthat::expect_true(all(apply(d1, 2, class) == "numeric"))
          })

