testthat::context(desc = "test segment")

testthat::describe(
  description = "segment result test",
  code = {
    it ("equall", {
        segmentr <- rTinySegmenter::tiny_segmenter$new(
          text = "私の名前は中野です"
        )

        testthat::expect_equal(
          object = segmentr$tokenize(),
          expected = "私 | の | 名前 | は | 中野 | です"
        )
    })
  }
)
