test_that("`equals.dbproj_char_tuple` works", {
  expect_equal(equals.dbproj_char_tuple(char_tuple(c('a')), char_tuple('a')), TRUE)
  expect_equal(equals.dbproj_char_tuple(char_tuple(c('b')), char_tuple('b')), TRUE)
  expect_equal(equals.dbproj_char_tuple(char_tuple(c('a')), char_tuple('c')), FALSE)
  expect_equal(equals.dbproj_char_tuple(char_tuple(c('b')), char_tuple('c')), FALSE)
})

test_that("`contents.dbproj_char_tuple` works", {
  expect_equal(contents.dbproj_char_tuple(char_tuple(c('a', 'b', 'c'))), c('a', 'b', 'c'))
})

test_that("`insert.dbproj_char_tuple` works", {
  expect_equal(
    equals.dbproj_char_tuple(
      char_tuple(x = c('a', 'b')), 
      insert.dbproj_char_tuple(
        obj = char_tuple('a'), 
        x = 'b', 
        in_place = FALSE
      )
    ), 
    TRUE
  )
}) 

test_that("`update.dbproj_char_tuple` works", {
  expect_equal(
    equals.dbproj_char_tuple(
      char_tuple(x = c('a', 'b')), 
      update.dbproj_char_tuple(
        obj = char_tuple(x = c('c', 'd')), 
        x = c('a', 'b'), 
        in_place = FALSE
      )
    ), 
    TRUE
  )
})

test_that("`delete.dbproj_char_tuple` works", {
  expect_equal(
    equals.dbproj_char_tuple(
      char_tuple(x = c('a', 'c')), 
      delete.dbproj_char_tuple(
        obj = char_tuple(x = c('a', 'b', 'c', 'd')), 
        x = c('b', 'd'), 
        in_place = FALSE
      )
    ), 
    TRUE
  )
})
