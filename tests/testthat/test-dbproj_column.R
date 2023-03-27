test_that("`equals.dbproj_column` works", {
  expect_equal(
    equals.dbproj_column(
      column(name = 'name1', type = 'character'), 
      column(name = 'name1', type = 'character')
    ), 
    TRUE
  )
  expect_equal(
    equals.dbproj_column(
      column(name = 'name1', type = 'numeric'), 
      column(name = 'name2', type = 'numeric')
    ), 
    FALSE
  )
  expect_equal(
    equals.dbproj_column(
      column(name = 'name1', type = 'character'), 
      column(name = 'name1', type = 'numeric')
    ), 
    FALSE
  )
})

test_that("`contents.dbproj_char_tuple` works", {
  expect_equal(
    contents.dbproj_column(column(name = 'name1', type = 'character')), 
    list(name = 'name1', type = 'character')
  )
  expect_equal(
    contents.dbproj_column(column(name = 'name2', type = 'numeric')), 
    list(name = 'name2', type = 'numeric')
  )
})
