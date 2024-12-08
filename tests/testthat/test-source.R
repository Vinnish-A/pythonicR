
# test-source -------------------------------------------------------------


## buildContent ----

test_that(
  'buildContent back to root', {
    expect_equal(
      buildContent('part1$', fileWhenTest('test.R')),
      buildContent('part1.1$', fileWhenTest('test.R'), T)
    )
  }
)

test_that(
  'buildAllContent lists all content under a certain level', {
    expect_equal(
      buildAllContent('part1$', fileWhenTest('test.R')),
      "vec_all = c('a', 'b', 'c', 'd')\nvec1 = vec_all[[1]]\ncat(vec1, file = 'tmp/vec1.txt')\nvec2 = vec_all[[2]]\ncat(vec2, file = 'tmp/vec2.txt')\nc = 'a'\nvec3 = vec_all[[3]]\ncat(vec3, file = 'tmp/vec3.txt')"
    )
  }
)

test_that(
  'buildContent finds wrong with wrong-format file', {
    expect_error(
      buildContent('part1', fileWhenTest('overlevel.R'))
    )
  }
)

test_that(
  'buildAllContent finds wrong with wrong-format file', {
    expect_error(
      buildAllContent('part1', fileWhenTest('overlevel.R'))
    )
  }
)

## findWhere ----

test_that(
  'findWhere finds wrong with file cotaining same subtitles', {
    expect_error(
      findWhere(buildFile(fileWhenTest('subSame.R'), checkType_ = 'strict'), 'partA')
    )
  }
)

test_that(
  'findWhere finds wrong with file cotaining subtitle which doesn\'t exist', {
    expect_error(
      findWhere(buildFile(fileWhenTest('subSame.R'), checkType_ = 'strict'), 'partZ')
    )
  }
)

test_that(
  'Regular expressions are applicable in findWhere.', {
    expect_equal(
      findWhere(buildFile(fileWhenTest('subSame.R'), checkType_ = 'strict'), 'partC$'),
      'partC'
    )
  }
)

test_that(
  'findWhere can find the subtitle with unique path.', {
    expect_equal(
      findWhere(buildFile(fileWhenTest('subSame.R'), checkType_ = 'strict'), 'partH'),
      c('partC', 'partA', 'partH')
    )
  }
)
