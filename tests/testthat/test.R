data("iris")
set.seed(100)
train <- sample(nrow(iris), nrow(iris) * .7)
train.species <- iris$Species[train]
test.species <- iris$Species[-train]
p <- psi(train.species, test.species)

expect_equal(round(as.numeric(p), 4), 0.0707)
expect_equal(attr(p, 'Empty Levels'), NULL)

x <- c(1:6, NA)
y <- c(2:7)
expect_warning(p <- psi(x, y, 3))
expect_equal(attr(p, 'Empty Levels'), 'Missing')
expect_equal(round(as.numeric(p), 4), 0.1291)
