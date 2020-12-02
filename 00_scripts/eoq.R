D <- 1000
K <- 5
h <- 0.25
Q = sqrt(2*D*K/h)

roll2 <- function(faces = 1:6, number_of_dice = 2) {
  dice = sample(faces, size = number_of_dice, replace = TRUE)
  sum(dice)
}

roll3 <- function(faces = 1:6, number_of_dice = 1) {
  dice = sample(faces, size = number_of_dice, replace = TRUE, prob = c(.1, .1, .1, .1, .1, .5))
  sum(dice)
}

hist(replicate(n=100, expr=roll3(), simplify=TRUE))