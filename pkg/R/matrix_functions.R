#' Generate a random invertible matrix of integers
#'
#' This function generates random square integer matrices. 
#' It tests to be sure the random matrix is invertible (does not have a determinant of zero); 
#' if the determinant is zero, it keeps trying until it produces a solvable matrix.
#' @param n Number of rows and columns in the square matrix.
#' @param seed A character string from which a seed for the random number generator will be generated. 
#' You can use your user id or email address to get a reproducible matrix.
#' @keywords random invertible matrix
#' @export
#' @examples
#' random_invertable_matrix(3, "bob")

random_invertible_matrix <- function(n, seed=NA){
	if (!is.na(seed)){
		if(is.character(seed)){
			require(digest)
			seed <- strtoi(paste0("0x",substr(digest(seed),1,4)))
		}
		set.seed(seed)
	}
	# Initialize with a singular matrix
	A <- matrix( 1:n^2, nrow=n)
	# Choose random integer matrixes until we find a non-singular one
	while (det(A) == 0)
		A <- matrix( sample(1:99, n^2, replace=T), nrow=n)
	A
}

#' Generate a rotation matrix to transform a 2-column input by rotating it through a given angle.
#'
#' @param angle in radians
#' @export
#` @examples
#` x <- y <- 1:10
#` plot(x, y, type="l", xlim=c(-10,10), ylim=c(-10,10))
#` 
#` rotate_me <- matrix(c(x, y), ncol=2)
#` rotated <- rotate_me %*% rotation_matrix(pi/2)
#` lines(rotated[,1], rotated[,2], col="red")
#` rotated <- rotate_me %*% rotation_matrix(pi/4)
#` lines(rotated[,1], rotated[,2], col="green")
#` 
#` # rotation matrixes are orthonormal, so the transpose is the inverse.
#` rot90 <- rotation_matrix(pi/2)
#` rot90 %*% t(rot90)

rotation_matrix <- function(theta)
	matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow=2)
