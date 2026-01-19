###
set.seed(918)

### 0.1 Matrices and vectors

d <- 5
n <- 10
v <- rnorm(5)
M <- matrix(rnorm(n * d), nrow = n, ncol = d)

# vectorization of M (stacking column by column)
vecM <- c(M)
# vectorization of M (stacking row by row)
Mt <- t(M) # matrix tranpose
vecM <- c(Mt)

### 0.2 Transposition and Symmetry
# create a symmetric matrix
A0 <- matrix(rnorm(n * n), nrow = n, ncol = n)
A <- 0.5 * (A0  + t(A0))
At <- t(A)
all.equal(A, At) # confirm A is symmetric
length(unique(c(A))) == n * (n + 1) / 2 # degrees of freedom = number of unique elements here

### 0.3 Dot product

# vectors
w <- rnorm(d)
abs(sum(v * w) - as.numeric(t(w) %*% v)) # equal

# orthogonality
u1 <- c(-1, 0.5, 2)
u2 <- c(1, 1, 0.25)
sum(u1 * u2)

# trace inner product on matrices
A1 <- matrix(rnorm(n * d), nrow = n, ncol = d)
B1 <- matrix(rnorm(n * d), nrow = n, ncol = d)
# all equivalent
sum(c(A1) %*% c(B1))
sum(diag(t(A1) %*% B1))
sum(diag(t(B1) %*% A1))

### 0.4 Matrix vector multiplication
Mv <- M %*% v # Matrix-vector multiplication

k <- 7
A2 <- A1
B2 <- matrix(rnorm(d * k), nrow = d, ncol = k)
C <- A2 %*% B2 # matrix product
all.equal(dim(C), c(nrow(A2), ncol(B2))) # check dimensions

# associativity
l <- 8
C2 <- matrix(rnorm(k * l), nrow = k, ncol = l)
all.equal((A2 %*% B2) %*% C2, A2 %*% (B2 %*% C2))

### 0.5 other matrix operations --- skipped

### 0.6 norms on vectors

norm_p  <- function(v, p) (sum(abs(v)^p))^(1/p)
norm_p(v, 2)
sqrt(sum(v * v)) # equivalent
norm_p(v,1)
norm_p(v, 100)
max(abs(v)) # numerically equivalent

# Frobenius norm of matrices
norm_p(c(A1), 2)
sqrt(sum(A1^2)) # equivalent

### 0.7 Matrix Rank, Range, Nullspace

# the svd function (singular value decomposition) provides it all:
svdC2 <- svd(C2, nu = nrow(C2),nv =  ncol(C2))
U <- svdC2$u # basis of the range space
sum(svdC2$d > 1E-10) # rank = number of non-zero singular values
# basis of the nullspace
vnull <- svdC2$v[,(sum(svdC2$d > 1E-10)+1):ncol(C2)]
C2 %*% vnull # numerically zero

### 0.8 Matrix inverse

A0inv <- solve(A0)
all.equal(A0 %*% A0inv, diag(n)) # check that the result is the identity

### 0.9 eigenvalues and eigenvectors, sepctral decomposition

eigenA <- eigen(A)
U <- eigenA$vectors
Lambda <- diag(eigenA$values)
all.equal(U %*% Lambda %*% t(U), A) # verify spectral decomposition

# determinant
determinant(A)
lambda <- diag(Lambda)
prod(lambda)
# trace
all.equal(sum(diag(A)), sum(lambda))

### 0.10 spectral norm

max(abs(lambda))

### 0.11 quadratic forms and positive definiteness

u1 <- U[,1,drop = FALSE]
t(u1) %*% A %*% u1 # equals lambda[1]

A3 <- matrix(rnorm(n * d), nrow = n, ncol = d)
A4 <- A3 %*% t(A3) # this matrix is positive definite
eigenA4 <- eigen(A4)
min(eigenA4$values) # all eigenvalues are (numerically) non-negative
# compute matrix square root
R <- eigenA4$vectors %*% diag(sqrt(eigenA4$values + 1E-12))
all.equal(A4, R %*% t(R)) # check

### 0.12 derivatives --- skipped

### 0.13 least squares and projections
b <- rnorm(n)

# normal equations
vstar <- solve(crossprod(A), crossprod(A, b)) # crossprod(A): short for t(A) %*% A; crossprod(A, b): short for t(A) %*% b

# projection matrix
P <- A %*% solve(crossprod(A), t(A))
all.equal(A %*% vstar, P %*% b) # check
all.equal(P, t(P))
all.equal(P, P%*%P) # careful: note that P^2 squares the entries of P
eigen(P)$values


