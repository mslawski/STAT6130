ALS <- function(X, A_init, tol = 1E-5, maxiter = 100){


    A <- A_init
    n <- nrow(X)
    d <- ncol(X)
    r <- ncol(A)
    B <- matrix(nrow = r, ncol = d)
    Xt <- t(X)

    if(is.null(A) & is.null(B))
        stop("Starting values for at least one of A or B need to be provided \n")

    objs <- numeric(maxiter)

    for(iter in 1:maxiter){

        for(j in 1:d){

            rhs <- numeric(r)

            for(i in 1:r){
                rhs[i] <- sum(A[!is.na(X[,j]),i] * X[!is.na(X[,j]),j])

            }
            lhs <- matrix(nrow = r, ncol = r, data = 0)

            for(i in 1:r){

                lhs[i,] <- colSums(sweep(A, STATS = A[,i], FUN = "*", MARGIN = 1)[!is.na(X[,j]),])
            }

            B[,j] <- solve(lhs, rhs)

        }

        B_t <- t(B)


        for(j in 1:nrow(A)){

            rhs <- numeric(r)

            for(i in 1:r){
                rhs[i] <- sum(B_t[!is.na(Xt[,j]),i] * Xt[!is.na(Xt[,j]),j])
            }

            lhs <- matrix(nrow = r, ncol = r, data = 0)

            for(i in 1:r){
                lhs[i,] <- colSums(sweep(B_t, STATS = B_t[,i], FUN = "*", MARGIN = 1)[!is.na(Xt[,j]),])
            }

            A[j,] <- solve(lhs, rhs)

        }

        Xhat <- A %*% B
        objs[iter] <- sum((Xhat[!is.na(X)] - X[!is.na(X)])^2)

        if(iter > 1){
            if(objs[iter - 1] - objs[iter] < tol)
                break
        }

    }

    objs <- objs[1:iter]

    return(list(A = A, B = B, Xhat = Xhat, objs = objs))

}


