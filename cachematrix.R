## 缓存逆矩阵的函数对，从而减少逆矩阵计算的时间

## 此函数用于创建可缓存逆矩阵的特殊“矩阵”对象
## 这个特殊“矩阵”对象中有四个函数
## 分别是设置矩阵，获得矩阵，设置逆矩阵和获得逆矩阵。

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverseM <- function(IM) m <<- IM
        getinverseM <- function() m
        list(set = set, get = get,
             setinverseM = setinverseM,
             getinverseM = getinverseM)
}
}


## 此函数用于计算上述makeCacheMatrix返回的特殊“矩阵”的逆矩阵。
## 如果已经计算逆矩阵（且尚未更改矩阵）
## 那么cachesolve将检索缓存中的逆矩阵。

cacheSolve <- function(x, ...) {
        m <- x$getinverseM()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverseM(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
