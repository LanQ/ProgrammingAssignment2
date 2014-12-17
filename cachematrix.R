# 基本功能参考示例的code：
# 设置矩阵值
# 获取矩阵值
# 设置逆矩阵值
# 获取逆矩阵值
##########################################
# 验证方法
# 初始化一个矩阵： x <- matrix(1:4, 2, 2)
# 创建矩阵函数： p <- makeCacheMatrix(x)
# 初次求逆(调用solve计算)： cacheSolve(p)
# 再次求逆(从cache里取值)： cacheSolve(p)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


##获取逆矩阵

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        
        # 利用solve函数求逆矩阵
        m <- solve(data, ...)
        x$setinverse(m)
        m
}