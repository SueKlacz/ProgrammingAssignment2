makeCacheMatrix<-function(x=numeric()) {
  ## This function creates a special matrix 
  ## object that can cache its inverse.
  m<-NULL
  set<-function(y){
      x<<-y
      m<<-NULL
  }
  get<-function()x
  setcache<-function(solve) m<<-setcache
  getcache<-function() m
  list(set=set,get=get,
       setcache=setcache,
       getcache=getcache)
}

cacheSolve<-function(x, ...) {
  ## This function computes the inverse of the special
  ## matrix returned by the makeCacheMatrix function.
  m<-x$getcache()
  if(!is.null(m)) {
      ## This if statement will check to see if function is
      ## already cached. If so, it will used cached data; if not,
      ## it will create
      message("getting cached data")
      return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setcache(m)
  m
}
 