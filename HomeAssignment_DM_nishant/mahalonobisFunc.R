myMahaFunction <- function(element1,element2,icov){
  dm= sqrt(t(element1-element2)%*%icov%*%(element1-element2))
  return(dm)
}