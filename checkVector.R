check <-function (vec){
  val <- vec[1]
  counter = 1
  for (k in 2:length(vec)){
    if (vec[k] == val)
      counter <- counter+1
  }
  if (counter == length(vec)){
    return (T)
  } else {
    return (F)
  }
}
