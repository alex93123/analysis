#type = p, Me, Q, m, d
format <- function (value, type){
  if (type == "text") {
    return ("error")
  }
  if (type == "p"){
    if (value >= 0.05){
      return (round(value,2))
    } else if (value >=0.001) {
      return (round(value,3))
    } else if (value >=0.0001){
      return (round(value,4))
    } else {
      return ("<0,00001")
    }
  } else {
    if (value >= 100){
      return (round(value,0))
    } else if (value >=10) {
      return (round(value,1))
    } else if (value >=0.1){
      return (round(value,2))
    } else {
      return (signif(value,digits=1))
    }
  }
}
