#cortest matrix function
cor.test.mat <- function(data, vec_pos, type = "p.value"){
  cortestl <- list()
  cortest_mat <- matrix(NaN, length(vec_pos), length(vec_pos))
  for (i in vec_pos) {
    
    for (k in vec_pos) {
      re_test <- cor.test(data[, i], data[, k])
      if(type == "p.value"){
        cortest_mat[i, k] <- round(re_test$p.value, 3)
      }
      
      if(type == "estimate"){
        cortest_mat[i, k] <- round(re_test$estimate, 3)
      }
      
      if(mode(type) == "numeric"){
        cortest_mat[i, k] <- round(re_test[type], 3)
      }
    }
  }
  return(cortest_mat)
}