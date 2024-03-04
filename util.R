# Funktioner til brug i shinyApp

# Laver dataframe med punkter til trekanten
dfForSingleShotTri <-  function(shot) {
  g1=c(120, 36)
  g2=c(120,44)
  x=c(shot[1],g1[1],g2[1],shot[1])
  y=c(shot[2],g1[2],g2[2],shot[2])
  resshot=data.frame(x,y)
  colnames(resshot)=c("sx","sy")
  return(resshot)
}

#test
# Function to check if a point is inside the triangle
point_inside_triangle <- function(point, triangle_vertices) {
  # Extract vertices
  A <- triangle_vertices[1, ]
  B <- triangle_vertices[2, ]
  C <- triangle_vertices[3, ]
  
  # Compute barycentric coordinates
  lambda1 <- ((B[2] - C[2]) * (point[1] - C[1]) + (C[1] - B[1]) * (point[2] - C[2])) /
    ((B[2] - C[2]) * (A[1] - C[1]) + (C[1] - B[1]) * (A[2] - C[2]))
  lambda2 <- ((C[2] - A[2]) * (point[1] - C[1]) + (A[1] - C[1]) * (point[2] - C[2])) /
    ((B[2] - C[2]) * (A[1] - C[1]) + (C[1] - B[1]) * (A[2] - C[2]))
  lambda3 <- 1 - lambda1 - lambda2
  
  # Check if the point is inside the triangle
  return(all(c(lambda1, lambda2, lambda3) >= 0))
}

# Funktion til at måle distance mellem to punkter
dist2d <- function(a,b,c){
  v1 <- b-c
  v2 <- a-b
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
}
