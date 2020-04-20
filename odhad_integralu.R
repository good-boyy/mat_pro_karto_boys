
# horni a dolni odhad Riemannova integralu

#------------------------------------------------------------------------
# vstupni parametry -- zadava uzivatel

funkce <-function(x){
  y = -5*x^2 + 3*x + 5  # predpis funkce 1 promenne x
}

  
a = 1     # krajni body
b = 2

n = 200000    # pocet intervalu, na ktery chci usek mezi krajnimi body rozdelit

#------------------------------------------------------------------------

move = (b - a)/n # velikost intervalu

# hodnoty na ose x
hodnoty_x <- seq(from=a,to=b,by=move)

# hodnoty na ose y
hodnoty_y <- funkce(hodnoty_x)


# -----------------------------------------------------------------------------------
# horni odhad

h_odhad = c()
h_x = c()

p = 1
for (i in hodnoty_x[1:n]){
  
  if(p==n+1){
    j = i
  }else{
    j = i + move
  }
  
 
  if(funkce(j)>funkce(i)){
    h_x[p] <- j
    h_odhad[p] <- funkce(j)
  } else{
      h_x[p] <- i
      h_odhad[p] <- funkce(i)
    }
  p = p + 1
  
}


# ------------------------------------------------------------------------------------
# dolni odhad

d_odhad = c()
d_x = c()

q = 1
for (k in hodnoty_x[1:n+1]){
  
  l = k - move

  if(funkce(l)<funkce(k)){
    d_x[q]  <- l
    d_odhad[q] <- funkce(l)
  } else{
    d_x[q] <- k
    d_odhad[q] <- funkce(k)
  }
  q = q + 1
  
}


#--------------------------------------------------------------------------------
# finish

h_soucet = sum(h_odhad*move)
d_soucet = sum(d_odhad*move)



plot(hodnoty_y~hodnoty_x,type="h")
lines(hodnoty_y~hodnoty_x)

#lines(h_odhad~h_x, col = "red")
#lines(d_odhad~d_x, col = "blue")


abline(h=0,v=a)
abline(h=0,v=b)

H <- paste("Horní součet:", h_soucet, sep = " ")
D <- paste("Dolní součet:", d_soucet, sep = " ")

result <- paste(H, D, sep = "\n")

cat(result)

