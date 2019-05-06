
seq_valid <- function(n,j,k,modulo){
  j = min(j,k); y=j-1 %% modulo
  if (j==1 & n>1) {y=0}
  else
    for(i in 3:n){
      
      x = (j-1) %% modulo
      y = (x*y) %% modulo
    }
  
  return(y)
}


n=4; j=4; k=2
modulo = 10^10+7
Q2_a=seq_valid(n,j,k,modulo)

n=4; j=100; k=1
modulo = 10^10+7
Q2_b=seq_valid(n,j,k,modulo)

n=100; j=100; k=1
modulo = 10^10+7
Q2_c=seq_valid(n,j,k,modulo)

n=347; j=2281; k=829
modulo = 10^10+7
Q2_d=seq_valid(n,j,k,modulo)

n=1.26*10^26; j=4.17*10^6; k=1
modulo = 10^10+7
Q2_e=seq_valid(n,j,k,modulo)

n=10^7; j=10^12; k=829
modulo = 10^10+7
Q2_f=seq_valid(n,j,k,modulo)