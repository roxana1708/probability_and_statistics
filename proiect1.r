#Ex1-------
const_normalizare <- function(f) {
  if(any(sapply(seq(-100, 100, length.out = 1000), f) < 0)){
    stop("Functie are valori negative negativa")}
  else {
    tryCatch(integ <- integrate(Vectorize(f), lower = -Inf, upper = Inf)$value,
             error= function(err)
             {
               stop("Integrala e divergenta")
             })
    if(integ == 0)
    {stop("Functia data nu are contanta de nor")
    }
    const_norm <- 1/integ
    return(const_norm)
  }
}

#Ex2-------
verif_dp <- function(func, i_inf, i_sup) {
  ok <- 0
  
  if(any(sapply(seq(i_inf, i_sup, by = 0.05), func)) < 0) {
    #print("Functia nu este densitate de probabilitate, deoarece incalca proprietatea 1")
    ok <- 1
    return(ok)
  }
  
  if((integrate(Vectorize(func), i_inf, i_sup)$value) != 1) {
    #print("Functia nu este densitate de probabilitate, deoarece incalca prorietatea 2")
    ok <- 2
    return(ok)
  }
  
  return(ok)
}

afisarea_verif_dp <- function(func, intrv) {
  okk = verif_dp(func, intrv)
  if(okk == 0) {
    print(TRUE)
  }
  if(okk == 1) {
    print("Functia nu este densitate de probabilitate, deoarece incalca prorietatea 2")
  }
  if(okk == 2) {
    print("Functia nu este densitate de probabilitate, deoarece incalca proprietatea 1")
  }
}

#Ex3-------
setClass("va_cont", slots = list(densitate = "function", inf = "numeric", sup = "numeric"))

va_cont <- function(densitate, inf, sup) {
  if(verif_dp(densitate, inf, sup)==0) {
    object <- new("va_cont", densitate = densitate, inf = inf, sup = sup)
    return(object)
  } else {
    afisarea_verif_dp(densitate, inf, sup)
  }
}

probabilitate <- function(object, i, j) {
  if(i >= object@inf && j <= object@sup) {
    integrate(Vectorize(object@densitate), i, j)$value
  }
}

setMethod("show", "va_cont", function(object) {
  print("Densitatea de probabilitate: ")
  print(object@densitate)
  print(paste("Marginea inferioara ", object@inf))
  print(paste("Marginea superioara ", object@sup))
})

#Ex4-------
p4n <- function()
{
  dNormala <- rnorm(sample(1:100, 1), mean = 0, sd = 1) # generam valori aleatorii pentru repartitia normala
  cdfNormala <- pnorm(dNormala) # obtinem functie de repartitie
  
  hnorm <- hist(cdfNormala,breaks=length(dNormala), col="red", xlab="",main="Repartitie normala")
  xsz <- seq(min(cdfNormala), max(cdfNormala), length=length(cdfNormala))
  ysz <- dnorm(xsz, mean=mean(cdfNormala), sd=sd(cdfNormala))
  ysz <- ysz*diff(hnorm$mids[1:2]*length(cdfNormala))
  lines(xsz, ysz, col="orange", lwd = 2)
}


p4aprox <- function()
{
  dAprox <- c(sample(1:100, sample(1:100,1)))
  cdfAprox <- pnorm(dAprox)
  haprox <- hist(cdfAprox, breaks=length(dAprox), col="red", main="Repartitie cu aproximare")
  xsz <- seq(min(cdfAprox),max(cdfAprox), length = length(cdfAprox))
  ysz <- dnorm(xsz, mean=mean(cdfAprox), sd=sd(cdfAprox))
  ysz <- ysz*diff(haprox$mids[1:2]*length(cdfAprox))
  lines(xsz, ysz, col="orange", lwd =2)
}


#Ex5-------
media <- function(f)
{
  xfx <- function(x) {return (x*f(x))}
  return(integrate(xfx, -Inf, +Inf)) 
}

momentC <- function(f, ord)
{
  m <- media(f)
  dif <- function(x){ return( (x-m) ^ ord )} 
  
  arg <- function(x){ return( dif(x) * f(x) )} #extragem argumentul pe care il integram
  
  #tratam cazul in care nu exista moment pana la ordinul dat
  tryCatch(
    
    expr = {(integrate(arg, -Inf, +Inf))$value},
    error = function(err){ print(err)}
  )
}


dispersia <- function(f)
{
  return(momentC(f, 2)) #calculam momentele centrate de ordin 2
}

momenteInit <- function(f, ord)
{
  arg <- function(x,f){return( x^ord * f(x) )}
  tryCatch(
    expr = {(integrate(arg, -Inf, +Inf))$value},
    error = function(err){ print(err)}
  )
}

#Ex6-------
media_uniform <- function(Func, a, b) #media pentru repartitia uniforma, Func este functia data de utilizator
{
  densitate <- function(x){ 1/(b-a) }  #functia de densitate
  Func_final = Multiply(Func, densitate)   #functia finala de integrat
  medie <- integrate(Vectorize(Func_final), a, b)
  medie = as.numeric(medie[1])
  return(fractions(medie))
}

dispersia_uniform <- function(Func, a, b)  #dispersia pentru repartitia uniforma
{
  Func1 = Multiply(Func, Func)
  exp1 <- media_uniform(Func1, a, b)  #Am folosit formula Var(X) = E(X^2) - E(X)^2
  exp2 <- media_uniform(Func, a, b)
  dispersie = as.numeric(exp1[1]) - as.numeric(exp2[1])^2  #disperia calculata dupa formula de mai sus
  return(fractions(dispersie))
}

Multiply = function(a, b)
{
  force(a)
  force(b)
  function(x){a(x) * b(x)}
}


#Ex8-------
rep <- function(nume_repartitie){
  
  if(nume_repartitie=="Repartitie_binominala_extern")
  {binominala
  }else if(nume_repartitie=="Repartitie_poisson_extern")
  {poisson}
  else if(nume_repartitie=="Repartitie_continua_uniforma_extern")
  {continua_uniforma}
  else if(nume_repartitie=="Repartitie_gamma_extern")
  {gamma_}
  else if(nume_repartitie=="Repartitie_beta_extern")
  {beta_}
  else if(nume_repartitie=="Repartitie_X2_extern")
  {helmert}
  else if(nume_repartitie=="Repartitie_uniforma")
  {curs_uniforma}
  else if(nume_repartitie=="Repartitie_exponentiala")
  {curs_exponentiala}
  else if(nume_repartitie=="Repartitie_normala")
  {curs_normala}
  else stop("Stringul introdus nu reprezinta una din alegerile posibile")
  
}


binominala <- data.frame(
  " " = c("Notiune:","Definitie:","Definitie","Definitie","Media:","Dispersia:","Sursa:"),
  " "=c("Repartitia Binominala",
        "Variabila  aleatoare  discretă X urmează legea binomială(X are o repartiţie binomială) cu parametrii",
        "n şi p (n∈ℕ, 0<p<1) dacă ia valorile 0,1,2,....,n cu probabilităţile",
        "P(X=k) =Combinari de n luate cate k de p^k*q^n-k, k=0,1,2,....,n, unde q=1-p",
        " E(X)=np",
        "Var(X)=npq.",
        "http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf"))


poisson <- data.frame(
  " " = c("Notiune:","Definitie:","Definitie","Definitie","Media:","Dispersia:","Sursa:"),
  " "=c("Poisson(legea evenimentelor rare)",
        "Variabila aleatorie X urmeaza legea Poisson(X are repartitie Poisson)",
        "cu parametrul  λ ( λ>0) daca poatea lua orice valoare intreaga pozitiva si" ,
        "P(X=k)= ((λ^k)/(k!))*e^-λ, k=0,1,2,....,n",
        "E(X)=λ",
        "Var(X)=λ",
        "http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf") )


continua_uniforma <- data.frame(
  " " = c("Notiune:","Definitie:","Definitie","Definitie","Media:","Dispersia:","Sursa:"),
  " "=c("Legea continuă uniformă (rectangulară)",
        "Variabila aleatorie X urmeaza legea continuă uniformă (rectangulară)(X are repartiţie" ,
        "uniformă) cu parametrii μ si ω(μ∈ℝ ω>0 ) dacă densitatea sa de probabilitate (repartiţie) este",
        "funcţia: f(x)={ 1/ω, x∈[μ-ω/2, μ+ω/2],||0, x∈(-∞, μ-ω/2)∪(μ+ω/2, ∞]}",
        "E(X)=μ",
        "Var(X)=(ω^2)/12",
        "http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf"))


gamma_ <- data.frame(
  " " = c("Notiune:","Definitie:","Definitie","Moment initial de ordin k:","Media:","Dispersia:","Sursa:"),
  " "=c("Legea gamma",
        "Variabila aleatoare X urmează legea  gamma (X are repartiţie gamma)",
        "cu parametrul p(p>0) dacă densitatea sa de probabilitate (repartiţie) este funcţia:",
        "f(x)={(λ^p*x^p-1*e^-λx)/Γ(p), x>0, ||0, x<=0}",
        "E(X)=m1(X)=p",
        "Var(X)=m2(X)-[m1(X)]^2=p",
        "http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf"))


beta_ <- data.frame(
  " " = c("Notiune:","Definitie:","Definitie","Definitie","Moment initial de ordin k:","Media:","Dispersia:","Sursa:"),
  " "=c("Legea beta",
        "Variabila aleatoare X urmează legea beta (X are repartiţie beta)cu parametrii",
        "p şi q (p,q>0) dacă densitatea sa de probabilitate (repartiţie) este funcţia",
        "f(x)={(1/(B(p,q)))x^p-1(1-x)^(q-1), x∈(0,1) || 0, x∈!(0,1)} unde B este integarala Euler de primul tip.",
        
        "mk(X)=(p(p+1)...(p+k-1))/ (p+q)(p+q+1)....(p+q+k-1) ,k∈ℕ*",
        "E(X)=m1(X)=p/(p+q)",
        "Var(X)=m2(X)-[m1(X)]^2=pq/((p+q)^2*(p+q+1))",
        "http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf"))

helmert <- data.frame(
  " " = c("Notiune:","Definitie:","Definitie","Definitie","Media:","Dispersia:","Sursa:"),
  " "=c("Legea x^2 (Helmert-Pearson)",
        "Variabila aleatoare X urmează legea X^2 (Helmert-Pearson) (X are repartiţie X^2) cu parametrul ",
        "n (n∈ℕ) dacă densitatea sa de probabilitate (repartiţie) este funcţia f(x)={(1/(2^(n/2)Γ*(n/2)))*x^(n/2-1)*e^(-x/2),  x>0 ",
        "||0, x<=0} unde B este integarala Euler de primul tip.",
        "E(X)=m1(X)=n",
        "Var(X)=m2(X)-[m1(X)]^2=2n",
        "http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf"))



curs_uniforma <- data.frame(
  " " = c("Notiune:","Parametri:" ,"Domeniu de valori: " ,"Notatie: ","Densitate:","Repartitie:","Modele:" ,"Sursa:"),
  " "=c("Repartitia uniforma",
        "a < b.",
        "[a,b].",
        "uniform(a,b) sauU(a,b).",
        "f(x) =1b−apentrua≤x≤b.",
        "F(x) = (x−a)/(b−a) pentrua≤x≤b.",
        " Toate rezultatele din domeniul de valori au probabilitate egal ̆a(mai precis, toate rezultatele au aceea ̧si densitate de probabilitate).",
        "Curs 5/prof. Cristian Niculescu"))


curs_exponentiala <- data.frame(
  " " = c("Notiune:","Parametri:" ,"Domeniu de valori: " ,"Notatie: ","Densitate:","Repartitie:","Modele:" ,"Sursa:"),
  " "=c("Repartitia exponentiala",
        "λ >0." ,
        "[0,∞).",
        "exponential(λ) sau exp(λ).",
        "f(x) =λe−λx pentru x≥0.",
        "F(x) = 1−e−λx pentrux≥0.,P(X > x) = 1−F(x) =e−λx.",
        "Timpul de asteptare pentru un proces continuu de schimbare a starii.",
        "Curs 5/prof. Cristian Niculescu"))


curs_normala <- data.frame(
  " " = c("Notiune:","Parametri:" ,"Domeniu de valori: " ,"Notatie: ","Densitate:","Repartitie:","Modele:" ,"Sursa:"),
  " "=c("Repartitia normala ",
        "μ∈R,σ >0.3" ,
        "R",
        "normal(μ,σ2) sau N(μ,σ2).",
        "f(x) =1σ√2πe−(x−μ)2/2σ2",
        "F(x)  nu  are  formula,   asa  ca folosim  tabele  sau  comenzi  capnormın R pentru a calcula F(x)",
        "masurarea  erorii,  inteligentei/abilitatii,  ınaltimii,  mediilor  loturilor de date.",
        "Curs 5/prof. Cristian Niculescu"))

