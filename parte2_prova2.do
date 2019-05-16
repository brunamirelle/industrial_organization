cd "C:\Users\bruna.mirelle\Downloads"
 adopath + "C:\Users\bruna.mirelle\Downloads"

import delimited C:\Users\bruna.mirelle\Documents\flatdata.csv
drop num
rename v1 indice
rename v2 produto
rename v3 idade
rename v4 capital
rename v5 trabalho
rename v6 investimento

egen t = seq(), to(6)
egen empresa = seq(), f(1) t(531) b(6)

gen year = .
forvalue i=1/6{
replace year= 199`i' if t == `i'
}

*substituindo idade 0 por missing 
replace idade = . if idade<=0
*substituindo investimento 0 por missing 
replace investimento=. if investimento<=0

drop if missing(idade)

xtset empresa year

sort empresa year 
by empresa: gen count = _N
gen sobrevi = count == 6
gen temY1=1 if year==1996
sort empresa temY1
by empresa: replace temY1 = 1 if temY1[_n-1] == 1
replace temY1 = 0 if temY1 == .
sort empresa year
by empresa: gen tem_gap=1 if year[_n-1] !=year-1 & _n !=1
sort empresa tem_gap
by empresa: replace tem_gap =1 if tem_gap[_n-1]==1
replace tem_gap = 0 if tem_gap ==.
by empresa: generate exit = sobrevi ==0 & temY1 == 0  & tem_gap != 1  & _n == _N
replace exit = 0 if exit == 1 & year == 1996



* gerando as variáveis em log
gen ln_k = ln(capital)
gen ln_la= ln(trabalho)
gen ln_inv = ln(investimento)
gen ln_y = ln(produto)


xtset empresa year

* 3 - replique as colunas de 1 a 5 na tabela VI de Olley e Pakes (1996)
*Coluna 1 e 2: Painel balanceado
eststo: xtreg ln_y ln_la ln_k idade  if count==6
estimate store col1

eststo: xtreg ln_y ln_la ln_k idade if count==6, fe
estimate store col2

*Coluna 3, 4 e 5
eststo: xtreg ln_y ln_la ln_k idade  
estimate store col3

eststo: xtreg ln_y ln_la ln_k idade, fe
estimate store col4

eststo: xtreg ln_y ln_la ln_k idade ln_inv
estimate store col5

esttab col1 col2 col3 col4 col5 using tab1.tex, se compress

xtset empresa year
drop if missing(lninv)
// Create terms for polynomial in (i,k,a)
gen double ln_invln_k = ln_inv*ln_k
gen double ln_invidade = ln_inv*idade
gen double ln_kidade = ln_k*idade
gen double ln_invsq = ln_inv^2
gen double ln_ksq = ln_k^2
gen double idadesq = idade^2

*segunda ordem 
eststo: reg ln_y ln_la c.ln_inv##c.ln_inv c.ln_k##c.ln_k c.idade##c.idade c.ln_inv##c.ln_k c.ln_inv##c.idade c.ln_k##c.idade
estimate store col6
*terceira ordem
eststo: reg ln_y ln_la c.ln_inv##c.ln_inv##c.ln_inv  c.ln_k##c.ln_k##c.ln_k c.idade##c.idade##c.idade c.ln_inv##c.ln_inv##c.ln_k c.ln_inv##c.ln_inv##c.idade  c.ln_k##c.ln_k##c.ln_inv  c.ln_k##c.ln_k##c.idade c.idade##c.idade##c.ln_inv c.idade##c.idade##c.ln_k  
estimate store col7    
*quarta ordem 
eststo: reg ln_y ln_la c.idade##c.idade##c.idade##c.idade c.ln_k##c.ln_k##c.ln_k##c.ln_k c.ln_inv##c.ln_inv##c.ln_inv##c.ln_inv    c.ln_inv##c.ln_inv##c.ln_inv##c.ln_k c.ln_inv##c.ln_inv##c.ln_inv##c.idade c.ln_k##c.ln_k##c.ln_k##c.ln_inv c.ln_k##c.ln_k##c.ln_k##c.idade c.idade##c.idade##c.idade##c.ln_inv c.idade##c.idade##c.idade##c.ln_k         
estimate store col8

esttab col6 col7 col8 using poly.tex, se compress noomitted keep(ln_la ln_inv ln_k idade _cons)



*Etapa I  
*reg ln_y  ln_la c.ln_inv##c.ln_inv##c.ln_inv##c.ln_inv c.ln_k##c.ln_k##c.ln_k##c.ln_k c.idade##c.idade##c.idade##c.idade c.ln_inv##c.ln_inv##c.ln_inv##c.ln_k c.ln_inv##c.ln_inv##c.ln_inv##c.idade c.ln_k##c.ln_k##c.ln_k##c.ln_inv c.ln_k##c.ln_k##c.ln_k##c.idade c.idade##c.idade##c.idade##c.ln_inv c.idade##c.idade##c.idade##c.ln_k

reg ln_y ln_la c.idade##c.idade##c.idade##c.idade c.ln_k##c.ln_k##c.ln_k##c.ln_k c.ln_inv##c.ln_inv##c.ln_inv##c.ln_inv    c.ln_inv##c.ln_inv##c.ln_inv##c.ln_k c.ln_inv##c.ln_inv##c.ln_inv##c.idade c.ln_k##c.ln_k##c.ln_k##c.ln_inv c.ln_k##c.ln_k##c.ln_k##c.idade c.idade##c.idade##c.idade##c.ln_inv c.idade##c.idade##c.idade##c.ln_k         
predict double lny_hat if e(sample), xb
scalar b_ln_la = _b[ln_la]
scalar b_ln_a = _b[idade]
scalar b_ln_k = _b[ln_k]

*Etapa II - probabilidade de sobreviver
probit exit

probit exit L.(c.idade##c.idade##c.idade##c.idade c.ln_k##c.ln_k##c.ln_k##c.ln_k c.ln_inv##c.ln_inv##c.ln_inv##c.ln_inv    c.ln_inv##c.ln_inv##c.ln_inv##c.ln_k c.ln_inv##c.ln_inv##c.ln_inv##c.idade c.ln_k##c.ln_k##c.ln_k##c.ln_inv c.ln_k##c.ln_k##c.ln_k##c.idade c.idade##c.idade##c.idade##c.ln_inv c.idade##c.idade##c.idade##c.ln_k )  
predict phat if e(sample), pr

*phi_hat
gen double phi_hat = lny_hat - ln_la*b_ln_la

*gerando depvar
gen double lhs = ln_y - ln_la*b_ln_la

*marcando missing

gen marc =1 
gen l1phi = L.phi_hat
gen l1lnk = L.ln_k
gen l1idade = L.idade
foreach var of varlist lhs ln_k idade l1phi l1lnk l1idade {
replace marc = 0 if `var' >= .
}
gen double phat2 = phat^2
gen double phat3 = phat^3
gen double phat4 = phat^4


// obter os coeficientes de capital e idade 

eststo: nl (lhs = {b0} + {bk}*ln_k + {ba}*idade + {t1}*(l1phi - {bk}*l1lnk - {ba}*l1idade) + {t1sq}*(l1phi - {bk}*l1lnk - {ba}*l1idade)^2 + {t1t}*(l1phi - {bk}*l1lnk - {ba}*l1idade)^3 + {t1q}*(l1phi - {bk}*l1lnk - {ba}*l1idade)^4 + {t2}*phat + {t2sq}*phat2 + {t2t}*phat3+ {t2q}*phat4 + {t1t2}*(l1phi - {bk}*l1lnk - {ba}*l1idade)*phat + {t1t2sq}*(l1phi - {bk}*l1lnk - {ba}*l1idade)*phat2 + {t1t2t}*(l1phi - {bk}*l1lnk - {ba}*l1idade)*phat3 + {t1sqt2}*phat*(l1phi - {bk}*l1lnk - {ba}*l1idade)^2 + {t1tt2}*(phat)*(l1phi - {bk}*l1lnk - {ba}*l1idade)^3 + {t1tt2sq}*phat2*(l1phi - {bk}*l1lnk - {ba}*l1idade)^3 + {t1tt2sq}*phat3*(l1phi - {bk}*l1lnk - {ba}*l1idade)^2) if marc==1
estimate store coef
esttab coef using q5.tex, se

*kernel

*Etapa 2
gen l1lninv = L.ln_inv
gen l1lny = L.ln_y
gen l1lnla=L.ln_la
matrix h = (1.163007, 12.03426 , 0.7771964)
matrix H=diag(h)
npregress kernel exit l1lnk l1idade l1lninv ,  kernel(gaussian)  predict(phatt deriv dev1 dev2) estimator(linear) 

gen marc2 = 1
foreach var of varlist lhk ln_k idade phi_hat phatt{
replace marc2 = 0 if `var' >= .
}
xtset empresa year

eststo: nl (lhs =  {bk}*ln_k + {ba}*idade + {t1}*phatt + {t2}*(l1phi - {bk}*l1lnk - {ba}*l1idade)) if marc==1

estimate store coef2 
esttab coef2 using q6.tex, se replace

*Calculando a produtividade

gen prod_kernel = exp(ln_y - 0.831*ln_la -(0.262)*ln_k - 0.01*idade) 
gen prod_p = exp(ln_y - 0.831*ln_la  - (0.0337)*ln_k + 0.00415*idade) 

sutex prod_kernel prod_p, lab nobs key(prod) replace file(prod.tex)

*calculando o crescimento da produtividade por ano
forvalue i=1991(1)1996 {
gen share`i' = ln_k/sum(ln_k) if year==`i'
}


*media da produtividade por ano
egen pbarra = mean(prod_kernel), by(year) 

*calculando o share
bysort year: gen share = ln_y/sum(ln_y)
*share barra
egen sharebarra = mean(share), by(year)

*media ponderada da produtividade por ano
 egen pt = wtmean(prod_kernel), by(year) weight(share)  

 *covariancia 
 egen cov = corr(prod_kernel ln_y), by(year)
bysort year: correlate prod_kernel ln_y, covariance 

egen corr = corr(prod_kernel ln_k), by(year)

collapse pt pbarra cov corr, by(year)

