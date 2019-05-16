clear
cd "C:\Users\bruna.mirelle\Downloads"
 adopath + "C:\Users\bruna.mirelle\Downloads"


import excel "C:\Users\bruna.mirelle\Downloads\dados_prova.xls", sheet("Sheet1")

drop in 1/2
rename A market
rename B product
rename C q
rename D price
rename E x
rename F w
drop G H I J
drop in 1


destring market product q price x w, replace

* calculando os shares
egen M = sum(q), by(market)
gen share = q/M

* calculando o log
gen ln_share = ln(share)

* subtraindo o log do "outside good" do "inside good" (1 é o nosso outside good)
gen dshare = .
forval i = 1(1)500 {
    qui sum ln_share if product==1 & market==`i'
    replace dshare = ln_share - `r(max)' if market==`i'
}


sutex  q price x w share ln_share dshare , lab nobs key(descstat) replace file(descstat.tex)
corrtex  q price x w share ln_share dshare,  file(corr.tex) replace


*1 - B)

*OLS simples
eststo: reg dshare price x  
estimate store col1

*1 - C)
*IV utilizando os custos como IV para os preços
eststo: ivreg2  dshare x (price=w), gmm2 robust
estimate store col2
*IV utilizando as caracteristicas da empresa competidora como IV
*Construindo a variável
egen sum_x = sum(x), by(market)
egen count_x = count(x), by(market)
gen mean_x = (sum_x - x) / (count_x - 1)

eststo: ivreg2 dshare x (price=mean_x), gmm2s robust
estimate store col3
*1-D)
* soma das caracteristicas dos produtos das outras empresa
gen soma_x = (sum_x - x)
eststo:ivreg2 dshare x (price=soma_x), gmm2s robust
estimate store col4 
* soma das caracteristicas dos outros produtos da mesma empresa
egen soma_outros = sum(x), by(market product)
eststo:ivreg2 dshare (price=soma_outros), gmm2s robust
estimate store col5

*1-E  regressão com os dois instrumentos do item C
eststo: ivreg2  dshare x (price=w mean_x), gmm2 robust
estimate store col6

esttab col1 col2 col3  col6 using dados1.tex, se compress replace

clear
import excel "C:\Users\bruna.mirelle\Downloads\dados_prova2.xls", sheet("Sheet1")

drop in 1/2
rename A market
rename B product
rename C q
rename D price
rename E x
rename F w
drop in 1


destring market product q price x w, replace

* calculando os shares
egen M = sum(q), by(market)
gen share = q/M

* calculando o log
gen ln_share = ln(share)

* subtraindo o log do "outside good" do "inside good" (1 é o nosso outside good)
gen dshare = .
sort  market product
forval i = 3(1)500 {
    qui sum ln_share if product==1 & market==`i'
    replace dshare = ln_share - `r(max)' if market==`i'
}


sutex  q price x w share ln_share dshare , lab nobs key(descstat2) replace file(descstats.tex)
corrtex  q price x w share ln_share dshare,  file(corr2.tex) replace


*1 - B)

*OLS simples
eststo: reg dshare price x  
estimate store col7

*1 - C)
*IV utilizando os custos como IV para os preços
eststo: ivreg2  dshare x (price=w), gmm2 robust
estimate store col8
*IV utilizando as caracteristicas da empresa competidora como IV
*Construindo a variável
egen sum_x = sum(x), by(market)
egen count_x = count(x), by(market)
gen mean_x = (sum_x - x) / (count_x - 1)

eststo: ivreg2 dshare x (price=mean_x), gmm2s robust
estimate store col9
*1-D)
* soma das caracteristicas dos produtos das outras empresa
gen soma_x = (sum_x - x)
eststo:ivreg2 dshare x (price=soma_x), gmm2s robust
estimate store col10 
* soma das caracteristicas dos outros produtos da mesma empresa
egen soma_outros = sum(x), by(market product)
eststo:ivreg2 dshare (price=soma_outros), gmm2s robust
estimate store col11

*1-E  regressão com os dois instrumentos do item C
eststo: ivreg2  dshare x (price=w mean_x), gmm2 robust
estimate store col12

esttab col7 col8 col9  col12 using dados2.tex, se compress replace
