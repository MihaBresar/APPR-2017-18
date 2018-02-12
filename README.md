# Analiza podatkov s programom R, 2017/18

## Analiza naborov v ligi NBA
Avtor: Miha Brešar

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2017/18

## Tematika

V projektu bom analiziral nabore v ligi NBA. Analiziral bom uspešnost ekip in igralcev. Igralce bom ocenjeval na podlagi statistike in dobrinosa ekipi. Ekipe pa na podlagi uspešnosti igralcev ki so jih izbrale na naborih. S to analizo želim ugotoviti, katere ekipe so najboljše pri izbiranju mladih igralcev. Analiziral bom tudi univerze s katerih prihajajo igralci. Večina igralcev, ki so izbrani na naboru, namreč prihaja z ameriških univerz. 

Za vir podatkov bom uporabil:

https://www.basketball-reference.com/draft/ 

https://en.wikipedia.org/wiki/NBA_draft 

## Tabele:

V tabelah bom zbral podatke:

## Tabela1 (Osebni podatki igralcev):
 
  1. Ime in Priimek igralca

  2. Država

  3. Izbor

  4. Leto nabora  
  
  5. Pozicija
  
  6. Ekipa

 ## Tabela2 (Statistični podatki Igralcev) :
 
 1. Odigrane tekme
 2. Minute/tekmo
 3. Točke/tekmo
 .
 .
 .

 
Načrt:

Z dobljenimi podatki bom poskušal napovedati uspešnost igralcev, ki bodo letos izbrani na naboru.

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`. Ko ga prevedemo,
se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`. Podatkovni
viri so v mapi `podatki/`. Zemljevidi v obliki SHP, ki jih program pobere, se
shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `maptools` - za uvoz zemljevidov
* `sp` - za delo z zemljevidi
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `reshape2` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)
