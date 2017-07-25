# Ohjeet Haliaksen aineiston käyttöön

**Aleksi Lehikoinen / 2017-07-23**

## Aineiston käyttö ja siihen viittaaminen

Lintuaseman aineistosta on avoimesti ladattavissa päiväkohtaiset lajisummat aseman aloituksesta vuodesta 1979 lähtien eri havainnointitavoittain: paikalliset, muutto, vakiomuutonhavainnointi, lisäalueen paikalliset sekä yömuutto. Aineisto on zip-tiedossa, ja sen voi ladata alla olevasta linkistä. Aineisto voi käyttää vapaasti kun siihen viitataan oikealla tavalla:  

Tringa society (2017) Data of the Hanko Bird Observatory: day counts. Available at URL. <Downloaded on Date-Month-Year>. 

Aineistonkeruumenetelmät on kuvattu julkaisussa Lehikoinen & Vähätalo (2000) ja Vähätalo ym. (2004). Aineiston tarjoaja ei ota vastuuta aineiston analysointitavasta tai mahdollisesti aineistossa olevista tallennusvirheistä. Aineiston käyttöön liittyvistä kysymyksistä ota yhteyttä osoitteeseen halias@tringa.fi. Mikäli aineistosta tuotetaan julkaisu, pyydetään siitä lähettämään kopio samaan osoitteeseen pdf-muodossa tai postitse osoitteeseen: 

Tringa ry  
Annankatu 29 A 16  
00100 Helsinki.  

Aineisto kattaa havainnot vuoden 2016 loppuun mennessä ja tiedostoa tullaa päivittämään uudella aineistolla noin vuoden välein.

## Aineiston sisältö

Aineisto sisältää kolme tiedostoa: 

1. Lyhyet ohjeet tiedoston rakenteesta ja käytöstä (sama kuin tämän sivun teksti)
2. Varsinainen aineistotiedosto csv-muodossa
3. R-ohjelman koodi, jolla voi nopeasti visualisoida lajikohtaisia vuodenaikaisia esiintymisiä. Aineistoa voi selailla esimerkiksi excelissä ilman R-ohjelmaa. Ilmaisen R-ohjelman voi ladata osoitteesta https://cran.r-project.org/bin/windows/base/ ja RStudion https://www.rstudio.com/products/rstudio/download/.

Tiedostossa on yhteensä 15 saraketta:

| ID | sarake       | kuvaus |
|----|--------------|--------|
| 1  | FIN_name     | Lajin tai lajiryhmän suomenkielinen nimi    |
| 2  | SWE_name     | Lajin tai lajiryhmän ruotsinkielinen nimi   |
| 3  | ENG_name     | Lajin tai lajiryhmän englanninkielinen nimi |
| 4  | Sci_name     | Lajin tai lajiryhmän tieteellinen nimi      |
| 5  | Species_Abb  | Lajin tai lajiryhmän lyhenne                |
| 6  | Species_code | Lajin tai lajiryhmän numero                 |
| 7  | Date         | Päivämäärä muodossa PÄIVÄ.KUUKAUSI.VUOSI    |
| 8  | Day-of-Year  | Ns. Juliaaninen päivämäärä eli kuluvana vuonna monesko päivämäärä. 1.1. = 1, 2.1. = 2, jne. Karkausvuosina 31.12. = 366, muuten 365. |
| 9  | Vuosi        | Havaintopäivän vuosi |
| 10 | Local        | Havaittujen paikallisten lintujen lukumäärä. Kyyhkyillä, käet, pöllöillä, kehrääjillä, kirskujilla, säihkylinnuilla, tikoilla ja varpuslinnuilla määrät tarkoittavat lukumäärää, joka on havaittu niemen kärjessä olevalta ydinalueelta. Muilla lajeilla | vesi- ja rantalinnut, petolinnut) sarakkeen luku kuvaa koko seuranta-alueella havaittujen yksilöiden määrää. |
| 11 | Migr         | Koko päivänä muutolla havaittujen lintujen lukumäärä. |
| 12 | Stand        | Vakioidulla muutonhavainnointijaksolla havaitut muuttajat. Vakiohavainnointi alkaa auringonnoususta ja kestzä 1.4.–1.11. neljä tuntia, muina aikoina kaksi tuntia. Huomaa, että vakiota ei ole suoritettu kaikkina havaintopäivinä, eli nolla arvo ei välttämättä tarkoita, että vakio on suoritettu ja nolla lintua havaittu. |
| 13 | Additional   | Ns. lisäalueella havaittujen yksilöiden lukumääriä kyyhkyjen, käkien, pöllöjen, kehrääjien, kirskujien, säihkylintujen, tikkojen ja varpuslintujen osalta. Mikäli lajia ei ole havaittu ydinalueella (ks. kohta 9 [Local]), mutta laji on havaittu lisäalueella lukumäärä on kirjattu. Lukumäärä on kirjattu myös mikäli lisäalueen havaintomäärä on merkittä. Tämän sarakkeen tiedot ovat luonteeltaan satunnaishavaintoja, joiden käyttöä ei suositella analyyseissä ilman tarkkaa harkintaa. |
| 14 | Observed     | Aseman alkuaikoina joidenkin lajien paikallislukumäärää ei välttämättä kirjattu vaikka laji havaittiin (ns. + merkintä). Mikäli laji on havaittu paikallisena, mutta lukumäärää ei ole kirjattu saa tämä sarake arvon TRUE, mikäli lajin paikallisten lukumäärä on puolestaan kirjattu (ja vaikka se olisi 0) saa sarake arvon FALSE. |
| 15 | Night_migr   | Occasional night migration sums. Mainly has importance in the Bittern, where majority of the observations are done during night. |


## Kirjallisuus

Lehikoinen, A. & Vähätalo, A. 2000: Lintujen muuton ajoittuminen Hangon lintuasemalla vuosina 1979–1999. — Tringa 27: 150–227.
Vähätalo, A. V., Rainio, K., Lehikoinen, A. & Lehikoinen, E. 2004: Spring arrival of birds depends on the North Atlantic Oscillation. — Journal of Avian Biology 35: 210–216.
