
/* This program is distributed under GNU General Public License.
   We hope that the study of the source code may be usefull for somebody.
   Authors: Petr Olsak + Mirek Olsak.  See also: www.olsak.net.  October 2003.

   Compile me by: cc -O2 -o grid grid.c

   Sorry, the comments are only in Czech language. For user documentation
   (in English) see the gridoce.tex or gridoce.pdf file.
   */

/* [uvod] Program resi lustovky typu griddlers, resp. nonograms, 
   viz napr. www.griddlers.net. Uzivatelska dokumentace je v souboru
   gridoc.tex nebo gridoc.pdf.

   Predpokladame, ze bezduche pouzivani tohoto programu nebude pro
   mnoho lidi prilis zabavne. Zabavnejsi je reseni tech lustovek
   manualne.  Za daleko nejzabavnejsi ale povazujeme studium a
   vymysleni algoritmu, kterymi se lustovky daji resit a implementovat
   do pocitace.  Proto je v tomto zdrojovem kodu dukladny komentar ke
   vsem pouzitym algoritmum.

   Algoritmy by mely byt pokud mozno inteligentni, protoze vyuziti brutalni 
   sily (rychlosti pocitace) je mnohdy nemozne nebo velmi neuzitecne. Kdo 
   vymysli algoritmus jeste ucelnejsi, nez ten, ktery je zde implementovan, 
   necht da autorum vedet. Dekujeme.  
   */

/* [limity] MAXBLOKU je maximalni pocet bloku v jednom radku
   resp. sloupci, MAXBAREV je maximalni pocet barev. MAXRADKU je
   maximalni pocet radku lustovky, MAXSLOUPCU je maximalni pocet
   sloupcu. MAXBUF je maximalni delka jmena souboru, ktery je
   nacitan. LIMITPOKUSU je maximalni pocet uspesnych pokusu, jehoz
   prekroceni vede k bezprostrednimu ukonceni tzv. intenzivniho
   algoritmu, viz [dusleny].  MAXDELKASLOVA je nejvetsi delka slova
   pro deklaraci barvy v XPM formatu, viz [savexpm]. 
   TRACKLIMIT je delka pameti na zapamatovani zmen, viz [ulozdopasku].

   Pri nacitani ulohy ze souboru je kontrolovano, zda neni prekrocen
   nektery limit. Pokud ano, program skonci s chybovou zpravou a
   vypise jmeno limitu.  Uzivatel pak muze zmenit hodnotu tohoto
   limitu a program prekompilovat znovu.  
   */
   
#define MAXBLOKU       30
#define MAXBAREV       10
#define MAXRADKU       200
#define MAXSLOUPCU     MAXRADKU
#define MAXBUF         100
#define LIMITPOKUSU    10000000
#define MAXDELKASLOVA  30

/* [defaults] Nasleduji implicitni hodnoty promennych, ktere
   se daji zmenit pomoci parametru prikazoveho radku:
   */

int      llevel =     2 ;   /* -log */
int      outlevel =   2 ;   /* -out */
long int steps =      0 ;   /* -stop */
long int paused =     0 ;   /* -p */
int      xpmnum =     1 ;   /* -xpm */
int      totalnum =  30 ;   /* -total */
int      limitbloku = 7 ;   /* -bl */

/* [prototypy] Uvedeme prototypy vsech pouzitych funkci, 
   abychom mohli dale psat funkce v poradi, jak to vyhovuje 
   cloveku a nikoli v poradi, jak to vyzaduje prekladac.
   */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int   cisloradku (int r);
void  cmdparser (int argc, char**argv);
void  dalsikrok ();
void  error (char *s);
void  formaterror (char *s);
char  genbarva (int kmax);
void* getmemory (long int i, char *name);
void  gridprintpole (int b);
int   iicd (int k);
int   iief (int k);
void  inipole ();
int   jakabarva (int i, int j);
void  logradek (int jak);
int   main (int argc, char**argv);
int   nactibarvu (int jak);
int   nacticislo ();
void  nactipart (FILE *f);
void  nactiproblem ();
int   nactiradek ();
int   najdidalsi (int od, int starti, int startb);
int   najdiprvni ();
FILE* openfile (char *name);
int   posunblok (int j, int i);
void  pprint (FILE *f, int i, int j, int k);
char  primoradek (int i, int b);
void  printbloky (int * p, char *s);
void  printpole (int podrobne);
void  printstatistic ();
int   projdidukladne ();
char  rpole (int i, int b);
char  rpolei (int i, int b);
void  rprint (FILE*f, int k, char c);
void  savexpm ();
void  skipblanks ();
void  skipline ();
void  spole (int i, int b, char c);
int   sumabarev (int b);
void  testkonzistence (int i, int y, int z);
int   testujradek (int i, int j);  
int   tridmaxll ();
void  tridprintpole (int b);
void  tridsavexpm (FILE *f);
void  tridulozprvek (int k, int b, char c);
int   tri (int k);
int   trj (int k);
void  trset (int r);
void  ulozdopasku (int i, int ba);
void  ulozprvek (int i, int b, char c);
void  ulozreseni ();
void  usage ();
void  vemzpasku ();
void  vsechnareseni ();
int   vyres ();
void  vyresdukladneradek ();
int   vyresradek ();
void  wlog (int level, char *s, long int value);

#define MAX(A,B)  (((A)>(B))?(A):(B))

/* [sumarium] Deklarujeme globalni promenne, ktere pocitaji
   pocet pruchodu, pocet reseni radku/sloupcu atd. pro potreby
   sumarni informace tistene po vyreseni lustovky.
   */

unsigned long int sumkr = 0 ;  /* pocet kroku typu r */
unsigned long int sumkc = 0 ;  /* pocet kroku typu c */
unsigned long int sumki = 0 ;  /* pocet kroku typu i */
unsigned long int sumkt = 0 ;  /* pocet kroku typu t */
unsigned long int sumrr = 0 ;  /* pocet reseni radku rychlym algoritmem */
unsigned long int sumur = 0 ;  /* pocet uspechu rychlym algoritmem */
unsigned long int sumri = 0 ;  /* pocet reseni radku intenzivnim algoritmem */
unsigned long int sumui = 0 ;  /* pocet uspechu intenzivnim algoritmem */
unsigned long int sumr  = 0 ;  /* pocet nalezenych reseni */

/* [vtupnidata] Popiseme strukturu vstupnich dat, se kterymi pracuji
   algoritmy na reseni lustovky. Tato data se inicializuji po precteni
   vstupniho souboru pomoci funkce nactiproblem(). Jak je implementovano
   cteni souboru nas bude zajimat az pozdeji, viz [nactiproblem].

   V programu nerozlisujeme mezi radky a sloupci. Tim si usetrime
   psani dvou variant stejnych funkci, jednou pro radky a podruhe pro
   sloupce. Udaje o radcich jsou tesne nasledovany udaji o sloupcich a
   pouze podle promennych "pocetradku" a "pocetsloupcu" pozname, jaka je
   geometrie lustovky. K idexovani prave zkoumaneho radku/sloupce
   pouzivame globalni promenou "ii". Je-li "ii < pocetradku", pracujeme
   skutecne s radkem a jinak pracujeme se sloupcem. Pri nastaveni
   promenne "ii" nastavime take delku radku "ll" bud na hodnotu
   "pocetsloupcu" nebo "pocetradku".

   V tomto textu budeme dale pouzivat pojem "zobecneny radek" nebo jen
   "radek", tj. muze se jednat ve skutecnosti o sloupec. Pokud chceme
   mluvit o skutecnem radku lustovky, pak piseme "realny radek".

   Pole vb[][] obsahuje velikosti bloku tak, jak jsou uvedeny v zadani.  
   Pole pb[] obsahuje pocet techto bloku pro kazdy radek.
   Pole bb[][] obsahuje indexy barev odpovidajicich bloku. Index nula
   je rezervovan pro barvu pozadi a promenna pocetbarev udava pocet
   pouzitych barev vcetne barvy pozadi.
   Pole vm[][] udava velikost minimalni mezery za blokem. Je-li nula,
   pak nasledujici blok muze tesne nasledovat, je-li 1, pak pred 
   nasledujicim blokem musi byt aspon jedno volne misto. 
   Vyjimku tvori hodnota vm[][] = -1, coz znaci, ze nasledujici blok
   je pripojen bez mezery a navic se nesmi od stavajiciho bloku
   oddelit.

   Pole barva[] prevadi index barvy na znak, ktery se pro barvu 
   ma pouzit pri tisku na terminal. Tj. barva[bb[ii][j]] znamena 
   znak barvy j-teho bloku na ii-tem radku.  */

int pocetradku, pocetsloupcu, pocetbarev;
int ii, ll;
int dvebarvy;     /* totez jako pocetbarev==2, ale rychlejsi vyhodnoceni */
int triddlers=0;  /* resime triddlers? */
int maxj;         /* maximalni sirka radku pri triddlers */

int  vb[MAXRADKU+MAXSLOUPCU][MAXBLOKU];  /* velikosti bloku */
int  pb[MAXRADKU+MAXSLOUPCU];            /* pocet bloku */
int  bb[MAXRADKU+MAXSLOUPCU][MAXBLOKU];  /* barva bloku */
char vm[MAXRADKU+MAXSLOUPCU][MAXBLOKU];  /* velikost mezery */
char barva[MAXBAREV];                   /* barva pro vytisteni na terminal */

/* [pole] Vysvetlime strukturu dat, ktere popisuji castecne vyresenou
   lustovku. Hlavni pameti na ukladani reseni je pole[][][]. Jedna se
   o vice vrstev dvourozmernych matic znaku, kazda vrstva (indexovana
   poslednim indexem) se vztahuje k jedne barve.  pole[0][][] je
   rezervovano na sumarni informaci o vsech barvach, nebo (v pripade
   jen dvoubarevne ulohy) se pracuje pouze s touto nultou vrstvou. 
   V takovem pripade pole[0][i][j] obsahuje znak odpovidajici
   policku lustovky na realnem radku "i" a sloupci "j".
   Existuji tri moznosti, co muze v prvku pole[0][i][j] byt:
   '?' .. policko zatim nezname, '*' .. policko cerne (nebo v barve bloku), 
   ' ' .. policko v barve pozadi. Krome toho pri pause-modu jsou v kazdem
   kroku zanaseny nove objevene udaje pomoci '#' .. policko v barve bloku
   a '-' .. policko v barve pozadi.

   U barevne lustovky obsahuje pole[b][i][j] pro b > 0 otaznik, pokud nevime, 
   zda na tomto policku barva b bude nebo ne. pole[b][i][j]==' ' znamena, ze
   tam barva b urcite nebude a pole[b][i][j]=='*' znaci, ze tam barva b
   urcite bude. Pro rychlejsi dotazovani do pole udrzuji algoritmy 
   v konzistentnim stavu sumarni informaci v prvcich pole[0][i][j]:
   '?' .. aspon v jednom pole[b][i][j] je otaznik pro b>0
   ' ' .. vsechna pole[b][i][j] pro b>0 maji mezeru, tj. jedna se
   urcite o barvu pozadi. '*' .. existuje prave jedno b, kde je
   pole[b][i][j] hvezdicka a pro vsechna ostatni b je pole[b][i][j]
   mezera. Tj. zname definitivne barvu tohoto policka.

   Protoze pole alokujeme az za behu programu podle aktualnich hodnot 
   "pocetradku",  "pocetsloupcu", "pocetbarev" pomoci malloc(),
   muzeme pracovat jen s linearnim polem, tj. pole[]. Pristup do
   vrstev a radku resime programove a mame na to pripraveny zkratky:
   BIJ ... pristup do b-te vrstvy, i-teho radku a j-teho sloupce,
   IJ  ... pristup do nulte vrstvy, i-teho radku a j-teho sloupce.

   Protoze algoritmy nerozlisuji mezi radky a sloupci lustovky
   (pracuji se zobecnenymi radky a s globalnim ii, viz [vstupnidata]).
   Je tedy potreba pripravit funkci rpole(), ktera precte policko pole ze
   zobecneneho radku ii z pozice i a z vrstvy b. Dale potrebujeme 
   funkci spole(), ktera do prvku pole odpovidajici zobecnenemu radku
   ii a pozici i v barevne vrstve b ulozi novy znak c.  

   Pripadem if(triddlers) se zatim pri prvnim cteni nebudeme zabyvat.
   Zamerime se na pravouhle lustovky. K triddlerum se podrobne
   vratime pozdeji.
   */

#define BIJ b*vrstva + i*pocetsloupcu + j
#define IJ  i*pocetsloupcu + j

char *pole;
long int vrstva,    /* velikost vrstvy pole, vrstva = pocetradku*pocetsloupcu */
         total;     /* celkova delka pole */

char rpole (int i, int b)   
{
  if (triddlers) return pole [b*vrstva + tri(i)*maxj + trj(i)];
  if (ii < pocetradku) return pole [b*vrstva + ii*pocetsloupcu + i];
  else      return pole [b*vrstva + i*pocetsloupcu + ii-pocetradku];
}

void spole (int i, int b, char c)  
{
  if (triddlers) pole [b*vrstva + tri(i)*maxj + trj(i)] = c;
  else {
    if (ii < pocetradku) pole [b*vrstva + ii*pocetsloupcu + i] = c;
    else                 pole [b*vrstva + i*pocetsloupcu + ii-pocetradku] = c;
  }
}

/* [cosradkem] O kazdem zobecnenem radku si drzime nasledujici informace:
   cosradkem[ii] = 1 ... ma smysl se radkem ii zabyvat, neni vhodne jej
                         pri prochazeni radku preskakovat.
   cosradkem[ii] = 0 ... nema smysl radek ii zkoumat rychlym algoritmem, zadne
                         nove informace z nej timto algoritmem neziskame.
   dukladne[ii] ... nenulove, pokud ma smysl se radkem zabyvat 
   v intenzivnim algoritmu. V takovem pripade ukladame v dukladne[]
   slozitost radku, viz [projdidukladne] 

   vyreseno[ii] ... pocet vyresenych policek v radku ii.

   psvradku[ii] ... pocet stupnu volnosti radku. Kdyz umistime
   vsechny bloky v radku co nejvice vlevo, pak stupen volnosti udava,
   o kolik pozic je mozno posledni blok posunout, nez narazi
   na konec radku.
   */

char cosradkem[MAXRADKU+MAXSLOUPCU];  
int  psvradku[MAXRADKU+MAXSLOUPCU];    
int  vyreseno[MAXRADKU+MAXSLOUPCU];    
int  dukladne [MAXRADKU+MAXSLOUPCU];

/* [pu] Pole pu[] budeme pouzivat na vypocet pruniku nekonfliktnich
   rozlozeni bloku. Jakmile najdeme dalsi nekonfliktni rozlozeni bloku,
   pricteme do pu[] jednicku vsude tam, kde mame cernou barvu.
   Take pricteme jednicku do sumpu. Na konci je prunik cernych barev
   vsude tam, kde pu[i] == sumpu a prunik bilych barev vsude tam,
   kde pu[i] == 0.

   V pripade ruznych barev musime pridat poli pu[] jeste jeden rozmer 
   pro barvy. Nulta vrstva zde ale znamena barvu s indexem 1, 
   abychom neplytvali mistem v pameti.

   Pole pu[][] alokujeme dynamicky, ale ponechame mu dvourozmerny charakter, 
   viz [inipole].
   */

long int * pu[MAXBAREV-1]; 
long int sumpu;         

/* [inipole] Funkce inipole() je volana z funkce main a inicializuje
   data pro lustovku pred zahajenim reseni, nebo pred vlozenim dalsich
   udaju pomoci parametru -ini 

   U cernobile lustovky ma smysl se na zacatku nezabyvat radky,
   ktere maji stupen volnosti vetsi nez je maximalni delka bloku.
   O to se prave snazime v zaveru funkce inipole.
   */

int tr0, tr1;   /* konstanty, ktere pocita trset(), viz [triddlers] */

void inipole () 
{
  long int i;
  int j, sumb, maxb, maxll;
  
  vrstva = pocetradku*pocetsloupcu;
  maxll = MAX(pocetradku,pocetsloupcu);
  if (triddlers) {
    trset(pocetradku-1);  maxj = tr1;
    if (maxj > MAXRADKU) error ("limit MAXRADKU for triddlers exeeded");
    vrstva = pocetradku * maxj;
    maxll = tridmaxll ();
    if (maxll > MAX(MAXRADKU,MAXSLOUPCU)) 
      error ("limit MAXRADKU,MAXSLOUPCU for triddlers exeeded");
  }
  total = vrstva*pocetbarev;
  if (pocetbarev==2) total = vrstva;
  pole = getmemory (total, "pole");
  for (i=0; i<total; i++) pole[i] = '?';

  pu[0] = getmemory (maxll*sizeof(long int), "pu[0]");
  if (!dvebarvy) for (i=1; i<pocetbarev-1; i++) 
    pu[i] = getmemory (maxll*sizeof(long int), "pu");

  ll = pocetsloupcu;
  for (ii=0; ii<pocetradku+pocetsloupcu; ii++) {
    if (ii==pocetradku) ll = pocetradku;
    if (triddlers) trset(ii);
    sumb = 0; maxb = 0;
    for (j=0; j<pb[ii]; j++) {
      if (vb[ii][j] > maxb) maxb = vb[ii][j];
      sumb += vb[ii][j];
      if (vm[ii][j]>0) sumb += vm[ii][j];
    }
    psvradku [ii] = ll - sumb;
    if (pocetbarev > 2 || pb[ii] == 0 || maxb > psvradku[ii]) 
          cosradkem [ii] = 1, vyreseno [ii] = dukladne [ii] = 0;
    else  cosradkem [ii] = 0, vyreseno [ii] = dukladne [ii] = 0; 
  }
}  

/* [ulozprvek] Abychom udrzeli udaje cosradkem[] a vyreseno[]
   konzistentni se stavem rozlustenosti lustovky, bude lepe pri kazdem
   novem vyresenem policku zapisovat do pole[][][] prostrednictvim
   funkce ulozprvek(). Tato funkce ulozi zmenu c na pozici i do vrstvy
   b podobne jako spole, ale navic pozmeni odpovidajici vyreseno[] a
   cosradkem[]. Jde o to, ze pokud jsme zanesli zmenu napr. na j-tou
   pozici v realnem radku k, pak je potreba otevrit realny sloupec j 
   dalsimu reseni, protoze tam muzeme ziskat diky nove informaci dalsi
   nove informace. Nastavime tedy tomuto sloupci cosradkem[] = 1 a
   vynulujeme dukladne[], protoze je sance, ze rychly algoritmus v tom
   sloupci neco najde.

   Globalni promenna "pocetzmen" pocita zmeny v kazdem kroku programu. 
   Podle ni pak zjistime, zda byl krok uspesny (nejake novinky jsme objevili)
   nebo neuspesny ("pocetzmen" se nezmenilo).

   Funkce ulozdopasku() uklada zmenu do pameti track[], viz [ulozdopasku].
   Zavolame ji jen tehdy, kdyz jsme ve zkousecim modu ("zkousime==1").  
   */

int pocetzmen, zkousime;

void ulozprvek (int i, int b, char c)
{
  pocetzmen++;
  if (zkousime) ulozdopasku (i, b);
  if (triddlers) { tridulozprvek (i, b, c); return; }
  if (ii < pocetradku) {
    pole [b*vrstva + ii*pocetsloupcu + i] = c;
    cosradkem[i+pocetradku] = 1; 
    dukladne[i+pocetradku] = 0;
  }
  else  { 
    pole [b*vrstva + i*pocetsloupcu + ii-pocetradku] = c;
    cosradkem[i] = 1; 
    dukladne[i]=0; 
  }
  if (b > 0) return;
  vyreseno [ii]++;
  if (ii < pocetradku) vyreseno[i+pocetradku]++;
  else                 vyreseno[i]++;
}

/* [vyres] Tato funkce provadi opakovane kroky typu r (rychle
   prochazeni radku) a c (rychle prochazeni sloupcu). Pokud narazi na
   sporne zadani, vrati KO. Pokud nelze pomoci rychleho algoritmu
   ziskat dalsi informace, vyvolame zde funkci projdidukladne(), ktera se
   pokusi najit dalsi zmenu za pouziti intenzivniho algoritmu.  Pokud
   byla projdidukladne() uspesna (navratova hodnota je kladna), pak se
   vracime k rychlemu aloritmu a znovu tocime dokola prochazeni radku
   a sloupcu. Pokud neni ani vyvolani funkce projdidukladne() upesne,
   ukoncime vyres() a vratime OK. V miste vyvolani vyres() pak musime
   osetrit, zda OK znamena skutecne vyresenou lustovku nebo situaci,
   kdy algoritmy selhaly, a neni jasne, jak pokracovat. Viz [vsechnareseni].

   Promennou "pocetzmen" vzdy pronuluje funkce dalsikrok(). Pak pri
   reseni jednotlivych radku pomoci funkce vyresradek() muze dojit 
   k jejimu zvetseni na nenulovou hodnotu. Po projiti realnych radku i
   sloupcu se ptame, zda je promenna "pocetzmen" nulova. Pokud ano,
   je potreba ucinit nova opatreni.  Pokud ne, pak resime radky a
   sloupce znovu.

   Globalni promenna "pocetpreskocenych" pocita radky, ktere jsme 
   preskocili, protoze mely pocet bloku vetsi nez limitbloku.
   Existuji-li takove radky a je vyreseno==0, pak zmenime limitbloku
   na maximalni hodnotu a pustime se do reseni znovu. Nyni uz nebudeme
   preskakovat radky kvuli tomu, ze maji moc bloku.  
   */

#define OK 1
#define KO 0

int  pocetpreskocenych; 
char typkroku;
int  pocetzmen2 = 0;   /* v triddlers scitame zmeny predchozich dvou kroku */

int vyres () 
{
  while (1) {
    pocetpreskocenych = 0;
    if (pocetzmen) dalsikrok ();
    typkroku = 'r';                   /* realne radky */
    ll = pocetsloupcu;                             
    for (ii=0; ii<pocetradku; ii++) if (vyresradek () == KO) return KO;
    if (pocetzmen) dalsikrok ();
    typkroku = 'c';                   /* realne sloupce */
    ll = pocetradku;                             
    for (ii=pocetradku; ii<pocetradku+pocetsloupcu; ii++) 
      if (vyresradek () == KO) return KO;
    if (pocetzmen+pocetzmen2 == 0) {
      if (pocetpreskocenych == 0) {
	if (projdidukladne () == 0) return OK;
	else continue;
      }
      if (limitbloku < MAXBLOKU) {
	limitbloku = MAXBLOKU;
	wlog (2, "\n (bl=%d)", limitbloku);
	continue;
      }
      if (projdidukladne () == 0) return OK;
    }
  }
}

/* [globradek] Nase pozornost se nyni zameri na funkci vyresradek().
   Tato funkce pracuje s mnozstvim dalsich globalnich promennych. 
   Venujme se proto nyni jejich vyznamu. 

   Vetsinu promennych bychom nemuseli deklarovat a pouzivat. Napriklad
   misto np by stacilo vsude psat pb[ii]. Kvuli optimalizaci rychlosti
   se ale vyplati casto pouzivane hodnoty mit v primo adresovatelnych
   promennych a nevyzvedavat je z pole.
   */

int np,              /* pocet bloku aktualniho radku, tj. pb[ii] */
    npp,             /* zkratka za np-1 */
    lll;             /* zkratka za ll-1 */

int vbl[MAXBLOKU],   /* velikost bloku lokalne ii-teho radku, tj. vb[ii][] */
    vml[MAXBLOKU],   /* velikost mezery lokalne, tj. vm[ii][] */
    bbl[MAXBLOKU],   /* barva bloku lokalne, tj. bb[ii][] */
    bml[MAXBLOKU];   /* blok musi nasledovat za svym predchudcem bez mezery */

int p[MAXBLOKU],     /* seznam pocatku bloku pri navrhu rozlozeni bloku */
    pr[MAXBLOKU];    /* druha varianta navrhu rozlozeni bloku v radku */

char radek[MAX(MAXRADKU,MAXSLOUPCU)];  /* stav radku, ktery prave resime */

/* [prvekpole] K prvkum pole[][][] budeme pristupovat pomoci funkce
   prvekpole(). Tu ztotoznime s vyse uvedenou funkci rpole(), pokud
   pracujeme s barvami. Pri cernobile verzi je ale casove vyhodnejsi
   si prekopirovat vysetrovany radek do jednorozmerneho pole radek[]
   a ptat se na hodnoty do tohoto pole. Neni pak potreba pocitat adresu
   prvku nasobenim, zatimco pri vyhledani adresy prvku pole[][][]
   nasobit musime.

   V tzv. pravo-levem algoritmu budeme take potrebovat cist radek
   "pozpatku". Pak se vyplati ztotoznit prvekpole() s funkci
   rpolei(), jak za chvili uvidime. Mame tedy dva duvody, proc pouzit
   trik se ztotoznovanim prvekpole() vhodne funkci. 
   */

char (*prvekpole) ();

char primoradek (int i, int b)  /* kvuli optimalizaci dvoubarevne lustovky */
{
  return radek [i];
}

char rpolei (int i, int b)       /* precte hodnotu z pole pozpatku */
{
  if (triddlers) return pole [b*vrstva + tri(lll-i)*maxj + trj(lll-i)];
  if (ii < pocetradku)  return pole [b*vrstva + ii*pocetsloupcu + lll-i];
  else     return pole [b*vrstva + (lll-i)*pocetsloupcu + ii-pocetradku];
}

/* [deklarace2] pro potreby funkce logradek(), ktera tiskne informace
   o rozlozeni bloku, pripravime nasledujici konstanty.
   */

#define START 1
#define END   2
#define ALL   3

/* [vyresradek] Funkce se pokusi najit dalsi reeni v ii-tem radku na zaklade
   zadani (velkosti, barev a poradi bloku na radku) a na zaklade castecneho
   reseni radku (nekleta policka uz mohou byt znama). V dalsim textu
   "nekonfliktni rozlozeni bloku" bude znamenat takove rozlozeni bloku na 
   radku, ktere je v souladu jednak se zadanim a jednak s informacemi 
   o jiz vyresenych polickach na radku.

   Funkce vyreasradek() vrati KO, pokud neexistuje zadne nekonfliktni 
   rozlozeni bloku, jinak vrati OK (nezavisle na tom, zda se funkci podarilo
   objevit novou informaci na radku nebo ne).

   Nejprve ve funkci zkoumame, zda ma smysl se radkem vubec zabyvat 
   (cosradkem[ii]==0). Pokud je radek plne vyresen, pak udelame zaverecnou 
   kontrolu nekonfliktotnosti reseni a potom zavreme dalsi zkoumani tohoto
   radku pomoci cosradkem [ii]=0.

   Vlastni cinnost funkce je zalozena na tzv. pravo-levem algoritmu.
   Vyuziva se k tomu funkce najdiprvni(), ktera najde prvni nekonfliktni
   rozlozeni bloku tak, aby bloky byly pokud mozno co nejvice umisteny vlevo.
   Vysledek teto funkce je predan v globalni promenne p.

   Pokud nastavime nejprve prvekpole() tak, aby vracel udaje o stavajicim 
   reseni radku pozpatku a do promennych vbl[], bbl[], vml[], bml[]
   rovnez ulozime dane bloky v prevracenem poradi, pak nam funkce najdiprvni()
   najde prvni nekonfliktni rozlozeni bloku tak, aby bloky byly umisteny co 
   nejvice vlevo. Tyto udaje si zkopirujeme z promenne p[] do promenne pr[]
   a zavolame najdriprvni() v normalnim rezimu, kdy dostaneme reseni bloku
   umistene co nejvic vlevo. Nyni staci udelat pruniky bloku se stejnym 
   indexem a to jsou policke, o kterych vime, ze budou cerna (resp. budou
   mit konkretni barvu). Take udelame pruniky mezer se stejnym indexem a 
   mame policka, ktera budou urcite mit barvu pozadi. 

   Tyto informace ulozime do pole pu[][] tak, ze 0 znamena barvu pozadi 
   (prunik mezer), 2 znamena barvu bloku a 1 znamena nadale nejasne
   policko. Funkce ulozreseni() pak cte pole pu[][] a vlozi nove inforamce 
   o polickach pomoci ulozprvek() do pole[][][].

   V zaveru funkce pomoci hodnoty "pocetzmen" pred zavolanim ulozresebni()
   a po zavolani ulozreseni() zjistime, zda byl pravo-levy algoritmus
   uspesny, tj. nasel novou informaci. Pokud byl uspesny, pricteme
   do sumare uspesnych pruchodu jednicku.

   Dale nastavime dukladne [ii] = 1, cosradkem [ii] = 0, tj. 
   doporucime radek k reseni intenzivnimu algoritmu a zneprstupnime 
   radek pravo-levemu algoritmu, ktery se k nemu (dokud nenastane 
   v radku dalsi zmena) nevraci.
   */

int A=0, B=0, C=0, D=0, E=0, F=0;  /* strany sestiuhelnika pro triddlers */

int vyresradek () 
{
  int b, k, ni;
  register int i, j;
  
  if (triddlers && ii == pocetradku+C+D) {
    pocetzmen2 = pocetzmen;
    if (pocetzmen) dalsikrok ();
    typkroku = 'e';                   /* strany e, f sestiuhelniku */
  }
  if (cosradkem[ii]==0) return OK;
  if (triddlers) trset(ii);
  if (vyreseno[ii] == ll) {       /* zaverecna kontrola konzistence */
    np = pb[ii]; npp = np-1;
    if (np==0) return OK;
    for (j=0; j<np; j++) { 
      vbl[j] = vb[ii][j]; vml[j] = vm[ii][j]; bbl[j] = bb[ii][j];
      bml[j] = 0;
      if (j>0 && vml[j-1] < 0) vml[j-1] = 0, bml[j] = 1;
    }
    for (i=0; i<ll; i++) radek[i] = rpole (i, 0);
    if (najdiprvni () == KO) return KO; 
    cosradkem [ii] = 0;  dukladne [ii] = 0;
    return OK;
  } 
  if (pb[ii] > limitbloku)  {  /* zatim tento radek/sloupec preskocime */
    pocetpreskocenych++;       /* vratime se k nemu pozdeji kvuli */
    return OK;                 /* optimalizaci rychlosti */
  }
  sumrr++;
  for (i=0; i<ll; i++) for (j=0; j<pocetbarev-1; j++) pu[j][i] = 1;
  sumpu = 0; np=pb[ii]; npp=np-1; 
  if (np) {
    for (j=0; j<np; j++) {
      vbl[j] = vb[ii][npp-j]; bbl[j] = bb[ii][npp-j];
      if (j<npp) vml[j] = vm[ii][npp-j-1];
      else       vml[j] = 0;
      bml[j] = 0;
      if (j>0 && vml[j-1] < 0) vml[j-1] = 0, bml[j] = 1;
    }
    if (dvebarvy) prvekpole = primoradek;
    else prvekpole = rpolei;
    lll = ll-1;
    for (i=0; i<ll; i++) radek[i] = rpolei (i, 0);
    if (najdiprvni () == KO) return KO; 
    for (j=0; j<np; j++) { 
      vbl[j] = vb[ii][j]; vml[j] = vm[ii][j]; bbl[j] = bb[ii][j];
      bml[j] = 0;
      if (j>0 && vml[j-1] < 0) vml[j-1] = 0, bml[j] = 1;
      pr[j] = ll - p[npp-j] - vbl[j];
    }
    if (dvebarvy) prvekpole = primoradek;
    else prvekpole = rpole;
    for (i=0; i<ll; i++) radek[i] = rpole (i, 0);
    najdiprvni ();
    for (j=0; j<np; j++) for (i=pr[j]; i<p[j]+vbl[j]; i++) {
      if (dvebarvy) pu[0][i] = 2;
      else pu[bbl[j]-1][i] = 2;
    }
    for (i=0; i<p[0]; i++) pu[0][i] = 0;
    for (i=pr[npp]+vbl[npp]; i<ll; i++) pu[0][i] = 0;
    if (dvebarvy)
      for (j=0; j<npp; j++) for (i=pr[j]+vbl[j]; i<p[j+1]; i++)  pu[0][i] = 0; 
    else for (b=1; b<pocetbarev; b++) {
      i = 0; j = 0;
      while (i<ll) {
	while (j<np) {
	  if (bbl[j] == b) break;
	  j++;
	}
	if (j==np) ni = ll;
	else       ni = p[j];
	while (i<ni) pu[b-1][i++] = 0;
	if (j<np) i = pr[j] + vbl[j];
	j++;
      }
    }
    sumpu = 2;
    if (llevel>=3) logradek (ALL);
  }
  k = pocetzmen;
  ulozreseni ();
  if (k<pocetzmen)  sumur++;
  cosradkem [ii] = 0;
  if (vyreseno[ii] == ll)  dukladne [ii] = 0;
  else                     dukladne [ii] = 1;
  return OK;
}

/* [najdiprvni] Funkce najdiprvni() najde prvni nekonfliktni rozlozeni vsech 
   bloku v jednom radku umistene co nejvice vlevo. Pokud zadne takove rozlozeni
   neexistuje, vraci KO. Jinak vraci OK a v p[] mame pocatky jednotlivych bloku.

   Na zacatku nastavime ukazatel "i" do radku na nulu (prvni policko radku)
   a pocatek nulteho bloku p[0] zkusime nastavit na toto policko.

   Uvnitr cyklu "while (1)" resime umisteni j-teho bloku s tim, ze na zacatku 
   jej umistime na pozici i. Nejprve prekontrolujeme, zda blok neprecniva doprava 
   pres radek, to bychom museli vratit KO. Dale prekontrolujeme, zda nejsou
   v bloku mezery. Pokud ano, pak posuneme blok doprava az za posledni mezeru
   a pres navesti next: zkousime znova. Pri tomto posunu se ale muze stat,
   ze jsme odkryli hvezdicku, ktera byla puvodne kryta blokem. V takovem pripade
   musime predchozim blokem j-1 tuto hvezdicku zakryt. To delame pomoci 
   funkce posunblok(), ktera se vraci postupne k blokum vlevo od j-teho bloku 
   a snazi se zakryt hvezdicky. Vrati pak cislo bloku, ktery je nejvice v pravo
   a mame o nem jistotu, ze je usazen nekonfliktne. Index "j" se nam tedy muze 
   uvnitr cyklu "while (1)" zmensit.

   "opj" je pozice j-teho bloku pred tim, nez zacneme zkoumat, zda tesne za blokem
   nejsou hvezdicky. Pokud ano a pritom tam musi byt mezera, musime blok posunout,
   aby zakryl vsechny souvisle nasledujici hvezdicky vpravo. Pri teto cinnosti
   se zase muze stat, ze blok odkryje hvezdicky, na kterych uz byl. V takovem
   pripade zovu musime volat posunblok(j-1), tj. smykat s blokem vlevo od
   vysetrovaneho bloku.

   Kvuli volani funkce posunblok (j-1) se mohlo stat, ze se vyskytl pozadavek 
   posunout s blokem vlevo od nulteho bloku. Tam ale uz zadny blok neni, takze 
   musime vratit KO. 

   Na konci cyklu "while (1)" prekontrolujeme, zda nahodou uz nemame usazen
   posledni blok. V takovem pripade prekontrolujeme mezeru vpravo od tohoto
   bloku az do konce radku. Ta totiz nesmi obsahovat hvezdicky. Pokud obsahuje,
   musime zavolat posunblok(j) tak, abychom ty hvezdicky zakryli.
   Na uplnem konci "while (1): zvetsime index bloku a racime se na zacatek cyklu 
   s tim, ze budeme usazovat na nekonfliktni misto dalsi blok. Pokud jsme
   na konci cyklu usadili posledni blok, vracime OK, jsme uspesni a mame
   nalezeno nekonfliktni rozlozeni bloku.

   "oopj" je pozice bloku na zacatku cyklu "while (1)". Pokud jsme s blokem pohli
   a soucasne se jedna o blok, ktery musi byt prilepeny ke svemu levemu sousedovi
   (to se vyskytuje u lustovek s trojuhelniky podle obcasniku "Malovane krizovky"
   z Bratislavy), pak k nemu privolame jeho leveho souseda pomoci posunblok().
   */
   
int najdiprvni ()   /* najde prvni nekonfliktni rozmisteni bloku zleva */
{
  int b, opj, oopj;
  register int i, j, k;

  i=0; j=0; 
  while (1)  {   /* najdu pozici j-teho bloku */
    if (dvebarvy) b = 0;
    else          b = bbl[j];
    oopj = i;
  next:
    p[j] = i;
    if ((i = p[j] + vbl[j]) > ll) return KO; 
    while (--i >= p[j]) if (prvekpole (i, b) == ' ') { 
      for (k=i-dvebarvy; k >= p[j]; k--) if (radek [k] == '*') {
	j = posunblok (j-1, k);
	goto prev;
      }
      i++; goto next; 
    }
    opj = p[j]; i = opj + vbl[j];
    if (vml[j]) while (i < ll && prvekpole(i++, b) == '*') p[j]++;
    if (bml[j] && p[j] > oopj) {
      j = posunblok(j-1, p[j]-1);
      goto prev;
    }
    for (i=p[j]-1; i>=opj; i--)    /* kontrola obnazene mezery */
      if (radek[i] == '*') {
        j = posunblok(j-1, i);
	break;
      }
  prev:
    if (j < 0) return KO; 
    i = p[j] + vbl[j] + vml[j];
    if (j==npp) { /* kontrola mezery vpravo do konce radku */
      for (k=ll-1; k>=i; k--) 
	if (radek[k] == '*') {	  
	  j = posunblok (j, k);
	  break;
	}
      if (j<0) return KO;
      if (j<npp) i = p[j] + vbl[j] + vml[j];
      else return OK;
    }
    j++;
  }
}

/* [posunblok] Funkce se pokusi posunout j-ty blok na pozici i.
   Predpoklada pritom, ze j-ty blok uz byl nekam usazen.
   Funkce nejprve prekontroluje, zda pozadovane usazeni bloku nezakryva 
   nejakou mezeru a pokud ano, pokusi se blok usadit vpravo od mezery.

   Funkce dale prekontroluje, zda po posunuti se neodkryla nejaka hvezdicka.
   Pokud ano, zavola funkce posunblok() rekurzivne sebe sama na blok vlevo od 
   posunovaneho bloku tak, aby tento zakryl problemovou hvezdicku.

   Pokud funkce rekursivne sve pozadavky hromadi az do stavu, kdy se ma posunovat
   blok vlevo od nulteho bloku (tam ale uz zadny neni), funkce vrati -1.
   To je priznak celkoveho konfliktu v radku.
   Jinak funkce vrati cislo bloku, ktery je urcite usazen nekonfliktne.
   Toto cislo diky rekurzivnimu volani muze byt nakonec mensi, nez puvodni 
   cislo "j".

   Funkce posunblok() a najdiprvni() jsme se snazili optimalizovat na rychlost.
   Proto zde vidite konstrukce "goto". Verime, ze jsme tim nikoho moc neurazili.
   Pravdepodobne se da funkce najdiprvni() implementovat jeste jinak a efektivneji.
   Mame urcitou predstavu, ale zatim jsme to nezkouseli...
   */

int posunblok (int j, int i) /* posune j-ty blok, aby zakryl pozici i */  
{
  int opj, oopj, b;

  if (j < 0) return -1;
  opj = p[j];
  if (dvebarvy) b = 0;
  else          b = bbl[j];
  i = i-vbl[j];
  oopj = i+1;
next:
  p[j] = i+1;
  if ((i = p[j] + vbl[j]) > ll) return -1;
  while (--i >= p[j]) if (prvekpole (i, b) == ' ') goto next;
  i = p[j] + vbl[j];
  if (vml[j]) while (i < ll && prvekpole(i++, b) == '*') p[j]++;
  if (bml[j] && p[j] > oopj) return posunblok(j-1, p[j]-1);
  for (i=p[j]-1; i>=opj; i--)    /* kontrola obnazene mezery */
    if (radek[i] == '*') return posunblok(j-1, i);
  return j;  
}

/* [ulozreseni] Tato funkce na zaklade udaju v poli pu[][] ulozi
   pripadne nova reseni prave prozkoumaneho radku do pole[].  Funkce
   vola ulozprvek(), ovsem jen tehdy, pokud puvodni stav pole[]
   obsahuje otaznik. Nedela tedy zmeny, ktere uz jsou udelany.

   Je-li pocet bloku v radku nula, pak se jedna o vyjimku, kdy zaneseme
   do celeho radku jen barvu pozadi.

   Konstanty "cerna" a "bila" oznacuji symboly pro barvu bloku a barvu
   pozadi, ktere pouzivame ve vrstvach pole[][][].  Jejich implicitni
   hodnoty '*' a ' ' zmenime na '#' a '-', pokud trasujeme vypocet a
   chceme vytisknout odlisne nove informace po kazdem kroku (napr. 
   v rezimu -p). Podrobneji o vyuziti "cerna", "bila" viz [dalsikrok] */

char cerna = '*', bila = ' ';

void ulozreseni ()  
{
  int i, j, k;

  if (np) {
    if (dvebarvy) for (i=0; i<ll; i++) {
      if (pu [0][i] == sumpu && rpole (i,0) == '?') ulozprvek (i, 0, cerna);
      if (pu [0][i] == 0 && rpole (i,0) == '?')     ulozprvek (i, 0, bila);
    }
    else for (i=0; i<ll; i++) { 
      for (j=1; j<pocetbarev; j++){
	if (pu [j-1][i] == sumpu && rpole (i,j) == '?') { 
	  ulozprvek (i, 0, cerna);
	  ulozprvek (i, j, cerna);
	}
	if (pu [j-1][i] == 0 && rpole (i,j) == '?')  ulozprvek (i, j, bila);
      }
      if (rpole(i, 0) == '?') {
	for(k=1; k<pocetbarev; k++)
	  if(rpole(i, k) != ' ' && rpole(i, k) != '-') break;
	if(k==pocetbarev) ulozprvek (i, 0, bila);
      }
    }
  } 
  else for (i=0; i<ll; i++) if (rpole(i,0) == '?') {
    if (dvebarvy) ulozprvek (i, 0, bila);
    else for(k=0; k<pocetbarev  ; k++) ulozprvek (i, k, bila);
  }
}

/* [dalsikrok] Funkce dalsikrok() je volana jedine z funkce vyres()
   a navic pouze v pripade, ze je "pocetzmen" nenulovy.
   Funkce dalsikrok() pronuluje "pocetzmen" a zapise na terminal
   cislo a typ prave ukonceneho kroku.

   V pripade "-p" se funkce muze zastavit a cekat pomci "getc()"
   na stisk Enter. V pripade "-stop" (kladna hodnota "steps")
   funkce prekontroluje, zda uz neni prekrocen limit kroku a kdyz je,
   ukonci nemilosrdne program.

   Aby byly videt zmeny z posledniho kroku, jsou pri "-p" nebo '-stop"
   nastaveny hodnoty cerna = '#', bila = '-'. V takovem pripade musime
   po vytisteni mezivysledku promenit vsechny vyskyty '#' na '*' a 
   '-' na ' ' v promenne pole[]. Algoritmy typu najdiprvni() se pak 
   nemuseji zdrzovat podminkou typu 
   "if (prvekpole(i,b)=='*' || prvekpole(i,b)== '#')"
   protoze to by je hodne zdrzovalo. Naopak, uprava pole[]
   po kazdem kroku pri -p neni problem, protoze pri -p nikam nespechame.
   */

long int pruchod = 1;   /* cislo kroku */

void dalsikrok ()
{
  long int i;
  int ch;

  switch (typkroku) {
  case 'r': sumkr++; 
            wlog (3, "\n^^^ End of step %ld: ROWS\n", pruchod);      
	    break;
  case 'c': sumkc++; 
            wlog (3, "\n^^^ End of step %ld: COLUMNS", pruchod);
	    if (triddlers) wlog (3, " (C,D)", 0);
	    wlog (3, "\n", 0);
	    break;
  case 'e': sumkc++; 
            wlog (3, "\n^^^ End of step %ld: COLUMNS (E,F)\n", pruchod);
	    break;
  case 'i': sumki++; 
            wlog (3, "\n^^^ End of step %ld: INTENSIVE\n", pruchod); 
	    break;
  case 't': sumkt++; 
            wlog (3, "\n^^^ End of step %ld: TEST\n", pruchod);
	    break;
  }
  if (llevel==2) { 
    printf (" %ld%c", pruchod, typkroku);
    fflush (stdout);
  }
  if (paused && paused<=pruchod) { 
    printpole (1); 
    for (i=0; i<total; i++) {
      if (pole[i] == '#') pole[i] = '*';
      if (pole[i] == '-') pole[i] = ' ';
    }
    printf ("== Enter: next step, ^C: stop, <num>+Enter: pause after <num> steps ==> "); 
    ch = getc (stdin);
    if (ch >= '0' && ch <= '9') {
      i = ch - '0'; 
      ch = getc (stdin);
      while (ch >= '0' && ch <= '9') { i = 10*i + ch - '0'; ch = getc (stdin); }
      if (i==0) paused = 0, cerna = '*', bila = ' ';
      if (i>=2) paused = pruchod + i, cerna = '*', bila = ' ';
    }
    while (ch != '\n') ch = getc (stdin) ;  /* vyprazdnime buffer */
  }  
  pocetzmen = 0;
  pruchod++; 
  if (pruchod == paused || pruchod == steps) cerna = '#', bila = '-';
  if (steps && pruchod > steps) {
    if (!paused) printpole (1); 
    exit (0);
  }
}

/* [projdiukladne] Pokud selze rychly pravo-levy algoritmus, pustime se do
   dukladneho prochazeni radku. Funkce projdidukladne() vraci 1, pokud
   se povedlo dukladnym algoritmem najit aspon jednu zmenu. Jinak vraci 0.
   
   Protoze je dukladny algoritmus (viz [vyresdukladneradek]) pomerne
   narocny na cas, vyplati se seradit zobecnene radky podle slozitosti
   a zkouset pouzit vyresdukladneradek() nejprve na mene slozite
   radky. Seradime tedy zobecnene radky podle slozitosti a pak
   postupne volame podle tohoto seznamu funkci vyresdukladneradek().
   Jakmile se povede najit prvni zmenu, koncime s prochazenim tohoto
   seznamu (trebaze jsme ho neprosli jeste cely) a vracime 1.

   Otazkou zustava, jak pozname slozitost radku. V tomto bode by se dalo
   vymyslet urcite neco lepsiho a vypocet tak jeste zrychlit. Zatim mame
   implementovan velmi jedoduchy vypocet slozitosti, ktery vychazi 
   z nasledujici uvahy.

   Pokud bychom meli vysetrit vsechny polohy bloku na radku bez zavislosti
   na tom, co uz mame v radku vyreseno (to intenzivni algoritmus v zasade 
   dela), pak existuje s*(s-1)*(s-2)*...*(s-n+1) moznosti rozlozeni bloku,
   kde s je pocet stupnu volnosti v radku (viz "psvradku[]") a n je pocet 
   bloku. Slozitost by pak mohla byt rovna tomuto cislu. Bohuzel, je velmi 
   pravdepodobne, ze toto cislo nam prekroci omezeni long long intu a 
   podobnych promennych. Prechazet na vypocet slozitosti pomoci float se 
   nam nechtelo. Pokud bychom vzorec zlogaritmovali, pak mame
   ln s + ln (s-1) + ... + ln (s-n+1), coz uz urcite neprekroci zadne meze.
   Bohuzel, logaritmovani je pomerne narocna operace na cas, takze pri privreni
   obou oci misto logaritmu piseme identitu a dostavame vzorec pro vypocet
   slozitosti: s + s-1 + s-2 + ... + s-n+1 = (2*(n-1)*s - n*n + n)/2.
   Dvojnasobek tohoto vzorce (ponekud zjedoduseneho) zvetseny o dvojnasobek 
   nevyresenych policek jsme vzali jako zaklad pro vypocet slozitosti.  

   Je-li v radku jen jediny souvisly blok otazniku, pak plati,
   ze pokud pravolevy algoritmus na nic neprisel, dukladny algoritmus 
   take neobjevi nic noveho. Proto v takovem pripade vracime
   dukladne[ii] = 0 a radkem se nezabyvame.
   */

int projdidukladne () 
{
  int i, j, slozitost, pocetdukladnych;
  int seznamradku[MAXRADKU+MAXSLOUPCU];   /* seznam radku podle slozitosti */

  pocetdukladnych = 0;
  for (ii=0; ii<pocetradku+pocetsloupcu; ii++)   /* seradit podle slozitosti */
    if (dukladne[ii]) {
      if (ii<pocetradku) ll = pocetsloupcu;
      else               ll = pocetradku;
      if (triddlers) trset(ii);
      i = 0;                               /* prozkoumame souvislost otazniku */
      while (i<ll) if (rpole(i++,0)=='?') break;
      while (i<ll) if (rpole(i++,0)!='?') break;
      for (; i<ll; i++) if (rpole(i,0)=='?') break;
      if (i==ll) { dukladne[ii] = 0; continue; } /* jediny blok otazniku */
      slozitost = 
	 4*(ll-vyreseno[ii]) +  2*pb[ii]*psvradku[ii] - pb[ii]*pb[ii];
      i = 0;
      while (i < pocetdukladnych) {
	if (dukladne [seznamradku[i]] > slozitost) break;
	i++;
      }
      for (j=pocetdukladnych; j>i; j--)  seznamradku [j] = seznamradku [j-1];
      seznamradku [i] = ii;
      dukladne [ii] = slozitost; 
      pocetdukladnych++;
    }
  if (pocetdukladnych) { wlog (2, " !", 0); typkroku='i'; }
  for (i=0; i<pocetdukladnych; i++) {
    ii = seznamradku [i];
    if (ii<pocetradku) ll = pocetsloupcu; 
    else               ll = pocetradku;
    if (triddlers) trset(ii);
    cosradkem [ii] = 1; 
    vyresdukladneradek ();
    dukladne [ii] = 0;
    if (pocetzmen) { 
      sumri += i+1; sumui++;
      if (llevel==2) wlog (2, "(%d)Y", i+1); 
      return 1; 
    }
  }
  sumri += pocetdukladnych;
  if (pocetdukladnych && llevel==2) wlog (2, "(%d)N", pocetdukladnych);
  return 0;
}

/* [vyresdukladneradek] Nejprve pronulujeme pu[][] (pole pruniku)
   a pripravime si hodnoty vysetrovaneho radku do vbl[], vml[], atd.
   Pak zavolame uz znamou funkci najdiprvni(), ktera vytvori vychozi stav
   pozic bloku nekonflitkni se stavajicim resenim a co nejvice nalevo.

   Nemusime se bat konfliktu se stavajicim resenim, protoze radek byl
   uz prekontrolovan pravo-levym algoritmem, ktery v radku nenasel zadne 
   nove resene policko.
   
   Do ppj si ulozime polohu posledniho bloku, pokud je (bez ohledu na
   stavajici reseni) umisten uplne vpravo. Tuto konstantu budeme 
   v nasledujicim kroku casto vyuzivat, proto ji zde pocitame jen jednou.

   Pomoci funkce najdidalsi() najdeme vsechny nekonfliktni polohy
   bloku (vcetne uz pripravene prvni polohy). Funkce ulozi o techto
   nekonfliktnich polohach informace do pole pruniku pu[][].
   
   Nakonec pomoci funkce ulozreseni() ulozime vysledek nasi cinnosti 
   do pole[][][]. Funkce projdidukladne() pak podle hodnoty
   "pocetzmen" pozna, zda jsme byli uspesni ci nikoliv.

   Funkce najdidalsi muze najit vice nekonfliktnich poloh bloku, nez je 
   cislo LIMITPOKUSU. To by teoreticky mohlo vest k prekroceni long intu.
   Proto funkce v takovem pripade zaveli k ustupu a dalsi pokusy nehleda.
   My pak pomoci testu "sumpu >= LIMITPOKUSU" zjisitime, zda doslo k teto 
   situaci a pokud ano, velime rovnez k ustupu bez zaneseni reseni.
   */

int ppj;

void vyresdukladneradek () 
{                  
  int i, j;

  for (i=0; i<ll; i++) for (j=0; j<pocetbarev-1; j++) pu[j][i] = 0;
  sumpu = 0; np=pb[ii]; npp=np-1;
  for (j=0; j<np; j++) { 
    vbl[j] = vb[ii][j]; vml[j] = vm[ii][j]; bbl[j] = bb[ii][j];
    bml[j] = 0;
    if (j>0 && vml[j-1] < 0) vml[j-1] = 0, bml[j] = 1;
  }
  for (i=0; i<ll; i++) radek[i] = rpole (i, 0);
  if (llevel>=3) logradek (START);
  najdiprvni ();
  ppj = ll - vbl[npp];
  najdidalsi (0, ll, np); 
  if (sumpu >= LIMITPOKUSU) return;
  if (llevel>=3) logradek (END);
  ulozreseni ();
}

/* [najdidalsi] je funkce, ktera rekurzivne vola sebe sama.
   Parametry: "od" ... cislo bloku, od ktereho delame zmeny smerem doprava,
   "starti" ... doporuceni pro funkci testujradek, odkud staci zkoumat 
   nekonfliktonst radku. "startb" ... doporuceni pro funkci testujradek, 
   od ktereho bloku staci zkoumat nekonfliktnost radku.

   Funkce postupne navrhne vsechna mozna rozlozeni bloku pocinaje vychozim 
   rozlozenim ulozenym v p[] tak, ze postupne s bloky pohybuje smerem 
   doprava a pro kazde rozlozeni vyvola testujradek(), ktery zjisti, zda je
   navrh konfliktni vzhledem ke stavajicimu reseni nebo nikoli.

   Princip rekurze: Bloky vlevo od bloku "od" necham beze zmeny.
   1. Zavolam sam sebe, ovsem parametrem "od" o jedicku vetsim.
   2. V cyklu "while (1)" posunu s blokem "od" o jednicku doprava
      (to si obcas vynuti posunuti dalsich bloku v pravo) a pak
      zavolam sam sebe, ovsem s parametrem "od" o jednicku vetsim.
      Cyklus "while (1)" opakuji tak dlouho, dokud nenastane konflikt 
      se stavajicim resenim v mezere vlevo od obloku "od".

   Pokud je "od" poslednim blokem, pak vykonavam vlastni praci:
   1. Zavolam testujradek() na stavajici rozlozeni bloku.
   2. V cyklu "while (1)" posunu s posledni blok o jednicku doprava
      a znovu zavolam testujradek(). Cyklus opakuji tak dlouho,
      dokud nenastane konflikt s mezerou vlevo od posledniho bloku
      nebo dokud neprekrocim poslednim blokem pravou hranici radku
      (viz "if(p[od] > ppj)").
      
   Funkce testujradek() vrati cislo bloku nebo cislo mezery, ve kterem 
   odhalil konflikt se stavajicim resenim. Cislo mezery je odvozeno 
   od cisla bloku vlevo od mezery. Tj. testujradek muze teoreticky
   vratit hodnotu -1, pokud je konflikt v mezere vlevo od nulteho bloku.
   My ve funkci najdidalsi() tuto informaci pomoci promenne konflikt
   kopirujeme do navratove hodnoty. Pri navratu musime dat do puvodniho stavu
   vsechny polohy bloku, se kterymi jsme nejak pohybovali. Proto
   pracujeme s lokalnim polem uloz[].

   Pokud testujradek nenarazi na konflikt, vraci hodnotu MAXBLOKU
   a navic uklada informaci o rozlozeni bloku do pole pruniku pu[][].
   */

int najdidalsi (int od, int starti, int startb) 
{            
  int uloz[MAXBLOKU], konflikt;
  register int i;

  while (od < npp && bml[od+1]) od++;
  if (sumpu >= LIMITPOKUSU) return 0;
  if (od == npp) {
    if ((konflikt = testujradek (starti, startb)) < od) return konflikt;
    if (bml[od]) return MAXBLOKU;
    i = starti = p[od];
    while (1) {
      p[od]++;
      if (p[od] > ppj) {
	p[od] = i;
	return od-1;
      }
      if ((konflikt = testujradek (starti++, od)) < od) {
	p[od] = i;
	return konflikt;
      }
    }
  }
  else { 
    if ((konflikt = najdidalsi (od+1, starti, startb)) < od) return konflikt;
    for (i=od; i<np; i++) uloz[i] = p[i];
    starti = p[od];
    while (1) {
      for (i=od; i<np; p[i++]++) {
	if (i < npp) 
	  if (p[i] + vbl[i] + vml[i] < p[i+1]) { 
	    p[i]++; break; 
	  }
      }
      if (p[npp] + vbl[npp] > ll) {
	for (i=od; i<np; i++) p[i] = uloz[i];
	return od-1;
      }
      if ((konflikt = najdidalsi (od+1, starti++, od)) < od) {
	for (i=od; i<np; i++) p[i] = uloz[i];
	return konflikt;
      }	
    }
  }
}

/* [testujradek] spolupracuje uzce s vyse uvedenou funkci najdidalsi().
   Testuje nekonfliktnost rozlozeni bloku od j-teho bloku a pritom
   prochazi radek od i-teho policka smerem doprava. Navratove hodnoty 
   viz [najdidalsi].

   "nz" je nasledujici zacatek bloku nebo konec radku. Nejprve zkoumame
   nekonfliktnost mezery a jakmile narazime na policko "nz", nastavime
   d=delka bloku a zkoumame nekonfliktonost bloku. V tomto rezimu pri
   i++ udelame soucasne d--, takze na konci bloku zacneme bez dlouhych
   reci znovu zkoumat nekonfliktnost mezery (protoze je d==0).

   Narazime-li na konflikt, vratime index konfliktniho bloku nebo 
   konfliktni mezery. Nenarazime-li na konflikt, ulozime do pu[][]
   informaci o dalsim nekonfliktnim reseni a vratime MAXBLOKU.

   Obcas se vyplati velet k ustupu. Napriklad pri sumpu>=LIMITPOKUSU.
   Take jednou za 500 uspesnych rozlozeni bloku zkousime, zda jeste existuje 
   neprazdny prunik vsech nekonfliktnich rozlozeni. Pokud ne, pak rovnez
   velime k ustupu jednoduse tak, ze nastavime sumpu=LIMITPOKUSU.

   Algoritmus najdidalsi() ma bohuzel mocninnou slozitost: zhruba
   O(s^n), kde "s" je pocet stupnu volnosti radku a "n" je pocet
   bloku. Pro rozsahle ulohy (radek se stovkami policek a desitkami
   bloku) je tedy tento algoritmus pomerne nepouzitelny. Mirek ma ideu
   algoritmu pouze s polynominalni slozitosti, ale je to tak
   programatorsky komplikovane, ze se mu to zatim nepovedlo
   implementovat. Az to budeme mit, pak se urcite podelime o tento
   vysledek v tomto otevrenem zdrojovem kodu.  
   */

int testujradek (int i, int j)  
{                              
  register int b=0, d=0;       
  int nz;

  if (j<np) nz = p[j];
  else      nz = ll;
  for (; i<ll; i++) {
    if (i==nz) {
      if (!dvebarvy) b = bbl[j];
      d = vbl[j++];
      if (j<np) nz = p[j];
      else      nz = ll;
    }
    if (d) { 
      d--;
      if (prvekpole(i,b) == ' ')  return j-1;
    }
    else
      if (prvekpole(i,0) == '*') return j-1;
  }
  sumpu++;
  if (llevel>=4) printbloky (p, "try");
  if (dvebarvy) { 
    for (j=0; j<np; j++)
      for (i=p[j]; i<p[j]+vbl[j]; i++) pu[0][i]++;
    if ((sumpu % 500) == 0) {     /* zkusime, zda ma smysl pokracovat */
      for (i=0; i<ll; i++) if (pu[0][i] == 0) return MAXBLOKU;
      for (i=0; i<ll; i++) if (pu[0][i] == sumpu) return MAXBLOKU;
      sumpu = LIMITPOKUSU;
    }    
  }
  else
    for (j=0; j<np; j++)
      for (i=p[j]; i<p[j]+vbl[j]; i++) pu[bbl[j]-1][i]++;
  return MAXBLOKU;
}

/* [main] Podivejme se nyni na problem z druhe strany -- z pohledu hlavniho 
   programu. Nejprve pomoci funkce cmdparser() prozkoumame parametry
   prikazove radky. Do pole buf[] si ulozime jmeno ulohy odvozene ze jmena
   vstupniho souboru (ale s odpojenou pripadnou priponou).

   Pomoci funkce nactiproblem() precteme zadani ze vstupniho souboru.
   Pak inicializujeme pole pomoci inipole(). Je-li zadan soubor pomoci
   parametru -ini, precteme jej funkci nactipart(). Pak se pustime do
   vlastniho reseni spustenim funkce vsechnareseni().

   Na zaver vypiseme pocet nalezenych reseni a statistiku poctu kroku
   a poctu vstupu do radkovych algoritmu.
 */

FILE *cmpfile=NULL,   /* soubor zadany pomoci -ini */
     *inifile=NULL,   /* soubor zadany pomoci -cmp */
     *mainfile;       /* hlavni soubor ulohy */
char buf [MAXBUF];    /* nazev ulohy */
char iityp = ' ';     /* typ radku, zejmena rozlisujeme u triddlers */

int main (int argc, char**argv)
{
  int k, inf;

  cmdparser (argc, argv);
  inf = argc-1;
  for (k=0; argv[inf][k]!=0 && argv[inf][k]!='.'; k++) buf[k] = argv[inf][k];
  buf [k] = 0;
  if (buf[0] == '-' && buf[1] == 0) strcpy (buf, "stdin");
  if (paused==1 || steps==1) cerna = '#', bila = '-';
  nactiproblem ();
  inipole ();
  if (inifile != NULL) nactipart (inifile);
  prvekpole = rpole;
  if (dvebarvy) prvekpole = primoradek;
  pocetzmen = 0;
  zkousime = 0;
  wlog (2, "Steps: (bl=%d)", limitbloku);
  vsechnareseni ();
  if (sumr==0) {
    if (llevel==0) return 2;
    printpole (0);
    printf ("\nKO: %s -- the task have NO solution, sorry. ", buf);
    if (triddlers) trset(ii);
    if (ii<pocetradku) printf ("The row");
    else               printf ("The column");
    printf (" %d%c includes conflict, for example\n", cisloradku(ii), iityp);
    printstatistic ();
  }
  else {
    if (llevel==0) return 0;
    printf ("\nOK: %s -- the number of solutions is %ld.\n", buf, sumr);
    printstatistic ();
  }
  return 0;
}

/* [vsechnareseni] Proc v hlavnim programu nespoustime primo funkci 
   vyres()? Muze se stat, ze uloha ma vice reseni, nebo ze 
   v urcitem miste selzou algoritmy zabyvajici se jen izolovane
   radky/sloupci bez sirsiho kontextu. Po aplikaci techto algoritmu
   tedy zustanou v poli reseni otazniky. My se ovsem nevzdavame
   a implementujeme funkci vsechnareseni() a tu vyvolame 
   z hlavniho programu. Funkce vsechnareseni() pak samozrejme vyvola
   funkci vyres().

   Zustanou-li po aplikaci funce vyres() nedoresene otazniky, pak se
   zamerime na jeden z nich (je jedno ktery) a nahradime ho nejprve
   cernou a pak bilou barvou. Po kazdem takovem nahrazeni zavolame
   rekurzivne funkci vsechnareseni(), ktera na svem pocatku vola funkci
   vyres(). Pokud navrhovana cesta nevede k reseni, provedeme return 
   z vnitrne zavolane funkce vsechnareseni() a ve vnejsi funkci dame
   pole reseni do puvodniho stavu. To zaridime pomoci funkce vemzpasku(),
   viz [ulozdopasku] a [vemzpasku]. Pokud ve vnitrni funkci najdeme 
   reseni (pozname to podle navratove hodnoty OK nebo KO funkce vyres()),
   pak reseni vytiskneme.

   Rekurzivni volani zpusobi, ze se podari najit uplne vsechna reseni
   a projit uplne vsechny nedoresene moznosti. Zkuste treba mit v kazdem
   radku/sloupci zadan jen jeden blok delky jedna. Pak program najde
   vsechna reseni rozmisteni sachovych vezi tak, aby se vzajemne
   neohrozovaly. Pro ctvercovou lustovku jich je n!, kde n je pocet
   radku a sloupcu lustovky.

   V barevne verzi musime otaznik nahradit jednak barvou pozadi a
   druhak vsemi barvami, ktere maji moznost v danem policku byt.  
   Kazdou takovou barvou nahradime zvoleny otaznik a rekurzivne volame
   sami sebe 

   Pro potreby navratu k puvodnimu stavu lustovky pracujeme s polem track[],
   ktere alokujeme dynamicky.
   */

unsigned int *track = NULL;    /* pasek pro navratovou informaci */
unsigned int trindex = 0,      /* index do pole track[], viz [ulozdopasku] */ 
             tracktotal,       /* delka track[] po alokovani funkci malloc() */
             trskip;           /* viz [ulozdopasku] */

#define SIZEINT (8*sizeof(unsigned int))
#define TR ((triddlers&&ii>=A)? 2*(ii-A)+1 : 0)  /* korekce pro triddlers */

void vsechnareseni ()
{
  int backindex, b, i, j, k;

  if (vyres ()  == KO) {       /* reseni nenalezeno */
    if (llevel==2) wlog (2, " -", 0);
    if (llevel >2) {
      logradek (START);
      if (llevel >= 3) {
	if (ii<pocetradku) printf (".... CONFLICT: row %d %c", cisloradku(ii), iityp);
	else            printf (".... CONFLICT: column %d %c", cisloradku(ii), iityp);
      }
    }
    return;                     
  }
  ll = pocetsloupcu;            /* hledame nedoreseny radek */
  for (ii=0; ii<pocetradku; ii++) {
    if (triddlers) trset(ii);
    if (vyreseno[ii] != ll) break;
  }
  if (ii==pocetradku) {         /* vsechny radky jsou vyreseny */
    sumr++;                     /* vytiskneme reseni */
    wlog (3, "\n.... SOLUTION number %ld", sumr);
    if (cmpfile != NULL) nactipart (cmpfile);
    printpole (0);
    if (totalnum && sumr >= totalnum) {
      if (llevel>0) {
	printf ("\nMaximal number of solutions -total %d reached. I terminate.\n",
 		totalnum);
	printstatistic ();
      }
      exit (1);
    }
    if (cmpfile != NULL) exit (0);
    if (sumr<=xpmnum && cmpfile == NULL) savexpm ();
    return;
  }
  /* mame v reseni stale nejaky otaznik, poustime se do testu */

  wlog (2, " ?", 0);
  for (i=0; i<ll; i++) if (rpole (i, 0) == '?') break;
  if (i==ll) { 
    printf ("Internal error: something wrong???\n"); 
    printpole(1); 
    for (i=0; i<pocetradku; i++) printf ("%d\n", vyreseno[i]);
    exit (13); 
  }
  backindex = trindex;
  if (track == NULL) {  /* poprve pouzijeme pole track[], musime je alokovat */
    if (dvebarvy) trskip = 0;
    else          trskip = (pocetbarev-1) / SIZEINT + 1; /* viz [ulozdopasku] */
    k = pocetradku*pocetsloupcu;
    if (triddlers) k = pocetradku*maxj;
    for (j=0; j<pocetradku; j++) k -= vyreseno[j];  /* k je pocet otazniku */
    tracktotal = (2+trskip)*k; 
    if (!dvebarvy) tracktotal *= 2; /* mozna vice zapisu do jednoho policka */
    track = getmemory (tracktotal*sizeof(unsigned int), "track");  
    wlog (2, " track<%ld>", tracktotal);
  }
  zkousime = 1;
  if (dvebarvy) {
    if (llevel>=2) printf ("\n ' '->[%d,%d]", ii+1, i+1-TR), fflush (stdout);
    ulozprvek (i, 0, bila);
    cosradkem [ii] = 1; typkroku = 't';
    vsechnareseni ();
    while (trindex>backindex) vemzpasku ();
    if (llevel>=2) printf ("\n '*'->[%d,%d]", ii+1, i+1-TR), fflush (stdout);
    ulozprvek (i, 0, cerna);
    cosradkem [ii] = 1; typkroku = 't';
    vsechnareseni ();
    while (trindex>backindex) vemzpasku ();
  }
  else {
    trskip = (pocetbarev-1) / SIZEINT + 1;
    if (llevel>=2) printf ("\n ' '->[%d,%d]", ii+1, i+1-TR), fflush (stdout);
    ulozprvek (i, 0, bila);
    for (b=1; b<pocetbarev; b++) spole (i, b, ' ');
    cosradkem [ii] = 1; typkroku = 't';
    vsechnareseni ();
    while (trindex>backindex) vemzpasku ();
    for (b=1; b<pocetbarev; b++) if (rpole (i, b) == '?') {
      if (llevel>=2) printf ("\n '%c'->[%d,%d]", barva[b], ii+1, i+1-TR), fflush (stdout);
      ulozprvek (i, 0, cerna);
      ulozprvek (i, b, cerna);
      for (k=1; k<pocetbarev; k++) if (k!=b) spole (i, k, ' ');
      cosradkem [ii] = 1; typkroku = 't';
      vsechnareseni ();
      while (trindex>backindex) vemzpasku ();      
    }
  }
}

/* [ulozdopasku] Abychom obnovili pro potreby [vsechnareseni] stav
   pole reseni, zaznamenavame si ve funkci ulozprvek() pri hodnote
   promenne zkousime=1 do jednorozmerneho globalniho pole track[]
   (tzv. "pasku") souradnice zmeneneho policka.  Ve funkci
   vsechnareseni() drzime v lokalni promenne backindex (kazda instance
   rekurzivniho volani ma svuj backindex) ukazatel na misto v poli
   track[], kam se potrebujeme po vyzkouseni moznosti vratit. Pri
   navratu cteme pozpatku track[] az k mistu backindex a uvadime podle
   udaju v track[] pole reseni do puvodniho stavu. Globalni ukazatel
   "trindex" ukazuje vzdy tesne za posledni vlozeny prvek.

   V cernobile verzi staci ukladat do track[] hodnotu "ii", a "i", tj.
   souradnice zmeneneho policka. Pri navratu vime urcite, ze na 
   techto souradnicich mame zpetne namalovat otaznik a snizit hodnoty
   vyreseno[] odpovidajiciho radku a sloupce o jednicku.

   V barevne verzi musime ulozit do track stav otazniku a mezer daneho
   policka pro kazdou barvu. Muze se totiz stat, ze nektere barvy
   urcite do policka nepatri, zatimco jine maji zatim v policku
   otaznik.  Tento stav policka ukladame do track[] v komprimovanem
   tvaru: bit=0 .. mezera, bit=1 .. otaznik. Do pole track[] je tedy
   potreba krome souradnic policka ulozit "pocetbarev-1" bitu. Ve 
   skutecnosti ukladame "pocetbarev" bitu, protoze navic jeden bit
   je rezervovan na informaci, zda je nutne pri obnove pole reseni
   snizovat udaj vyreseno[]. V barevne verzi se pri ulozprvek()
   totiz nezvedaji udaje vyreseno[] vzdy, ale jen v pripade zmeny
   ve vrstve 0.

   Ve funkci vsechnareseni() je uveden vypocet hodnoty trskip, coz je
   pocet prvku pole track[], ktere je potreba na vyse popsanou bitovou
   informaci rezervovat. Ve vzorci se pracuje s konstantou SIZEINT,
   coz je pocet bitu jednotliveho prvku pole track. Napriklad, pokud
   mame 32 bitovy prvek pole track (to byva obvykle) a mame 34 barev
   (nepocitame barvu pozadi) a pricteme jeden bit na informaci o
   obnove vyreseno[], pak je potreba ukladat pro kazde zmenene policko
   35 bitu a je tedy trskip=2. V prvnim prvku je vyuzito vsech 32 bitu
   a ve druhem prvku jeste vyuzivame dalsi tri bity.  

   Ackoli funkce vsechnareseni() alokovala track dostatecne veliky,
   muze se stat, ze nebude stacit. K tomu muze dojit jen u barevnych
   lustovek, kdy se k jednomu policku vracime vicekrat, nez jej
   definitivne vyresime. Kazda nova navsteva policka ulozi
   novou informaci v track[]. Proto v pripade potreby zdvojnasobime 
   velikost track[] novym volanim malloc(). 
   */

void ulozdopasku (int i, int ba)
{
  unsigned int k, j, b=0;
  unsigned int *track1;

  if (trindex+trskip+2 > tracktotal) {  /* delka pasku je nepostacujici */
    tracktotal *= 2;
    track1 = getmemory (tracktotal*sizeof(unsigned int), "track1");
    wlog (2, " track<%ld>\n", tracktotal);
    for (k=0; k<trindex; k++) track1[k] = track[k];
    free (track);
    track = track1;
  }
  if (!dvebarvy) 
    for (k=0; k<trskip; k++) {
      track [trindex] = 0;
      for (j=0; j<SIZEINT; j++) {
	track [trindex] *= 2;
	if (b==0) {
	  if (ba==0) track [trindex] += 1;
	}
	else if (rpole (i, b) == '?') track [trindex] += 1;
	b++;
	if (b>=pocetbarev) break;
      }
      trindex++;
    }
  track [trindex++] = ii;
  track [trindex++] = i;
}

/* [vemzpasku] Funkce restauruje vzdy jedno policko pole[] na zaklade
   udaju z track[], ktere jsme tam vlozili pomoci ulozdopasku().
   Soucasne funkce snizi trindex tak, aby znovu ukazoval za posledni
   jeste neprectenou hodnotu v track[].
   */

void vemzpasku ()
{
  int i, j, k, b, m;

  i = track [--trindex];
  ii = track [--trindex];
  if (!dvebarvy) {
    b = pocetbarev - 1;
    m = pocetbarev % SIZEINT;
    for (k=0; k<trskip; k++) {
      trindex--;
      if (k>0) m = SIZEINT;
      for (j=0; j<m; j++) {
	if (track [trindex] % 2) spole (i, b, '?');
	else                     spole (i, b, ' ');
	track [trindex] /= 2;
	b--;
      }
    }
  }
  m = (rpole (i, 0) == '?');
  spole (i, 0, '?');
  if (m || dvebarvy) {
    vyreseno [ii]--;
    if (ii<pocetradku) vyreseno [pocetradku+i]--; 
    else               vyreseno[i]--;
    return;
  }
}

/* [triddlers] Program grid byl puvodne udelan jen pro griddlers.
   Protoze ale obsahuje koncept zobecneneho radku, nebyl problem
   bez vetsich zasahu do nej implementovat i triddlers.

   Trochu casu zabere spekulovani, jak nelepe indexovat
   dvourozmerne pole[][], abychom do nej napasovali trojuhelnikovou sit.
   Rozhodli jsme se ponechat radky pole[] tak, aby odpovidaly radkum
   triddlerove site. 

            F !/                                     !/
         ---------                                   --
       / 012345678 \                                45
    A / 01234567890 \        / 0                   45
     / 0123456789012 \ E       12                 45
     \ 12345678901234 \         34               45
   !- \ 34567890123456 \         56             45
       \ 5678901234567 /          78          \ 5
     B  \ 78901234567 /            90
         \ 901234567 / D            12      
          \ 1234567 /                34       !- \ 34567890123456 \   
            ------                   --
              C \!                     \!

   Strany sestiuhlenika jsme oznacili A, B, C, D, E, F. Plati pro ne 
   nasledujici vztah: A, B, C, D mohou byt libovolne, ovsem takove,
   aby E = A + B - D,  F = C + D - A  byla kladna cisla.

   Ve funkci nactiproblem() nastavime a prekontrolujeme hodnoty
   A, B, C, D, E, F. Navic polozime: pocetradku = A + B;
   pocetsloupcu = C + D + E + F.

   V pravo od sestiuhelniku je naznaceno, z jakych indexu se sklada
   zobecneny radek, ktery je cten jednak z mista !-, podruhe z mista
   \! a potreti z mista !/. Je videt, ze bude potreba naprogramovat 
   nove funkce rpole() a ulozprvek(), ktere se budou o tuto 
   problematiku starat. Napr. v teto ukazce nulty prvek radku 
   !- je pole[4][3], nulty prvek radku \! je pole[a+b-1][14], 
   a nulty prvek radku !/ je pole[0][5]. 

   Musime tedy naprogramovat prepocet ze souradnic zobecneneho 
   radku ii, k (k-ty prvek ii-teho zobecneneho radku) na souradnice 
   i, j pole[][].

   Nejprve provedene nastaveni promennych ll, iityp, tr0 a tr1 
   v zavislosti na vysetrovanem zobecnenem radku r. To provede 
   funkce trset(), kterou pri vstupu do radku r=ii zavolame jen jednou.
   iityp je jedno z pismen A, B, C, D, E, F podle nazvu strany 
   sestiuhelnika, kde ii-ty radek zacina. Funkce trset() nastavi 
   iityp hlavne proto, aby nasledujici funkce tri(), trj()
   (ktere maji pracovat rychle) mohly vyuzit konstrukce switch(iityp).

   Promenna tr0 obsahuje pocatecni index sloupce pole[] pri k=0. 
   Promenna tr1 obsahuje (v pripade iityp\in{a,b}) koncovy sloupec pole[], 
   presneji: ukazuje za posledni prvek radku pole[]. V ostatnich pripadech
   obsahuje tr1 pocatecni index raku pole[] pri k=0.

   Funkce tri() vrati souradnici i a funkce trj() vrati souradnici j.
   pro pouziti v kontextu pole[i][j]. Vstupem je k (k-ty prvek)
   a funkce pracuji s globalni promennou ii, resp. s predpocitanymi
   konstantami tr0, tr1 (kvuli optimalizaci rychlosti).

   Neptejte se, jak jsme na uvedene vzorce prisli. Vyzaduje to nactrnout 
   si sestiuhelnik a spekulovat. Kazdy jednotlivy vzorecek nas stal
   nezanedbatelnou dobu vyrazneho dusevniho usili.  
   */

void trset (int r)
{
  if      (r<A)         iityp = 'A';
  else if (r<A+B)       iityp = 'B';
  else if (r<A+B+C)     iityp = 'C';
  else if (r<A+B+C+D)   iityp = 'D';
  else if (r<A+B+C+D+E) iityp = 'E';
  else                  iityp = 'F';

  switch (iityp) {
  case 'A': tr0 = 0;
            tr1 = (r<E) ? 2*(F+r)+1 : 2*(E+F);
	    ll = tr1 - tr0;
	    return;
  case 'B': tr0 = 2*(r-A) + 1;
            tr1 = (r<E) ? 2*(F+r)+1 : 2*(E+F);
	    ll = tr1 - tr0;
	    return;
  case 'C': tr0 = 2*(r-A);
            tr1 = A+B-1;
            ll = (r-A-B<A) ? 2*(r-A)+1 : 2*(A+B); 
            return;
  case 'D': tr0 = 2*(B+C) - 1;
            tr1 = 2*(A+B) + C - 1 - r;
            ll = ((r-A-B<A) ? 2*(r-A)+1 : 2*(A+B)) - 2*(r-A-B-C) - 1;
	    return;
  case 'E': tr0 = 2*(A+B+C+D+E+F-1 - r);         
            tr1 = 2*(A+B) + C - 1 - r;
            ll = (r-A-B-C-D<C) ? 2*(r-A-B-C)+1 : 2*(C+D);
	    return;
  case 'F': tr0 = 2*(A+B+C+D+E+F - r) - 1;
            tr1 = 0;
            ll = ((r-A-B-C-D<C) ? 2*(r-A-B-C)+1 : 2*(C+D)) - 2*(r-A-B-C-D-E) - 1;
	    return;
  }
}

int tri (int k)   /* vrati radek pole[][] k-teho prvku radku ii */
{
  switch (iityp) {
  case 'A':  
  case 'B':  return  ii;
  case 'C':  return  tr1 - k/2;
  case 'D':  return  tr1 - (k+1)/2;
  case 'E':  return  tr1 + (k+1)/2;
  case 'F':  return  k/2;
  }
  return 0;  /* aby se -Wall nazral a koza zustala cela */
}

int trj (int k)  /* vrati sloupec pole[][] k-teho prvku radku ii */
{
  switch (iityp) {
  case 'A':  return  k;
  case 'B':  return  tr0 + k;
  case 'C':  return  tr0 - k;
  case 'D':  return  tr0 - k;
  case 'E':  return  tr0 + k%2;
  case 'F':  return  tr0 - k%2;
  }
  return 0;  /* aby se -Wall nazral a koza zustala cela  */
}

/* [triddlers-radky-krizem] Ke kazdemu prvku k na zobecnenem radku ii
   potrebujeme obcas zjistit, jak vypadaji ostatni dve zobecnene radky,
   ktere tento prvek obsahuji. Pokud jde o radek typu a nebo b, tak
   ten zjistime zavolanim funkce tri(). Pro ostatni typy radku (c nebo d,
   e nebo f) musime naprogramovat nasledujici funkce:
   */

int iicd (int k)   /* vrati radek typu c nebo d obsahujici prvek k */
{
  switch (iityp) {
  case 'A':  return  A - ii - 1 + A + B + (k+1)/2;
  case 'B':  return  A + B + k/2;
  case 'C':  
  case 'D':  return  ii;
  case 'E':  return  A + B + C + D - 1 - k/2;
  case 'F':  return  2*(A + B + C + D) + E - ii - 1 - (k+1)/2;
  }
  return 0;  /* aby se -Wall nazral a koza zustala cela  */
}

int iief (int k)   /* vrati radek typu e nebo f obsahujici prvek k */
{
  switch (iityp) {
  case 'A':  return  A + B + C + D + E + F - 1 - k/2; 
  case 'B':  return  A + B + C + D + E + F - 1 - (k+tr0)/2;
  case 'C':  return  2*(A + B + C) + D - 1 - ii + (k+1)/2;
  case 'D':  return  A + B + C + D + k/2;
  case 'E':
  case 'F':  return  ii;
  }
  return 0;  /* aby se -Wall nazral a koza zustala cela  */
}

/* [tridulozprvek] Nyni mame vse pripraveno na naprogramovani funkce
   tridulozprvek(), ktera je analogii funkce ulozprvek(), ale pro triddlers.
   */

void tridulozprvek (int k, int b, char c)
{
  int x=0, y=0;

  pole [b*vrstva + tri(k)*maxj + trj(k)] = c;
  switch (iityp) {
  case 'A':
  case 'B': x = iicd(k); y = iief(k);  break;
  case 'C':
  case 'D': x = tri(k);  y = iief(k);  break;
  case 'E':
  case 'F': x = tri(k);  y = iicd(k);  break;
  }
  cosradkem[x] = 1;  cosradkem[y] = 1;
  dukladne [x] = 0;  dukladne [y] = 0;
  if (b > 0) return;
  vyreseno [ii]++;  vyreseno[x]++;  vyreseno[y]++;
}

/* [tridmaxll] je funkce, ktera zjisti z parametru triddleru maximalni 
   delku zobecneneho radku.
   */

int tridmaxll ()
{
  int v;

  if (E<A) trset (A-1);
  else     trset (A);
  v = ll;
  if (A<C) trset (A+B+C-1);
  else     trset (A+B+C);
  if (ll>v) v = ll;
  if (B>F) trset (A+B+C+D+E-1);
  else     trset (A+B+C+D+E);
  if (ll>v) v = ll;
  return v;
}

/* [cisloradku] Funkce vrati cislo radku nebo sloupce. V pripade triddlers
   cisluje podel kazde strany sestiuhelnika zvlast.
   */

int cisloradku (int r)
{
  if (triddlers) switch (iityp) {
  case 'A': return  r + 1;
  case 'B': return  r - A + 1;
  case 'C': return  r - A-B + 1;
  case 'D': return  r - A-B-C + 1; 
  case 'E': return  r - A-B-C-D + 1;
  case 'F': return  r - A-B-C-D-E + 1; 
  }
  if (r<pocetradku) return  r + 1;
  else              return  r - pocetradku + 1;
}

/* [servis] To hlavni bylo receno. Dale uz asi v programu nenajdete nic 
   objevneho. Je treba jeste naprogramovat servisni funkce, ktere umozni
   nacist zadani ze souboru, tisknout logovaci informace nebo take reseni. 

   Pokud jste docetli az sem, pak gratulujeme. Doufame, ze se nebudete
   divit, ze od teto chvile jsou komentare mene podrobne, protoze
   nyni uz se jedna jen o rutinni progrmatorskou drinu bez hlubsi myslenky. 
   */

/* [usage][cmdparser] Nasleduje funkce vypisujici pouziti programu a 
   funkce, ktera precte parametry prikazove radky 
   */

void usage ()
{
  printf ("\nusage:  grid [options] <file>\n");
  printf ("   where <file> is the input file with the task\n");
  printf ("   options:\n");
  printf ("    -help          ... this text is printed and program terminates\n");
  printf ("    -p <number>    ... start paused mode after <number> steps\n");
  printf ("    -stop <number> ... do only <number> steps, don't solve the whole task\n");
  printf ("    -log <number>  ... set the verbosity of the output (default: 2)\n");
  printf ("    -out <number>  ... the format of solution printing (default: 2)\n");
  printf ("    -ini <file>    ... read partial solution from <file>\n");
  printf ("    -cmp <file>    ... read your partial solution for comparison\n");
  printf ("    -total <number>... maximal printed solutions limited by <number>\n");
  printf ("    -xpm <number>  ... number of maximal xpm output files (default: 1)\n");
  printf ("    -bl <number>   ... limit of bloks used in first steps (default: 7)\n");
  exit (1);
}

void cmdparser (int argc, char**argv)
{
  int ac=0;

  while (++ac < argc && argv[ac][0] == '-' && argv[ac][1] != 0) 
  switch (argv[ac][1]) {
  case 'p': if (argv[ac][2] != 0) usage ();
            if (++ac >= argc) usage ();
	    if (argv[ac][0] < '0' || argv[ac][0] > '9') usage();
	    paused = atoi (argv[ac]);
	    break;
  case 'c': if (strcmp (argv[ac], "-cmp") != 0) usage ();
            if (++ac >= argc) usage ();
            cmpfile = openfile (argv[ac]);
	    break;
  case 'i': if (strcmp (argv[ac], "-ini") != 0) usage ();
            if (++ac >= argc) usage ();
	    inifile = openfile (argv[ac]);
	    steps = 1;
	    break;
  case 'l': if (strcmp (argv[ac], "-log") != 0) usage ();
            if (++ac >= argc) usage ();
	    if (argv[ac][0] < '0' || argv[ac][0] > '9') usage();
	    llevel = atoi (argv[ac]);
	    break;
  case 'o': if (strcmp (argv[ac], "-out") != 0) usage ();
            if (++ac >= argc) usage ();
	    if (argv[ac][0] < '0' || argv[ac][0] > '9') usage();
	    outlevel = atoi (argv[ac]);
	    break;
  case 's': if (strcmp (argv[ac], "-stop") != 0) usage ();
            if (++ac >= argc) usage ();
	    if (argv[ac][0] < '0' || argv[ac][0] > '9') usage();
	    steps = atoi (argv[ac]);
	    break;
  case 't': if (strcmp (argv[ac], "-total") != 0) usage ();
            if (++ac >= argc) usage ();
	    if (argv[ac][0] < '0' || argv[ac][0] > '9') usage();
	    totalnum = atoi (argv[ac]);
	    break;
  case 'x': if (strcmp (argv[ac], "-xpm") != 0) usage ();
            if (++ac >= argc) usage ();
	    if (argv[ac][0] < '0' || argv[ac][0] > '9') usage();
	    xpmnum = atoi (argv[ac]);
	    break;
  case 'b': if (strcmp (argv[ac], "-bl") != 0) usage ();
            if (++ac >= argc) usage ();
	    if (argv[ac][0] < '0' || argv[ac][0] > '9') usage();
	    limitbloku = atoi (argv[ac]);
	    break;
  default: usage ();
  }
  if (ac != argc-1) usage();
  mainfile = openfile (argv[ac]);
}

/* [printstatistic] vytiskne zaverecnou statistiku poctu kroku a poctu
   vstupu do radkovych algoritmu 
   */

void printstatistic ()
{
  printf ("    steps %ld(%ldr,%ldc,%ldi,%ldt), lines n:%ld/%ld, i:%ld/%ld\n", 
	  pruchod-1, sumkr, sumkc, sumki, sumkt, sumur, sumrr, sumui, sumri);
}

/* [printpole] Funkce vytiskne reseni. Je-li podrobne==1, pak stav reseni
   neni ukoncen. Ma tedy smysl pri -out >= 3 tisknout jednotlive vrstvy 
   pole[]. Je-li lustovka cernobila, pak pri podrobne==1 vytiskneme
   primy obsah vrstvy 0, protoze tam mohou byt novinky vyznaceny 
   znakem '#' nebo '-'.
   */

char jmenobarvy[MAXBAREV+1][MAXDELKASLOVA];  /* jmena barev pro xpm */ 

void printpole (int podrobne)
{
  int b;
  if (outlevel==0) return;
  if (podrobne && (dvebarvy || outlevel>=3)) {
    for(b=1;b<pocetbarev;b++){
      if (outlevel>=2 && !dvebarvy) printf ("\nCOLOR: %s", jmenobarvy[b]);
      if (triddlers) tridprintpole (b);
      else           gridprintpole (b);
    }
    if (dvebarvy) return;
  }
  if (triddlers) tridprintpole (0);
  else           gridprintpole (0);
}

/* [gridprintpole] vytiskne jednu barevnou vrstvu pole[].
   Pri b=0 tiskne souhrnou informaci o vsech barvach soucasne.
   */

void gridprintpole (int b)
{
  int i, j, jak;
  
  if (outlevel>=2) {
    if (pocetsloupcu>=10) printf ("\n     ");
    for (j=1; j<=pocetsloupcu/10; j++) printf ("%10d", 10*j);
    printf ("\n:::: ");
    for (j=1; j<=pocetsloupcu; j++) printf ("%d", j%10);
  }
  printf ("\n");
  for (i=0; i<pocetradku; i++) {
    if (outlevel>=2) {
      if ((i%10)==9)  printf ("%3d: ", (i+1));
      else            printf ("%3d: ", (i+1)%10);
    }
    for (j=0; j<pocetsloupcu; j++){
      if (b) {
	if (dvebarvy) printf ("%c", pole[IJ]);
	else printf ("%c", pole[BIJ]);
      } 
      else { 
	if((jak=jakabarva(i,j)) == -1 ) printf ("?");
	else if (pole[IJ] == 'x') printf ("#");
	else if (pole[IJ] == 'X') printf ("-");
	else printf ("%c", barva[jak]);
      }
    }
    printf ("\n");
  }
  if (outlevel>=2) { 
    printf (":::: ");
    for (j=1; j<=pocetsloupcu; j++) printf ("%d", j%10);
    if (pocetsloupcu>=10) printf ("\n     ");
    for (j=1; j<= pocetsloupcu/10; j++) printf ("%10d", 10*j);
    printf ("\n");
  }
}
  

/* [tridprintpole] je varianta gridprintpole() pro triddlers
   */

void tridprintpole (int b)
{
  int i, j, d, e, a, jak;

  if (outlevel>=2) {
    printf ("\n      %*s", A+1, "");
    for (j=F; j>0; j--) printf (" %d", j%10);
    printf ("\n:::: ");
  } 
  else printf ("\n");
  e = E; d = D; a = 1;
  printf ("%*s", A+1, "");  rprint (stdout, 2*F+1, '-'); 
  if (outlevel>=2 && e) printf (" %2d", e--);
  printf ("\n");
  for (i=0; i<pocetradku; i++) {
    trset (i);
    if (outlevel>=2) {
      if (i==A) a = 1;
      printf ("%3d: ", a++);
    }
    if (i<A) printf ("%*s/ ", A-i-1, "");
    else     printf ("%*s\\ ", i-A, "");
    for (j=tr0; j<tr1; j++) 
      if (b) {
	if (dvebarvy) printf ("%c", pole[IJ]);
	else          printf ("%c", pole[BIJ]);
      }
      else {
	if((jak=jakabarva(i,j)) == -1 ) printf ("?");
	else if (pole[IJ] == 'x') printf ("#");
	else if (pole[IJ] == 'X') printf ("-");
	else printf ("%c", barva[jak]);
      }
    if (i<E) {
      printf (" \\");
      if (outlevel>=2 && e) printf ("%2d", e--);
      printf ("\n");
    }
    else {
      printf (" /");
      if (outlevel>=2 && i!=E) printf ("%2d", d--);
      printf ("\n");
    }
  }
  if (outlevel>=2) printf (":::: ");
  printf ("%*s", B+1, ""); rprint (stdout, 2*C+1, '-'); 
  if (outlevel>=2) printf (" %2d", d);
  printf ("\n");
  if (outlevel>=2) { 
    printf ("      %*s", B+1, "");
    for (j=1; j<=C; j++) printf (" %d", j%10);
    printf ("\n");
  }
}

/* [rprint] je opakujici se print (repeated print).
   Znak c se vytiskne k-krat. navic zvedneme o pocet 
   vytistenych znaku promennou rp, coz se bude hodit
   pri ukladani do XPM
   */

int rp;

void rprint (FILE *f, int k, char c)
{
  register int i;

  for (i=0; i<k; i++) fprintf (f, "%c", c);
  rp += k;
}


/* [jakabarva] Pro potreby printpole() se hodi funkce jakabarva(), ktera
   vraci index barvy daneho policka pole[][i][j].
   */

int jakabarva (int i, int j) 
{
  int b;

  if(pole[IJ]=='?') return -1;
  if(pole[IJ]==' ' || pole[IJ]=='-') return 0;
  if (dvebarvy) return 1;
  for(b=1; pole[BIJ] != '*' && 
	   pole[BIJ] != '#'; b++);
  return b;
}

/* [wlog] Funkce na tisk informace pouze pri llevel >= level.
   Pouzivame zde fflush(), protoze na pomalych strojich potrebuje uzivatel
   videt, ze se neco deje.
   */

void wlog (int level, char *s, long int value) 
{
  if (llevel < level) return;
  printf (s, value);
  fflush (stdout);
}

/* [printbloky] tiskne rozlozeni bloku pro potreby llevel > 3.
   Je to velice nazorna logovaci informace, jak pracuji pouzite algoritmy.
   */

void printbloky (int * p, char *s) 
{
  int i, j, b=0, d;      
  int nz;

  printf ("%5s: |", s);
  j = 0; d = 0;
  if (j<np) nz = p[j];
  else      nz = ll;
  for (i=0; i<ll; i++) {
    if (i==nz) {
      b = bbl[j]; d = vbl[j++]; 
      if (j<np) nz = p[j];
      else      nz = ll;
    }
    if (d) { 
      d--;
      printf ("%c", barva[b]);
    }
    else
      printf (" ");
  }
  printf ("|\n");
}

/* [logradek] tiskne stav radku pred a po provedeni algoritmu, pokud uz
   je znam vysledek algoritmu v poli pu[][], ale jeste nebylo provedeno
   ulozreseni(). 

   Pri "jak==START" vytiskne jen vstupni informaci o radku,
   pri "jak==END" vytiskne jen koncove udaje o novem reseni a pri
   "jak==ALL" tiskne pocatecni udaje, dale vysledek pravo-leveho algoritmu
   a potom koncove udaje.
   */

void logradek (int jak)  
{
  int i, j, new;

  if (jak != END) {
    if (ii < pocetradku) printf ("\nROW: %d %c -- (%d) ", cisloradku(ii), iityp, np);
    else              printf ("\nCOLUMN: %d %c -- (%d) ", cisloradku(ii), iityp, np);

    printf ("[%d], found=%d, bloks: ", dukladne[ii], vyreseno[ii]);
    for (j=0; j<np; j++) {
      printf (" %d", vb[ii][j]);
      if (!dvebarvy) printf ("%c", barva[bb[ii][j]]);
    }
    if (dvebarvy) {
      printf ("\nin:    |");
      for (i=0; i<ll; i++) printf ("%c", rpole(i,0));
      printf ("|");
    }
    else for (j=1; j<pocetbarev; j++) {
      printf ("\nin  %c: |", barva[j]);
      for (i=0; i<ll; i++) printf ("%c", rpole(i,j));
      printf ("|");
    }
    printf ("\n");
  }
  if (jak == START) return;
  if (llevel >= 4 && jak==ALL) {
    printbloky (pr, "right");
    printbloky (p,  "left");
  }
  new=0;
  if (dvebarvy) {
    printf ("out:   |");  
    for (i=0; i<ll; i++) { 
      if (rpole(i,0) == '?') {
	if (pu[0][i] == 0)          { new++; printf("-"); }
	else if (pu[0][i] == sumpu) { new++; printf("#"); }
	else printf("?");
      }
      else printf ("%c", rpole(i,0));
    }
    printf ("| found news: %d\n", new);
  }
  else for (j=1; j<pocetbarev; j++) {
    new = 0;
    printf ("out %c: |", barva[j]); 
    for (i=0; i<ll; i++) {
      if (rpole(i,j) == '?') {
	if (pu[j-1][i] == 0)          { new++; printf("-"); }
	else if (pu[j-1][i] == sumpu) { new++; printf("#"); }
	else printf("?");
      }
      else printf ("%c", rpole(i,j));
    }
    printf ("| found news: %d\n", new);
  }
}

/* [testkonzistence] funkce provede kontrolu, zda soucet policek jedne barvy
   scitany podel realnych radku da stejny vysledek, jako soucet podel sloupcu.
   Tento test se provede pro vsechny pouzite barvy.

   Vzhledem k zobecneni u triddlers funkce pracuje s rozsahem
   ii = x..y-1 a to porovnava s radky ii=y..z-1. V pripade 
   triddlers pak volame tuto funkci dvakrat za sebou.
   */

void testkonzistence (int x, int y, int z) 
{
  int b, sumr, sums;

  for (b=1; b<pocetbarev; b++) {
    sumr = 0;
    for (ii=x; ii<y; ii++) sumr += sumabarev (b);
    sums = 0;
    for (ii=y; ii<z; ii++) sums += sumabarev(b);
    if (sumr != sums) {
      fprintf (stderr, 
	       "the number of items in rows != in columns, color: %c (%s)\n",
	       barva [b], jmenobarvy [b]);
      exit (2);
    }
  }
}

int sumabarev (int b)        /* soucet policek barvy b na radku ii */
{
  int j, v;

  v = 0;
  for (j=0; j<pb[ii]; j++) if (bb[ii][j] == b) v += vb[ii][j];
  return v;
}

/* [nactiproblem] Funkce nacita ulohu podle formatu popsaneho v souborech
   grid.pdf, kocka.g, ruze.g, aladin.g a brontik.g.

   Funkce postupne cte dalsi znak pomoci makra NEXTCHAR a uklada jej do 
   globalni promenne "ch". tato promenna je globalni, protoze s ni nepracuje
   funkce nactiproblem() sama, ale casto spolupracuje s dalsimi funkcemi, 
   jako napr. skipblanks(), skipline() atd.
   */

#define NEXTCHAR getc (mainfile)

int defbarva,          /* je deklarovana defaultni barva? */
    trojuhelniky,      /* jsou deklarovany lepici trojuhelniky? */
    mkformat,          /* je pouzit format vstupniho souboru podle mk.exe? */
    ch;                /* globalni promenna s naposledy prectenym znakem */

char znbarvy[MAXBAREV];   /* vstupni znaky barev */
int lep[MAXBAREV],        /* smer lepeni lepicich trojuhelniku pri cteni */
    lepc[MAXBAREV];       /* v radku (lep) a ve sloupci (lepc) */

char barvakolem = '-';  /* symbol pro barvu okoli do XPM pri triddlers */

#define LEFT   -1     /* trojuhelnik lepi doleva */
#define NORMAL  0     /* barva nelepi */
#define RIGHT   1     /* trojuhelnik lepi doprava */

void nactiproblem ()  /* precte zadani ze souboru */
{
  int i, i0=0, j, maxsirkaradku, indexradku=0;

  ch = NEXTCHAR; 
  ll = 1;           /* pro tento pripad je ll cislo radku v souboru */
  barva [0] = ' '; strcpy (jmenobarvy[0], "white");
  barva [1] = '*'; strcpy (jmenobarvy[1], "black");
  trojuhelniky = 0;
  if (ch >= '0' && ch <= '9') {
    pocetbarev = 2; defbarva = 1; mkformat = 1;
    ii = 0;
    goto start;
  }
  mkformat = 0;
  while (1) {
    while (ch != ':' && ch != '#' && ch != EOF) skipline ();
    if (ch == EOF) formaterror ("unexpected end of file");
    if (ch == ':') { ch = NEXTCHAR; break; }
    if (ch == '#') {
      ch = NEXTCHAR;
      if (ch == 'D' || ch == 'd') { ch = '!' ; break; }
      if (ch == 'T' || ch == 't') { triddlers = 1;  strcpy (jmenobarvy[MAXBAREV], "gray"); }
      skipline ();
    }
  }
  if (ch == '!') {  /* ":!" nebo "#d" ... deklarace barev */
    skipline ();
    ii = 2; 
    defbarva = 0;  barva [0] = ' '; barva [1] = '*'; 
    while (1) {
      if (ch == ':') break;
      skipblanks ();
      if (ch == EOF) formaterror ("unexpected end of file");
      switch (ch) {
      case '0': i = 0; break;
      case '1': i = 1; defbarva = 1; break;
      case '6': i = MAXBAREV; break;
      default:  i = ii++; 
	        if (i >= MAXBAREV) formaterror ("limit MAXBAREV exceeded");
	        znbarvy [i] = ch;
      }
      ch = NEXTCHAR;
      if (ch != ':') formaterror ("the colon expected");
      ch = NEXTCHAR;
      if (ch == '\n' || ch == EOF)
	formaterror ("wrong color declaration");
      if (i != MAXBAREV) barva [i] = ch;
      ch = NEXTCHAR; skipblanks (); 
      j = 0;
      while (ch != ' ' && ch != '\t' && ch != '\n') {
        if (j>=MAXDELKASLOVA-1) formaterror ("limit MAXDELKASLOVA exceeded");
	jmenobarvy [i][j++] = ch;
	ch = NEXTCHAR;
      }
      if (j == 0) formaterror ("the color name for XPM expected");
      jmenobarvy [i][j] = 0;
      if (i == MAXBAREV) { skipline (); continue; }
      skipblanks ();
      lep[i] = NORMAL;  lepc[i] = NORMAL;
      if (ch == '<') {
	trojuhelniky = 1;
	lep[i] = LEFT;
	ch = NEXTCHAR;
	if (ch == '>') lepc[i] = RIGHT;
	else           lepc[i] = LEFT;
      }
      else if (ch == '>') {
	trojuhelniky = 1;
	lep[i] = RIGHT;
	ch = NEXTCHAR;
	if (ch == '<') lepc[i] = LEFT;
	else           lepc[i] = RIGHT;
      }
       skipline ();
    }
    pocetbarev = ii;            /* pocet barev vcetne barvy pozadi */
    if (defbarva == 0) {        /* defaultni barva neni deklarovana */
      for (i=1; i<pocetbarev-1; i++) {
	barva [i] = barva [i+1];   
	znbarvy [i] = znbarvy [i+1];
	lep [i] = lep [i+1];  lepc [i] = lepc [i+1];
	strcpy (jmenobarvy[i], jmenobarvy[i+1]);
      }
      pocetbarev--;
    }
  }
  else {
    pocetbarev = 2; defbarva = 1;
  }
  ii = 0;
start:
  skipline ();
  maxsirkaradku = 0;
  while (1) {
    if (ch == EOF) formaterror ("unexpected end of file");
    if (ch == ':' || ch == '#') break;
    if (triddlers && ii >= MAXRADKU+MAXSLOUPCU)
      formaterror ("limit MAXRADKU+MAXSLOUPCU exceeded");
    if (!triddlers && ii >= MAXRADKU) formaterror ("limit MAXRADKU exceeded");
    j = nactiradek ();
    if (j < 0) formaterror ("unexpected end of file");
    if (npp > maxsirkaradku) maxsirkaradku = npp, indexradku = ii;
    pb[ii++] = j; 
    ch = NEXTCHAR; ll++;
  }
  pocetradku = ii - i0;
  if (pocetradku==0 && !triddlers) formaterror ("no rows declared");
  if (trojuhelniky) for (i=1; i<pocetbarev; i++) lep[i] = lepc[i];
  skipline ();
  while (1) {
    if (ch == ':' || ch == EOF) break;
    if (triddlers && ii >= MAXRADKU+MAXSLOUPCU)
      formaterror ("limit MAXRADKU+MAXSLOUPCU exceeded");
    if (!triddlers && ii - pocetradku >= MAXRADKU) 
      formaterror ("limit MAXSLOUPCU exceeded");
    j = nactiradek ();
    if (j == -1) break;
    if (npp > pocetradku && !triddlers) 
      formaterror ("the number of rows is less than the column length");
    pb[ii++] = j;
    ch = NEXTCHAR; ll++;
  }
  pocetsloupcu = ii - pocetradku - i0;
  if (pocetsloupcu==0 && !triddlers) formaterror ("no columns declared");
  if (pocetsloupcu < maxsirkaradku && !triddlers) {
    fprintf (stderr, "Error in the input file: ");
    fprintf (stderr, "the number of columns is less than the length of row %d\n", 
             indexradku);
    exit (10);
  }
  if (triddlers) {
    if (A+B==0) { 
      A = pocetradku;  B = pocetsloupcu;  i0 = A+B;      
      if (A+B==0) formaterror ("wrong hexagonal sides: A+B = 0");
      goto start; 
    }
    if (C+D==0) { 
      C = pocetradku;  D = pocetsloupcu;  i0 = A+B+C+D;  
      if (C+D==0) formaterror ("wrong hexagonal sides: C+D = 0");
      goto start; 
    }
    E = pocetradku;  F = pocetsloupcu; 
    if (E+F==0)     formaterror ("wrong hexagonal sides: E+F = 0");
    if (E != A+B-D) formaterror ("wrong hexagonal sides: A+B-D != E");
    if (F != D+C-A) formaterror ("wrong hexagonal sides: D+C-A != F");
    pocetradku = A+B;
    pocetsloupcu = C+D+E+F;    
  }
  if (mainfile != stdin) fclose (mainfile);
  if (triddlers) {
    testkonzistence (0, A+B, A+B+C+D);
    testkonzistence (A+B, A+B+C+D, A+B+C+D+E+F); 
  }  
  else testkonzistence (0, pocetradku, pocetradku+pocetsloupcu);
  dvebarvy = (pocetbarev == 2);
}

/* [nactiradek] precte radek vstupniho souboru obsahujici deklarace 
   realneho radku nebo realneho sloupce.

   Uvnitr cyklu "while(1)" cteme postupne jeden udaj, pokud lepici
   trojuhelniky nejsou deklarovany, nebo cteme trojici udaju, pokud
   lepici trojuhelniky jsou deklarovany. Trojice: pripadny lepici
   trojuhelnik vlevo, skutecny blok a pripadny lepici trojuhelnik
   vpravo. Funkce v takovem pripade zmensuje velikosti skutecnych bloku 
   o pocet lepicich trojuhelniku kolem bloku.  V takovem pripade muze
   blok mit nakonec delku nula. V takovem pripade jej funkce ze seznamu 
   bloku bez milosti odstrani.

   V prvnim pruchodu si funkce poznaci poznamku o lepeni ve tvaru 
   vm[ii][j]=2 a teprve v druhem pruchodu natavuje skutecne velikosti mezer.
   Problem je v tom, ze totiz bloky lepicich trojuhelniku na sebe mohou 
   navazovat bez mezery, i kdyz jsou stejne.  
   */

int nactiradek ()
{
  int i, j=0, lt=0, rt=0, prevbox, curb;

  while (1) {
    skipblanks ();
    if (ch == '\n' && mkformat && j == 0) return -1;
    if (ch == '\n' || ch == EOF) break;
    if (j >= MAXBLOKU) formaterror ("limit MAXBLOKU exceeded");
    if (trojuhelniky) {
      lt = nactibarvu (LEFT);
      if (lt) {
	vb[ii][j] = 1;  vm[ii][j] = -1;  bb[ii][j++] = lt;
	if (j >= MAXBLOKU) formaterror ("limit MAXBLOKU exceeded");
      }
    }
    vb[ii][j] = nacticislo ();
    if (vb[ii][j] == 0) {
      if (j == 0) { skipblanks(); return 0; }
      else formaterror ("zero as bock length is not allowed");
    }
    bb[ii][j] = nactibarvu (NORMAL);
    vm[ii][j] = 0;
    if (trojuhelniky) rt = nactibarvu (RIGHT);
    if (lt) vb[ii][j]--;
    if (rt) vb[ii][j]--;
    if (vb[ii][j] > 0) { 
      j++; prevbox = 1; 
    }
    else  {
      prevbox = 0;
      if (lt) { prevbox = 1; vm[ii][j-1] = 2; }
    }
    if (j >= MAXBLOKU) formaterror ("limit MAXBLOKU exceeded");
    if (rt) {
      if (prevbox && bb[ii][j-1] == 0) bb[ii][j-1] = 1;
      if (prevbox) vm[ii][j-1] = -1;
      vb[ii][j] = 1;  vm[ii][j] = 2;  bb[ii][j++] = rt;
    }
  }
  curb = npp = 0;
  for (i=0; i<j; i++) {
    npp += vb[ii][i]; 
    if (curb == bb[ii][i]) { vm[ii][i-1] = 1; npp++; }
    if (vm[ii][i] == 2) {
      vm[ii][i] = 0;
      curb = 0;
    }
    else curb = bb[ii][i];
  }
  return j;
}

/* [formaterror a dalsi] nasleduji pomocne funkce pro nactiproblem()
   */

void formaterror (char *s)  /* pomocna funkce pro nactiproblem() */
{
  fprintf (stderr, "Error in the input file, line %d: %s\n", ll, s);
  exit (10);
}

void error (char *s)  /* pomocna funkce pro osetreni chyby */
{
  fprintf (stderr, "Error: %s\n", s);
  exit (11);
}

void skipline ()            /* pomocna funkce pro nactiproblem() */
{
  while (ch != '\n' && ch != EOF) ch = NEXTCHAR;
  if (ch==EOF) formaterror ("unexpected end of file");
  ch = NEXTCHAR; ll++;
}

void skipblanks ()         /* pomocna funkce pro nactiproblem() */
{
  while (ch == ' ' || ch == '\t' || ch == '\r') ch = NEXTCHAR;
}

int nacticislo ()         /* pomocna funkce pro nactiproblem() */
{
  int v;
  if (ch < '0' || ch > '9')  formaterror ("the number is expected");
  v = 0;
  while (ch >= '0' && ch <= '9') {
    v = 10*v + ch - '0';
    ch = NEXTCHAR;
  }
  return v;
}

int nactibarvu (int jak)      /* vrati cislo barvy */
{
  int v;
  if (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' || ch == EOF || ch == ',') {
    if (defbarva == 0) formaterror ("the default color is undefined");
    if (jak==RIGHT) return 0;
    return 1;
  }
  for (v=1+defbarva; v<pocetbarev; v++)
    if (znbarvy[v] == ch) {
      if (jak != lep[v]) continue; 
      ch = NEXTCHAR;
      return v;
    }
  if (jak==LEFT || jak==RIGHT) return 0;
  if (jak==NORMAL && trojuhelniky) return 1; 
  formaterror ("the color symbol is undeclared");
  return 0;
}

/* [nactipart] Funkce nacte castecne reseni ze souboru s castecnym resenim.
   Pokud je f==cmpfile, pak je nutno reseni porovnat se skutecnym resenim,
   ktere mame v promenne pole[]. Pri porovnavani ukladame do pole[]
   ve vrstve nula otazniky, jsou-li v danem souboru otazniky a 'X'
   resp. 'x', pokud se reseni lisi od reseni v souboru. Funkce printpole()
   pak umi tyto znaky prevest na tisk beznychznaku '#' a '-', aby uzivatel
   rychle poznal, kde se jeho reseni lisi od spravneho.

   Pokud neporovnavame, pak jedoduse zanasime udaje ze souboru rovnou 
   do pole[][][].
   */

void nactipart (FILE *f)
{
  int i, j, b, cmpf;

  cmpf = (f==cmpfile);
  mainfile = f;
  ll = 1;
  ch = NEXTCHAR;
  tr0 = 0;  tr1 = pocetsloupcu;
next:
  while (ch != ':') skipline ();
  for (i=1; i<4; i++) { 
    ch = NEXTCHAR;
    if (ch != ':') goto next;
  }
  skipline ();
  for (i=0; i<pocetradku; i++) {
    if (triddlers) {
      trset (i);
      while (ch != '/' && ch != '\\' && ch != '\n' && ch != EOF) ch = NEXTCHAR;
    }
    else while (ch != ':' && ch != '\n' && ch != EOF) ch = NEXTCHAR;
    if (ch == '\n' || ch == EOF) formaterror ("the colon expected");
    ch = NEXTCHAR; 
    if (ch == '\n' || ch == EOF) formaterror ("unexpected end of line");
    ch = NEXTCHAR;
    for (j=tr0; j<tr1; j++) { 
      switch (ch) {
      case '?':
      case '.':  if (cmpf) pole[IJ] = '?';
                 break;
      case ' ':
      case '-':  if (cmpf) {
	           if (pole[IJ]=='*') pole[IJ]='x';
                 }
                 else { 
		   pole [IJ] = ' ';
		   if (!dvebarvy) for (b=1; b<pocetbarev; b++) pole [BIJ] = ' ';
		   vyreseno [i]++;  vyreseno[pocetradku+j]++;
		   cosradkem [i] = 1; cosradkem [pocetradku+j] = 1;
		 }
		 break;
      case '\n': 
      case EOF : formaterror ("next column?"); 
      default:   if (ch=='#') ch = '*';
	         if (dvebarvy) {
		   if (ch!='*' && ch!=barva[1]) 
		     formaterror ("unknown character fo black/white");
		 }
		 else {
		   for (b=1; b<pocetbarev; b++) pole [BIJ] = ' ';
		   for (b=1; b<pocetbarev; b++) if (barva[b] == ch) break;
		   if (b==pocetbarev) formaterror ("unknown character for color");
		   pole [BIJ] = '*';
		 }
		 if (cmpf) {
		   if (ch != barva [jakabarva(i,j)]) pole [IJ] = 'X';
		 }
		 else {
		   pole [IJ] = '*';
		   vyreseno [i]++;  vyreseno[pocetradku+j]++;
		   cosradkem [i] = 1; cosradkem [pocetradku+j] = 1;
		 }
      }
      ch = NEXTCHAR;
    }
    skipline ();
  }
  if (mainfile != stdin) fclose (mainfile);
}

/* [savexpm] ulozi udaje z pole[] do formatu XPM. Jmeno souboru odvodi
   z buf[]. 

   Funkce si nejprve prekontroluje, zda nejsou v deklaraci trojuhelniky.
   Pokud neexistuji trojuhelniky, pak funkce ulozi jedno policko jako jeden 
   pixel. Predpokladame totiz, ze si to uzivatel bude umet zvetsit treba 
   v gimpu.

   Jestlize existuje pozadavek na trojuhelniky, pak se jedno policko 
   uklada do XPM jako ctverec o velikosti XPMBLOCK * XPMBLOCK pixelu.
   */

#define XPMBLOCK  8

void savexpm ()   /* ulozi vysledek do xpm formatu */
{
  int i, j, i1, j1, k, b;
  char filename[MAXBUF];
  char fmt[15];

  FILE * outfile;
  char troj[MAXBAREV][3]; /* troj[b][0]==0   ... b neni trojuhelnik
                          troj[b][0]=='/' ... b je trojuhelnik typu "/"
			  troj[b][0]=='\' ... b je trojuhelnik typu "\"
			  troj[b][1]      ... znak barvy zleva
			  troj[b][2]      ... znak barvy zdola  */
  int pocetvsechbarev, numtroj;

  pocetvsechbarev = pocetbarev; 
  numtroj = 0;                       /* rozklad barev pro trojuhelniky */
  for (i=0; i<pocetbarev; i++) {
    j = 0;
    while (jmenobarvy[i][j] != 0 && jmenobarvy[i][j] != '/' &&
	   jmenobarvy[i][j] != '\\') j++;
    troj[i][0] = jmenobarvy[i][j];
    if (troj[i][0] == 0) continue;
    numtroj++;
    jmenobarvy[i][j] = 0;
    for (k=0; k<pocetvsechbarev; k++) 
      if (k != i && strcmp (jmenobarvy[k], jmenobarvy[i])==0) {
	troj[i][1] = barva[k]; break;
      }
    if (k==pocetvsechbarev) { /* musime zalozit novou levou barvu */
      if (pocetvsechbarev >= MAXBAREV) {
	fprintf(stderr,"limit MAXBAREV exceeded during tiangle preparation\n");
	return;
      }
      strcpy (jmenobarvy[k], jmenobarvy[i]);
      barva[k] = genbarva (k);
      if (barva[k]==0) {
	fprintf (stderr, "the next character for triangle color not found\n");
	return;
      }
      troj[i][1] = barva[k];  troj[k][0] = 0;
      pocetvsechbarev++;
    }
    jmenobarvy[i][j] = troj[i][0];
    j++;
    for (k=0; k<pocetvsechbarev; k++) 
      if (k != i && strcmp (jmenobarvy[k], &jmenobarvy[i][j])==0) {
	troj[i][2] = barva[k]; break;
      }
    if (k==pocetvsechbarev) { /* musime zalozit novou pravou barvu */
      if (pocetvsechbarev >= MAXBAREV) {
	fprintf(stderr,"limit MAXBAREV exceeded during triangle preparation\n");
	return;
      }
      strcpy (jmenobarvy[k], &jmenobarvy[i][j]);
      barva[k] = genbarva (k);
      if (barva[k]==0) {
	fprintf (stderr, "the next character for triangle color not found\n");
	return;
      }
      troj[i][2] = barva[k];  troj[k][0] = 0;
      pocetvsechbarev++;
    }
  }
  if (xpmnum==1) sprintf (filename, "%s.xpm", buf);
  else {
    k=xpmnum; j = 0; while (1) { j++; k /= 10; if (k==0) break; }
    sprintf (fmt, "%%s%%0%dld.xpm", j);
    sprintf (filename, fmt, buf, sumr);
  }
  outfile = fopen (filename, "w");
  if (outfile==NULL) {
    fprintf (stderr, "The file %s cannot be open to writting\n", filename);
    return;
  }
  k = strlen (filename) - 4;
  filename [k] = '_';
  fprintf (outfile, "/"); fprintf (outfile, "* XPM *");
  fprintf (outfile, "/\nstatic char *%s[] = {\n", filename);
  if (triddlers) { 
    tridsavexpm (outfile);
    goto finish;
  }
  if (numtroj) fprintf (outfile, "\"%d %d %d 1\",\n", 
			XPMBLOCK*pocetsloupcu, XPMBLOCK*pocetradku, 
                        pocetvsechbarev-numtroj);
  else fprintf (outfile, "\"%d %d %d 1\",\n", 
		pocetsloupcu, pocetradku, pocetvsechbarev-numtroj);
  for (i=0; i<pocetvsechbarev; i++) 
    if (troj[i][0]==0)  
      fprintf (outfile, "\"%c c %s\",\n", barva[i], jmenobarvy[i]);
  if (numtroj) 
    for (i=0; i<pocetradku; i++) 
      for (i1=0; i1<XPMBLOCK; i1++) {
	fprintf (outfile, "\"");
	for (j=0; j<pocetsloupcu; j++) {
	  b = jakabarva (i, j);
	  switch (troj[b][0]) {
	  case 0:    for (j1=0; j1<XPMBLOCK; j1++) fprintf (outfile, "%c", barva[b]);
	             break;
	  case '/':  for (j1=0; j1<XPMBLOCK-i1; j1++)
	               fprintf (outfile, "%c", troj[b][1]);
	             for (j1=XPMBLOCK-i1; j1<XPMBLOCK; j1++)
		       fprintf (outfile, "%c", troj[b][2]);
		     break;
	  case '\\': for (j1=0; j1<i1; j1++) 
	               fprintf (outfile, "%c", troj[b][1]);
	             for (j1=i1; j1<XPMBLOCK; j1++)
		       fprintf (outfile, "%c", troj[b][2]);
	  }
	}
	fprintf (outfile, "\",\n");
      }
  else
    for (i=0; i<pocetradku; i++) {
      fprintf (outfile, "\"");
      for (j=0; j<pocetsloupcu; j++) 
	fprintf (outfile, "%c", barva[jakabarva (i,j)]);
      fprintf (outfile, "\",\n");
    }
finish:
  fprintf (outfile, "};\n");
  filename [k] = '.';
  if (fclose (outfile) != 0) {
    fprintf (stderr, "The file %s cannot be closed\n", filename);
    return;
  }
  if (llevel>=2) 
    printf ("\nThe graphic image of the solution is saved in: %s\n", filename);
}

/* [genbarva] vygeneruje pismeno pro potreby XPM, ktere jeste neni v seznamu
   barva[] pouzito. Pokud se to nepodari, vrati nulu. 
   */

char genbarva (int kmax)
{
  char j;
  int k;

  for (j='A'; j < 'z'; j++) {
    for (k=0; k<kmax; k++)  if (j==barva[k]) break;
    if (k==kmax) return j;
  }
  return 0;
}

/* [si] Pomoci hodnot sil, sim, siw, sih a ssiw budeme kreslit do XPM
   trojuhelniky z triddlers. Tyto hodnoty popisuji jeden blok zahrnujici zleva
   pulku trojuhelniku stojiciho na spicce, dale jeden trojuhelnik stojici 
   na zakladne a zprava pulku trojuhelniku stojiciho na spicce:

     sil sim
     |    #    | 
     |    ##   |  sil[i] ... pocet pixelu leveho trojuhelnika na radku i
     |   ###   |  sim[i] ... pocet pixelu prostredniho trojuhelnika na radku i  
     |   ####  |  
     |  #####  |  siw    ... celkova sirka bloku
     |  ###### |  sih    ... pocet radku bloku
     | ####### |  ssiw   ... polovicni siw, udava, o kolik pixelu doleva se 
     | ########|             mame posunout pri tisku dalsi rady trojuhelniku

   Vzhledem k tomu, ze pro presny rovnostranny trojuhelnik plati
   sih/siw = sqrt(3)/2 ~= 0.8660254, prichazeji v uvahu pomery
   sih/siw = 7/8 = 0.875 nebo sih/siw = 8/9 ~= 0.88888 nebo  
    sih/siw = 6/7 = 12/14 ~= 0.8571.

   Rozhodli jsme se pro nejmensi variantu 7/8, protoze dobre aproximuje
   rovnoramenny trojuhelnik a navic siw je sude, takze odpada dilema,
   jak volit ssiw.

   Pro ilustraci zde v komentari udavame udaje, jak by trojuhelniky 
   vypadaly ve variante 8/9:

   char sil[] = {4,4,3,3,2,2,1,1};
   char sim[] = {1,2,3,4,5,6,7,8};
   int  siw = 9, sih = 8, ssiw = 4;
   
   a ve variante 12/14:

   char sil[] = {7, 7, 6, 5, 5, 4, 4, 3, 2, 2, 1, 1};
   char sim[] = {1, 2, 3, 5, 5, 7, 7, 9,10,11,12,13};
   int siw = 14, sih = 12, ssiw = 7;
   */

char sil[] = {4, 3, 3, 2, 2, 1, 1};
char sim[] = {1, 3, 3, 5, 5, 7, 7};
int siw = 8, sih = 7, ssiw = 4;

int  si;  /* cislo prave zpracovavaneho radku, si = 0..sih-1 */

/* [tridsavexpm] nahrazuje cast funkce savexpm() pro pripad triddlers.
   */

void tridsavexpm (FILE *f)     /* tisk XPM */
{
  int i, j, t;
  trset(E);          /* vypocet sirky triddleru */
  if (E<A) t = (A-E)*ssiw;
  else     t = (E-A)*ssiw;
  if (tr0%2) tr0--;
  if (tr1%2) tr1--;
  t += ((tr1-tr0)/2 + 1) * siw;
  for (i=0; i<pocetbarev; i++) if (barvakolem == barva[i]) break;
  if (i<pocetbarev) { /* znak pro barvu kolem je kolizi s nejakou jinou barvou */
    barvakolem = genbarva (pocetbarev);
    if (barvakolem==0) error ("the next character for outside color not found");
  }
  fprintf (f, "\"%d %d %d 1\",\n", t, (A+B)*sih + sih/2 + sih/2, pocetbarev+1);
  fprintf (f, "\"%c c %s\",\n", barvakolem, jmenobarvy[MAXBAREV]);
  for (i=0; i<pocetbarev; i++)  
    fprintf (f, "\"%c c %s\",\n", barva[i], jmenobarvy[i]);
  for (i=0; i<sih/2; i++) {
    fprintf (f, "\"");  rprint (f, t, barvakolem);  fprintf (f, "\",\n");
  }
  for (i=0; i<A+B; i++) for (si=0; si<sih; si++) {
    trset (i);
    fprintf (f, "\"");
    rp = 0;
    if (i<A) rprint (f, (A-i)*ssiw, barvakolem);
    else     rprint (f, (i-A)*ssiw, barvakolem);
    j = tr0; 
    if (j%2) j--;
    for (; j<=tr1; j+=2) {
      pprint (f, i, j-1, sil[si]); pprint (f, i, j, sim[si]); 
      pprint (f, i, j+1, siw-sil[si]-sim[si]);
    }
    rprint (f, t-rp, barvakolem);
    fprintf (f, "\",\n");
  }
  for (i=0; i<sih/2; i++) {
    fprintf (f, "\"");  rprint (f, t, barvakolem);  fprintf (f, "\",\n");
  }
}

/* [pprint] je pomocna funkce funkci tridsavexpm(). Tiskne k-krat
   znak, ktery vyzvedne z pole[][] z i-teho radku a j-teho sloupce.
   Pokud je j vlevo od prvniho sloupce nebo vpravo od posledniho, pak
   tiskne k-krat barvu okoli.
   */

void pprint (FILE *f, int i, int j, int k)
{
  if (j < tr0 || j >= tr1) rprint (f, k, barvakolem);
  else                     rprint (f, k, barva[jakabarva (i,j)]);
}


/* [system] Nasleduji bezne funkce na ziskani prostredku od systemu.
   Chceme, aby program zkolaboval ve vlastni rezii. Nechceme, aby byl
   program odstrelen systemem s nic nerikajicim "segmentation fault".
   */

void * getmemory (long int i, char *name)
{
  void *v;
  v = malloc (i);
  if (v==NULL) {
    fprintf (stderr, "malloc() failed during %s[%ld] initialisation.\n", name, i);
    exit (12);
  }
  return v;
}

FILE* openfile (char *name) 
{
  FILE *v;

  if (name[0] == '-' && name[1] == 0) return stdin;
  if ((v = fopen (name, "r")) == NULL) {
    fprintf (stderr, "cannot open the file %s to reading\n", name);
    exit (11);
  }
  return v;
}

