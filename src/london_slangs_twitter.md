---
title: "London gang slangs on Twitter"
author: "Sefa Ozalp"
date: "30/07/2018"
output:
  html_document:
    toc: true # table of content true
    toc_depth: 2  # upto three depths of headings (specified by #, ## and ###)
    number_sections: true  ## if you want number sections at each table header
    theme: united
    keep_md: yes
---


```r
# install.packages("rtweet")
library(tidyverse)
library(rtweet)
library(xml2)
library(memoise)
options(width=120)
knitr::opts_chunk$set( warning = FALSE, message = FALSE)
```

# TL/DR:

A quick markdown file to search London gang slang on Twitter. 


**Rationale**: The idea is to see whether these gang slang words produce sensible results from a simple twitter search in terms of references to crime and violent gang culture.

**Involves:**


1. Data scraping from multiple sources (one html, one pdf)
2. Data wrangling (duh)
3. Querying from Twitter search API
4. Memoise API call (not implemented, using manual caching)

**Sources:** Gang slang terms used in this document are taken from.

1. https://www.shinobilifeonline.com/index.php?topic=2973.0

2. https://bura.brunel.ac.uk/bitstream/2438/14817/1/FulltextThesis.pdf

    
**End Result:** 

1. A list of London gang slang terms with tweets containing those terms and comments whether they are useful for this study or not https://github.com/sefabey/fear_of_crime_paper/blob/master/data/slangs_from_shinobi.csv

2. 

# Gang slang from shinobilifeonline.com

##Srape keywords from a web page using Rvest

Scraping data from: https://www.shinobilifeonline.com/index.php?topic=2973.0. Used selectorgadget to identify xpath. This code scrapes all slang terms in the web page and returns a list consisting of **120** slang terms.


```r
html01 <- "https://www.shinobilifeonline.com/index.php?topic=2973.0"


slangs <- rvest::html(html01) %>% 
    rvest::html_nodes( xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "inner", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "bbc_size", " " ))]') %>% 
    rvest::html_text() %>% 
    as.tibble() %>% 
    mutate(slang_term=value) %>% 
    mutate(letter_count=str_count(slang_term)) %>% 
    filter(letter_count>1) %>% 
    # mutate(double_words= case_when( str_detect(slang_term, "/") ~ "yes",TRUE ~ "no")) %>% #no need to do this, separate_rows works as expected
    separate_rows(slang_term, sep = "/") %>% 
    distinct(slang_term,.keep_all = T) %>% 
    filter(!row_number()==1) %>% #drop the first row which was a header 
    mutate(slang_term_lower= str_to_lower(slang_term)) %>% 
    select(slang_term,slang_term_lower)

slangs <- slangs %>% 
    mutate(slang_term_query= paste0('\"',slangs$slang_term_lower,'\"'))

# wrote this slangs data to file just in case
```

##Scrape tweets using rtweet

Using SEARCH API, I queried 50 tweets for each term in the slang list. Apparently, not every slang term returned 50 tweets (some are really obscure and uncommon terms/spellings). Ultimately, this query resulted in a dataframe consisting of 5604 rows.


I am using below chunk for ~~memoising and/or caching purposes~~ future reference only. I wrote query results to a csv file and I will be working with that (otherwise I need to query twitter API every time I knit the rmd. This is impractical as it (1) returns different results each time, (2) twitter rate limits are pain). Therefore, not evaluating below chunk at all. 



```r
slangs <- slangs %>% 
    mutate(slang_term_query= paste0('\"',slangs$slang_term_lower,'\"'))

slang_tweets <- purrr::map_df(.x = slangs$slang_term_query, .f =  rtweet::search_tweets, 
                              n = 50, include_rts=F)

rate_limit() %>% 
    arrange(reset)

slang_tweets %>% rtweet::write_as_csv("slang_tweets.csv")
```


read slang tweets from csv

```r
slang_tweets <- rtweet::read_twitter_csv("../data/slang_tweets.csv")
```


## Exploring Tweets Matching keywords from Slangs list

Below, I will define and use a function that (1)finds tweets (scraped previously) which match nth term from the slang list, (2) randomly sample 20 tweets matching nth term, (3) print tweet text.

Then, I will read these tweets and try to get a sense of what they refer to. I will print the tweets first and then add my comments. Since this is quite repetitive, I will do this for first 40 terms. See the complete list for all terms  [https://github.com/sefabey/fear_of_crime_paper/blob/master/data/slangs_from_shinobi.csv]


*Note for persons with a keen eye:* The reason for using double distinct in the chunk below is, some slang terms returned less than 20 results so the chunk was throwing an error when using `sample_n(20)`. Thus had to do `distinct`, sample 20 with replacement and then do `distinct` again (for cases where unique n<20). I could have tackled this more elegantly (by dropping first `distinct` and using `sample_n` with replacement and then `disctinct`after that) but since I was using cache=T the some chunks, I was in too deep and I opted to carry on with not so elegant code. 


```r
print_slang_tweets <- function(n) {
    slang_tweets %>% 
        select(text) %>% 
        filter(str_detect(slang_tweets$text, pattern = regex (slangs$slang_term_lower[n],ignore_case = T))) %>% 
        distinct(text) %>% 
        sample_n(20, replace = T) %>% 
        distinct(text)
        }    
```

### 1) Term: **Jump Out Gang**

```r
print_slang_tweets(1) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"NEW banger !!!!🚗🏠👣🔫🔫🔫 \"Jump Out Gang\" available Now \\nhttps://t.co/zhLuuDjkUo\\n#iTunes #NowPlaying #HIPHOP #Spotify #IHEARTRADIO #SouthernCharm #ThursdayThoughts"},{"1":"Knee deep in them streets with nothing to say, I ain't playing no fucking games\\nI was with my niggas, you know they with it, ain't hang with no fucking lames\\nDon't drop our names, still jump out gang, we'll pull up bust some brains"},{"1":"Jump out gang putting niggas on they shit he yelling out for help bro I'm hit..."},{"1":"@gmhoncho @guwopkev__ Lmao jump out gang 💯\\U0001f92b"},{"1":"Jump out gang gone get it ova wit 🤧"},{"1":"@OHamersky Jump out gang"},{"1":"banging out of Florida Ricky Ruger with this new single\" jump out gang\"\\nhttps://t.co/KGR7rVGU2A https://t.co/UoRKzywv7n"},{"1":"you bitches and niggas ain’t jump out gang lol cut the act"},{"1":"Hot new artist!!!!! banging out of Florida @Rickyruger904 with this new single\" jump out gang\"\\nhttps://t.co/j2TiBTqDxt"},{"1":"It def go up with jump out gang 🤣🤣🤼‍♀️🤼‍♀️"},{"1":"Hot new artist!!!!! banging out of Florida Ricky Ruger with this new single\" jump out gang\"\\nhttps://t.co/KGR7rVGU2A"},{"1":"jump out gang putting niggas on their shit 4 me\\U0001f92b #Nocapp"},{"1":"T10 - Jump Out Gang pt.2 (Maine Musik Diss) [Shot by. Ronnie Tremblay] https://t.co/yXLcBLhuoo"},{"1":"Hot new artist!!!!! banging out of Florida Ricky Ruger with this new single\" jump out gang\"\\nhttps://t.co/j2TiBTqDxt https://t.co/ZhR7BZGVwe"},{"1":"Hot new artist!!!!! banging out of Florida Ricky Ruger with this new single\" jump out gang\" #MusicMonday\\n#Fortnite\\n#NowPlaying #Florida\\nhttps://t.co/j2TiBTqDxt"},{"1":"Listen to Ricky Ruger- Jump Out Gang by rick ruger #hotshit on #SoundCloud #WednesdayMotivation\\n#hiphop #Floridaboy #gang #gangshit\\nhttps://t.co/BHlupI10DW"},{"1":"I liked a @YouTube video https://t.co/erojbOWwmN T10 - Jump Out Gang pt.2 (Maine Musik Diss) [Shot by. Ronnie Tremblay]"},{"1":"@Caitlyn_joyce_m Jump out gang"},{"1":"Listen to Ricky Ruger- Jump Out Gang by rick ruger #SundayFunday on #SoundCloud #newbanger\\n#NowPlaying #floridaartists  https://t.co/cTlBqLZERH"},{"1":"Jump out gang ✏️"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** Term used to query twitter API was **Jump Out Gang**. Tweets reference to a band called jump out gang. Not very useful for studying further.


### 2) Term: **67**

```r
print_slang_tweets(2) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@trulyhae 67 wish me luck 🙏🙏🙏"},{"1":"@ExtremeConsoles @IanSanders_1981 @gamesyouloved @JuicyGameReview @16bitnostalgia @ColonelFalcon @masonicgamer @16bitdadblog @RetroGamerDaz @Malchedael_67 @Retr0Joe This is awesome!"},{"1":"@Dcurran37367656 Its getting too far i must agree. I heard they have banned some kiddy shows like noddy because he is a boy and it may put pressure on preschool kids who may feel trapped in the wrong gender, so as a fair approach genderless caracters are more appt. ffs world gone mad."},{"1":"1 Hour until US close\\nDOW -60pt\\nNAS +27pt\\n#Gold -$6 to US$1,227/oz\\n#Oil -1.3% to US$67.8/bl\\n#ironore -$1.1 to US$66.45/t\\nUS 10 Yr Yield 3.0%\\nSPI -30pts\\nA/US$0.7401\\n#ausbiz"},{"1":"@JosefAdd @minogaamokwe @gbobke @Jimbo679 @stephenlautens @ZoeIsLovely 1 guy with a dumb sign &amp; 1 guy yelling but wrong neighbourhood/wrong crowd -everyone there clapped when this a$$ was escorted away. 🇨🇦❤️🙏🏻#danforthstrong."},{"1":"@FutboArenas1905 @Alfaa_67 Hiç değilse gomis ve rodriguesi pazarlayabilmemiz lazım ki maicon falan da itelenebilir maaşı abes değil"},{"1":"@8bitToNow @Kuniotchi @FrappMocha @Gothweet @JonathanBlunn @BloggosPow @Malchedael_67 @RetroBoyJon Awesome man! Yeah that was the one I was thinking of. Now it's on my most wanted list! Cheers bro!"},{"1":"@IvyShoots @baker_russell @naomigallego @MnemoniXs @Watt_N_Idiot @Vickie627 @rcasonr @AndersEigen @Rapture571 @Happywife151 @HeidiM_67 @eagle1776n @funnyhaha444 @StephanieJMajor @LisaWinslow @JRMilward @KateofLate8 @Heather4amazon @solsukut @WildChild69 @TimW3811 @MollyBrown28 @Stephen_Faris @Badababa @SylvieDParris @AtheistEngineer @MaryAnn59685931 @Ah_Science @periwinklewidow @rpfregeau @BlancheMonique1 @Ornery_Opinions @manny_manatee2 @postordinary @leisure3000 @bizzimomma2 @pixelprotectors @NARAL @JuliBunting @GeniusPhx @morrigansarcher Do you know anybody who does?"},{"1":"Who has been doing the amazing Manchester bee trail this week? Look out for Oldfield Brow primary's beautiful 'Crocus Bee' located in the Manchester museum. (no.67 on the map) @awah_ofbrow_bee @Manchester_Bees #proud #primaryschool @McrMuseum https://t.co/M5GREgYkkZ"},{"1":"@coffeecup6891 @HeidiM_67 @baker_russell @naomigallego @MnemoniXs @Watt_N_Idiot @Vickie627 @rcasonr @AndersEigen @Rapture571 @Happywife151 @eagle1776n @funnyhaha444 @StephanieJMajor @LisaWinslow @JRMilward @KateofLate8 @Heather4amazon @solsukut @WildChild69 @TimW3811 @MollyBrown28 @Stephen_Faris @IvyShoots @Badababa @SylvieDParris @AtheistEngineer @MaryAnn59685931 @Ah_Science @periwinklewidow @rpfregeau @BlancheMonique1 @Ornery_Opinions @manny_manatee2 @postordinary @leisure3000 @bizzimomma2 @pixelprotectors @NARAL @JuliBunting @GeniusPhx @morrigansarcher Right I forgot she did that lol."},{"1":"@Bamfxoxo Not3s, Chip, stormzy, dappy, wretch 32, avelino, mist, Aj tracey and jhus, these lot put bangers out. Ill put a few drill artist too: Fredo (sick), AM x Skengdo, Loski (most wanted in london ppl say). 67, Harlem spartans."},{"1":"@ElTecnico67 @TheFeoAdrian He need a splash guard"},{"1":"August is designated as Children’s Eye Health and Safety Month!  NJ Diet cares about the overall health of the individual.  Jennifer is 67 yo, on day 26, down 25 lbs. She is sleeping through the entire night for the first time in years!\\n#WednesdayWisdom \\nhttps://t.co/KsWXxNzbJD"},{"1":"🇬🇭#Ayekoo NP▶️#Sheldon ft @KuamiEugene 🎶That thing 🎧@djadvicer10 📻@HappyFMGhana @MbBuabeng @BlackBillsGh @OneStoryOnly @BookMeWorld @kabuteyamevor @EugeneVidzro @obee_dj @TKarikari @mac_okocha #Ayekoo #HappyGhana #Dontpushme https://t.co/mcm7967WGz"},{"1":"@Iitbieb 67"},{"1":"Crude oil futures settle at $67.66 https://t.co/0Ti6obqyg7 #OOTT 🛢checkout https://t.co/lxJXcpDVii to stay up2date https://t.co/x9pO43HgXo"},{"1":"Prefeitura recebeu R$ 3.420.932,67 e alega não ter dinheiro para pagar Servidores da Educação https://t.co/N4oXaJhTlp via @wordpressdotcom"},{"1":"Wed 15:00: Temp 26.2 C; Humidex 33; Wind SSE 13 km/h; Humidity 67%; Press 101.4 kPa / falling."},{"1":"💦💎0536 777 15 67💭 💦💎\\n 💭 🍻slm beyler 19 yaşındayım™➿%💯sexsi ve ⭐güzel ❇çekici bir bayanım\\n\\n#istanbulescort #türkporno #amciksikmek #beylikdüzüescort #beykozescort #liseliifşa #amyalama #amciksikmek #türkporno #türksex #avcılarescort #bursaescort #beşiktaşescort https://t.co/4AqxHplCM1"},{"1":"@dOsUNmU_67 Lol we both know Allegri will play him in midfield 😂"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **67**. As the term is an integer, tweets can refer to anything. Not very useful for studying further.


### 3) Term: **86**

```r
print_slang_tweets(3) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"0309hrs Awas! treler rosak di km 86.1 dari Yong Peng - Ayer Hitam. Lorong kiri terhalang. Trafik terkawal. @LLMinfotrafik"},{"1":"@AnaCons5 Mi juego preferido, de hecho para el terremoto del 86 estaba haciendo varias tortitas de barro cuando vieno el remezón.."},{"1":"@Sassy_dotty @jet186 Seriously considering it..."},{"1":"@matthewsyed @GeraintThomas86 As I tweeted earlier a great article - certainly has people talking and opinions aired. Well deserved victory and as a Sunday cyclist whose ride fuelling consists of scones and flavoured lattes I applaud you. Sacrifice and hard work to achieve. 👏👍"},{"1":"@m__batkovic Apa druzxe __miki__86 si i dalje ne lazxi sad sam gledao"},{"1":"Current Conditions: Partly Cloudy with a temperature of 86 °F. #ncwx"},{"1":"@MSufiSaint @CMOGuj @vijayrupanibjp @PMOIndia @narendramodi @JigarBDesaiBjp @AmitThakerBJP @VrushantM @ravisinha_86 @LUVSUNEEL @zahidpatka @DanishTrannum @fozia_zulfiquar @Khalid28590696 @yunustalat गुजरात राज्य के मुख्यमंत्री श्री @vijayrupanibjp जी को जन्मदिन की हार्दिक शुभकामनाएं। ईश्वर से आपके उत्तम स्वास्थ्य, दीर्घायु और यशस्वी जीवन की कामना करता हूँ। @CMOGuj @DarshanaJardosh @CRPaatil @zankhanabenbjp @MLASangitaPatil https://t.co/ACoqdcUh4W https://t.co/TNOQY8RdaP"},{"1":"Individual scores for @ZvilleGirlsGolf: Annabelle Pancake 74, Abby Thielbar 76, Lauren Kaltenmark 78, Avery VonDielingen 83, Ellie Hine 86"},{"1":"@hugh_bothwell @GerbusJames @badibulgator @tarawasjesus @forthemasses @rmbctious @MechaPanda9K @keigh_see @alastairjallen1 @ExSapperBadMan @readyornotfory2 @fruitchicken1 @Geek_0nline @Tinman_73 @TheCarp86835734 @doctorbuttons @IMHO__2017 @Brian_Kitchener @RunningHippo @Foulkesy1 @nicholaspitts @wiguy45 @aaronsburrell @Watsdecraicjmac @bikinatroll @Shadowzerg @CollinOctantis @YahushuaIsGod @IEAffiliate @phead54693153 @atheistProgress @cbridger954 @CivilGunOwner @DeanKo @jon_hill987 @Blue_Ouija @KayeTatton @Nutt007 @msimmons872 @EMactions @Skiing_Gator @whitneyrhodasma @FlatSlugbrains @CrustyDemon999 @frenchie_myriam @WycheNick @captscorch @DansMonkeyShack @zankman1 @BasqueTerra Okay, using these four images, what is the horizon dip? Be careful, it's a trick question ;) infrared used to see through the pollution. https://t.co/JuYzJlMiOt"},{"1":"@HugS86 @HyperX if you could turn any part of your body into any food item, what food and which part"},{"1":"@PrincessaPinot @elcholo1923 @PBMMW @CalifWines_US @PeterRanscombe1 @moevino @eddiewat @CalifWinesUK_IR @PCWineAwards @SigneSJohansen @davidc1863 Sorry that was click bait!"},{"1":"@Kholu24 @ix_86 6"},{"1":"El Govern pagarà contractes laborals de 1.000 euros a 86 immigrants en situació irregular però amb un arrelament https://t.co/7GbHQBzexu"},{"1":"86. В автобусе перед игрой люди делятся на два типа:\\n1. Кто настраивается на игру \\n2. Кто едет красиво покататься , чтобы их тёлочки сфоткали"},{"1":"@GonzaLillo_86 Llegamos al siglo XXI! 👏👏👏👏"},{"1":"I added a video to a @YouTube playlist https://t.co/86PXTU0FbV this is rushed"},{"1":"15:15h Temperature: 86.2°F, Wind:0.0 mph SSW, Rain Rate:0.0 in./hr."},{"1":"Another Mexican classic for lunch. Carrots, broccoli, mash, cauliflower gratins, New York strip steak, pumpkin stuffed with ham and cheese, and some sort of sweetish bread roll. https://t.co/H86Dfyglxz"},{"1":"Students like to blame ASB as if it’s their fault. They can only do what ADMIN allows. They have so many restrictions and they really do try to make the student body happy but they can’t do what they wants because admin won’t allow it. I get where you’re coming from but still https://t.co/aC386kGjQl"},{"1":"\"Mi pregunta es sobre el herpes-zóster, tengo un familiar de 86 años (...) \\nTus consultas online sobre #salud de forma fiable, gratuita, segura y accesible https://t.co/rmX6y1z26z https://t.co/UVevTm1nc7"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **86**. Similar to 67 above, as the term is an integer, tweets can refer to anything. Random topics observed. Not very useful for studying further.


### 4) Term: **Harlem Spartans**

```r
print_slang_tweets(4) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"Spotify and Apple selling violent drill music of rap gang Harlem Spartans just days after member was stabbed to death - The Sun https://t.co/sFI3UkXR8M https://t.co/0oZfotyup4"},{"1":"🚨**🆕** Producer Steel Bangelz enlists Harlem Spartans rapper Loski for his track/Vide Video🎬🎥 – ‘Hot#UKRap #UKHipHop 👇 https://t.co/zGo73IkNgt"},{"1":"RIP SA (Harlem Spartans) everyone’s being taken too fucking young man"},{"1":"BAD APPLES Spotify and Apple selling banned violent drill music and tracks by rap group Harlem Spartans days after member was stabbed to death https://t.co/kPzSG0Amw8"},{"1":"📣 BIG LINK UP: 🚨**🆕** Producer Steel Bangelz enlists Harlem Spartans rapper Loski for his track/Vide Video🎬🎥 – ‘Hot#UKRap #UKHipHop 👇 https://t.co/zGo73IkNgt"},{"1":"Spotify and Apple selling banned violent drill music and tracks by rap group Harlem Spartans days after member was stabbed to death https://t.co/H2gWlfNY9C"},{"1":"I liked a @YouTube video https://t.co/aVHNljXcah Bis (Harlem Spartans) x Oboy (KuKu) - Money On My Mind [NEW] [AUDIO] | Slammer Media"},{"1":"Spotify and Apple selling banned violent drill music and tracks by rap group Harlem Spartans days after member was stabbed to death https://t.co/vlWNLDr6cq"},{"1":"Harlem Spartans Bands - hook on this is hard still not to forget Zico's verse 😅😅 https://t.co/57WS5fCVcz"},{"1":"@JudeHey_ Victim was SA(Splash Addict) aka Lattz from Harlem Spartans gang in Kennington, South LDN. Apparently he was the leader, he has made a few verses in drill music as well. A talented guy imo, shame he died at 18 😪."},{"1":"I saw this on The Sun app and thought you’d enjoy it\\n\\nSpotify and Apple selling banned violent drill music and tracks by rap group Harlem Spartans days after member was stabbed to death\\n\\nhttps://t.co/yiq0ThT7zh"},{"1":"Spotify and Apple selling banned violent music by rap group Harlem Spartans days after member was stabbed to death https://t.co/eNdgyXIAJT"},{"1":"Spotify And Apple Selling Violent Drill Music Of Rap Gang Harlem Spartans Just Days After Member Was Stabbed To Death https://t.co/0fcUry29yV"},{"1":"OOOOO TUNEEEEEE\\n\\nHere’s a song for you… Calling My Line (feat. Ay Em) by Harlem Spartans\\nhttps://t.co/fqxNWqP0Ps"},{"1":"I liked a @YouTube video https://t.co/rCZTTnEqFW Bis (Harlem Spartans) x Oboy (KuKu) - Money On My Mind [NEW] [AUDIO] | Slammer Media"},{"1":"@_Pengeth Excuse me pardon? Harlem Spartans"},{"1":"How have the locals found them💀 bts dancing to Harlem Spartans? I would have truly seen it all #BTSINLONDON #BTSINTHEUK https://t.co/MUsoWjKZAs"},{"1":"I added a video to a @YouTube playlist https://t.co/cmIu1Eu9pF [ SOLD ] Harlem Spartans x G Herbo (Trap/Drill) Type Beat \"GHOST\" |"},{"1":"I liked a @YouTube video https://t.co/CBCWktb65x AM (410) previews a new Harlem Spartans diss on his snap😨😲😬"},{"1":"The barbie in me jumped OUTT 🔫💕\\n\\nHere’s a song for you… Call Me a Spartan by Harlem Spartans\\nhttps://t.co/3Mr3795K9Z"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Harlem Spartans**. Observed references to a UK band called Harlem Spartans. HS are apparently banned for references to violence an UK gang culture in their music. Some references to violence (see the 3rd tweet for instance). Maybe useful for studying further.

### 5) Term: **Ounto Nation**

```r
print_slang_tweets(5) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"I liked a @YouTube video https://t.co/PXylCAmc2K (Ounto Nation) Poppy - Pops Ya Ready [Music Video] | GRM Daily"},{"1":"I liked a @YouTube video https://t.co/zJDRFBJH5S Poppy - Supply (Ounto Nation) [Music Video] | GRM Daily"},{"1":"@LUCASTORRElRA @AwinoE5 @RoIeNine Ounto Nation, ffs I use to vibes to abra different back in like 2016 ain’t listened to him in like a year or so"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Ounto Nation**. Returned only 3 tweets, which reference to YouTube videos. Not much sentiment observed. Not very useful for studying further. 

### 6) Term: **Aggy**

```r
print_slang_tweets(6) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"Y’all aggy as fuck https://t.co/UsGpz1PSpI"},{"1":"@xborisr they aggy as hell."},{"1":"I can’t be 40 and still dating! Shit aggy asf already!"},{"1":"@missalikhan096 Theek hai woo bilkul.. awain sb ko aggy laga rakha tha uss nay"},{"1":"@Courtney_POW aggy 😂 I’m fine. How are you?"},{"1":"Why isn’t the aggy rabbit in the #ChristopherRobin film? He was the most relatable."},{"1":"@LongHornFrenzy Add hypocrisy to the list of aggy “core values”. Morons..."},{"1":"@_hurtsa You’re in the thread too aggy"},{"1":"bitches that stay pressed over the next mf getting plastic surgery are so aggy like girl please take a breather"},{"1":"And if one more fucking nurse open something and not put a bag clip on it ima be aggy when tired of opening these shared snacks and they stale as hell 😑"},{"1":"On leave until this album drops, y’all mad aggy! I will continue streaming #FEFE, #BED, #RichSex and #ChunLi while I’m away. https://t.co/5afsOEHJLN"},{"1":"See the way today is set up, soon as I get home from the gym, I’m showering, smoking with bae and kissing on him till he calls me aggy. Light the candles and cuddle and watch tv till we fall asleep"},{"1":"Wish my parents would fuck off back on holiday they are aggy with me for no reason at all"},{"1":"@CLONG4REAL_ not aw😒 he aggy lol"},{"1":"I need like a full two session so I can sleep peacefully and come to work refreshed cuz ya girl is aggy today"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Aggy**. No mention of crime but used to express negative sentiments. Maybe useful for detecting emotions but not references to crime.


### 7) Term: **Aggro**

```r
print_slang_tweets(7) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@FlaucHA2 Je joue aggro sans ça aussi"},{"1":"For fans of: aggro YPIPO getting tuned up after saying racist shit https://t.co/uzoKmdadAK"},{"1":"@NuggGramz @TSM_Daequan If you get one shot by a noob you made a bad play. You either played to aggro and underestimated him commited to hard and jumped into his 1x1 or over peaked. All you have to do to not get one pumped by noobs is play safe."},{"1":"Why has no tank ever enlightened to tell me that regening gives aggro"},{"1":"@viniciuscu com certeza, mas o problema e que magic, fazer um deck do '' meta '' não fica por menos de 500R$ se ainda for um aggro insano ali, e depois já troca a season e muda as cartas"},{"1":"@N7xEmma @skoryss_lol because while stopwatch prevents dives, it also creates very easy dives, which is okay but i think its way too forgiving in terms of mistakes, if u dont dive correctly, stopwatch will save you bcs it resets aggro and makes u immune.\\n\\nso stopwatch is more of a \"mistake eater\""},{"1":"@DeirdreKoala I must admit I panicked for a moment when I got it cuz it's just so aggro lol"},{"1":"@StanCifka This is sick!!!! Aggro druid is about to be lit!"},{"1":"@Tan_yaa_ Eish those things are aggro hey 😂 not that we ever actually got bitten but 😂"},{"1":"@Aggro_tK @Cameron_2523 @CallofDuty  https://t.co/hCAKwOIXEr"},{"1":"United aggro https://t.co/pN0GpAJoid"},{"1":"@PaulBagz it’s just all over healing so regen between pulls slowly ticks up your healer aggro since they’re healed to full"},{"1":"1 mana 1/3 good for aggro decks, plus works as a single-card enabler for the happy ghoul package, so you don't have to be Warlock to use it consistently."},{"1":"#DailyAstro #AstroWeather Aug 1 2018:\\n\\nVesta stations direct, the Moon enters Mars-ruled Aries, and retrograde Mars squares Uranus.\\n\\nIs it worth it to cast off that thing, that person? Your life is your own, but accidents and aggro happen. A voice whispers: Remember the Mission. https://t.co/YDJt2YRLdP"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Aggro**. Very similar to aggy. No mention of crime but used to express negative sentiments. Maybe useful for detecting emotions but not references to crime.


### 8) Term: **Amm**

```r
print_slang_tweets(8) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@anderslindberg @AB_Karin Alltså har Aftonbladet \"nazistiska rötter\", av exakt samma slag som S brukar tillskriva Sd - där det faktum att malten över tidningen/partiet övergått till personer med annan ideologi saknar betydelse."},{"1":"@Amm_Dzik Ily ❤️❤️❤️"},{"1":"Bis (Harlem Spartans) x Oboy (KuKu) - Money On My Mind [NEW] [AUDIO] | Slammer Media: https://t.co/Qhky3fmos3 via @YouTube"},{"1":"@freeeky @sivanosoroginja amm .. ti v petkah? :D picz"},{"1":"@jhumbertojuarez @soyMargarlto JAJAJAJAJAJAJAJAJAAJJAJAJAJA AMM NO?"},{"1":"Happy first day of August! Traditionally, August 1st it marks the beginning of  harvesting the corn and wheat crops. It has been known as Lammas Day! Where did the word \"Lammas\" come from? Read more about this observance day! #HelloAugust\\nhttps://t.co/nfaRXIpned https://t.co/pyJuJ234dG"},{"1":"@amm__ar اها شكراً على توصيل الفكرة..\\nظننتها ( لا تغامر )"},{"1":"@Asad_Umar Aslaam alyekam,\\nAsad bhi ager 8 hazar arab ka tax akthha kerna hay to amm shopes ko bhi tax net main lain maslan,jota,mithhaee,kaprra,sonar,hardware.........wagera.es terha ki dosri shpes bhi.please es ko search karain inshallah ap kamyab hon gay 8 hazar arab."},{"1":"LIVE NOW FAM! #Twitch #WatchMixer #StreamME and #YouTubeLive https://t.co/fR2FoptTul https://t.co/ZCExWgBDbP https://t.co/Lr5KcdNG7k @JosheLyle @GidgitBaby @VPGamingPower @SupStreamers @TheYTForum #SupportSmallStreamers #SmallYouTuberARMY #ClutchNation"},{"1":"VIGLIACCHI SONO GLI STESSI CHE PRIMA LI HANNO ROVINATI E SUICIDATI I LAVORATORI, I PADRI DI FAMIGLIA, DEVONO RISARCIRE LE FAMIGLIE ROVINATE DI TASCHE LORO E SE CI SARANNO ESTREMI E' GIUSTO E SACROSANTO CHE VADANO IN GALERA, LORO ERANO INCARICATI DI PROTEGGERE IL POPOLO NON DI AMM https://t.co/mFkvSw7gFd"},{"1":"@Rammer34 I know you’ve heard it already the poke check needs to be fixed. Also camera classic or ice can’t remember is unplayable due to the scoreboard being in the way on one side of the ice surface. Other than that good strides still disappointed no frostbite engine though :("},{"1":"@HeathKatherine @the @JGWelcometomex @PTeach111 @Glorygirlone @Holley_Wammack @GitnerDebbie  https://t.co/44GWZpUETL"},{"1":"@HALKA_MGK_AMM 瞳に焼き付いたのはアナタとアノコの笑顔"},{"1":"@amm__ar عفواً ما معنى  ( لا تقامر؟ )"},{"1":"@HALKA_MGK_AMM 綺麗事はいらない　ただ貴方だけいれば…"},{"1":"@Ayakakarkakar2 @Mushtaq87363552 @Khadimhussain4 Aw aw khpl numuno la sirf pa zorr building rang roghan oki aw takhta pe olagai che chata mur pa amm awam ke begharta owai nur ba sa na wayam lalteen asm ------ nir drta khpla pata da😂😂"},{"1":"@HALKA_MGK_AMM 瞳　貴方を見つめるためにある"},{"1":"All great recipes have complementary flavors. This creamy corn pasta checks all of the boxes. This recipe uses a few different corn varieties, but you can use whatever is on hand. Get the recipe here: https://t.co/Int09CvADk Sponsored by @BadiaSpices and @HammerStahl https://t.co/7j0fcSWz2m"},{"1":"Kucoin gives out dividends for holding Kucoin Shares $KCS 🚀  \\n\\nRegister for BONUS ➡️ https://t.co/GeKhg4nwh4\\n\\n$BHC $BNTY $BPT $ETN $MONA $ECC $BYC $RFR $MAN $BBR $EDO $QWARK $MKR $TRST $MS $BTG $BAT $CLNS $SPANK $FLIXX $AUY $FUEL $PFE $FLDC $LINK $IXT $MYB $AMM 834 https://t.co/0n8OD6APXw"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Amm**. Lots of foreign language tweets. not useful.



### 9) Term: **Ahlie**

```r
print_slang_tweets(9) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@DrclMPrz @AhliezaB Iyo na ahahahahaha kay ahlie lang talaga iyon"},{"1":"@_kishanmistry Ahlie gotta put him in rice now"},{"1":"@ImMvgic @LIVESTOCKcanada @ReianaR3 @kevantunes1 If you win those you know we're sharing them ahlie? If it's not like that ur a wastemon"},{"1":"honestly it’s funny how all the old people who forgot about you jump back in when you are up \\n\\nAhlie @calebagada33 \\n\\nCongrats on signing btw 💰"},{"1":"@DrclMPrz @AhliezaB Hahaha namiss ko na yung darie ahlie melai na tawagan hahahah"},{"1":"@Alaye84 Ahlie"},{"1":"Ahlie we all know that one guy 🤣🤣🤣🤣 https://t.co/zn2fbi2Hot"},{"1":"@dopeislissa Ahlie https://t.co/etGg1ickSx"},{"1":"@ChelsiGermain @SabSaudin Ahlie I’m tryna have tea for breakfast lunch and dinner"},{"1":"Empty promises ahlie 👻"},{"1":"@chantelrachael so ur using ur 2 weeks vacation on this ahlie ?"},{"1":"@ahlie_1937 اتوقع خواردو 👣 برا"},{"1":"@MiddleOfDaMap ahlie octavia did what she had to do and i support it 100% if anyone else did otherwise then bellamy would’ve came down to a bunker full of dead and rotting people. she saved them all"},{"1":"You just nyamed dat after the workout ahlie https://t.co/xxtxt7xcUE"},{"1":"@killyswhore Ahlie that location was bare mid styll Dx"},{"1":"@okalrightsar Ahlie"},{"1":"she got it. she got it. she got it.\\n\\nshe bad ahlie?"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Ahlie**. random topics observed. not suitable.


### 10) Term: **Allow it**

```r
print_slang_tweets(10) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@riotarchitect @Heybabalou @SteeOne @TherealdoctorA @ActifyMG @jrphoenix77 @Quiggling @LeftwardSwing @cdbrzezinski @danintheoutback @katalin_pota @briane2874 @bearingleft @cathyxOR @earlerichardsjr @hayley561girl @dgendvil @Roger02235695 @KenJones707 @hiya_jazz @ConsiderThis1 @TomStark88 @sallyodum @RijuColonDee @PaulDeCristofo4 @jesseawayne1 @Sea_Stories @keptitmoving @aarrrgggghhhhh3 @BarbaraLNewsome @Montgomery_bjm @tbw24431703 @shipman96 @ILikeRainWater @OldWhiteManLfty @superhotgrammy @LucyTreadwell01 @pdq8ball @EddieMarine1 @TodMoore3 @gjnmedia @SupaReaper @loraleatucker @cal5k @stuberry69 @GeriSpinney @Lee8772 @AyilFukUUp @ExposingALEC @Roger51189854 We have white people trying to tell us that we don't know racism when we hear it or \\nSince they believe we allow it out of one person we should allow it out of him\\nWhen they don't know anything about us or our history \\nThey only know the lies we watch bigots spread to each other https://t.co/5KtbmtLas9"},{"1":"Life is meaningless only if we allow it to be. Each of us has the power to give life meaning, to make our time and our bodies and our words into instruments of love and hope.\\n\\nENJOY THE JOURNEY \\n#quote #sayings #lifequotes #life #love #hope #malaysia #journaling #journal #journey https://t.co/PD7qNKrzVa"},{"1":"Every pain you experience gives a lesson and every lesson will change you - if you allow it to"},{"1":"@_MosesV23 @_Mxrtyy My hair grows rapid allow it"},{"1":"No. No, you don't. We do not allow it. https://t.co/7hH8YUqlzN"},{"1":"@SteveKingIA So here's a conundrum for you, since you're both a white supremacist and a pro-forced-birth bible-thumping fanatic: what if a black Muslim woman wants an abortion? Do you allow it in order to decrease the brown population? What would Jesus say (besides that you're evil &amp; insane)?"},{"1":"(I know it's pre-season but it's Arsenal, allow it)"},{"1":"This is the worst risk to the country and our security as it makes all of us look weak, especially our court system, our Intel operations, and the current adminstrators who allow it to go on.  FISA warrants should be looked into as to whether they are to be allowed in future. https://t.co/LeqyxvbHKn"},{"1":"@tomemrich Wow.  That's cool and opens the doors to some actual darn functional stuff if they allow it.  If they have an API that could like to an IF-This-Then-That type service it's going to be huge.  I need to talk to @FusedVR about this. :)  Cool demo Tom!"},{"1":"REMINDER:\\nThe small demons \\n(Depression, Anxiety etc.) \\nComes to break you down spiritually So the bigger demon can take over. \\nDO NOT ALLOW IT!!!!\\nYou gotta FIGHT!! \\nI know you’re tired but this is an Attack on your LIFE!!\\nKEEP FIGHTING!!!"},{"1":"@ADaniel26099125 @City_Press @JKwritingz @Julius_S_Malema @FloydShivambu @MbuyiseniNdlozi @EFFSouthAfrica @SAPoliceService You jailed Mandela and we will not allow it to happen with juju"},{"1":"@kipjmooney Long as this isnt robyn slander ill allow it. Shes a goddess"},{"1":"@Yasminexoxoxo Allow it"},{"1":"@BassamLFC She’s peng i’ll allow it"},{"1":"Students like to blame ASB as if it’s their fault. They can only do what ADMIN allows. They have so many restrictions and they really do try to make the student body happy but they can’t do what they wants because admin won’t allow it. I get where you’re coming from but still https://t.co/aC386kGjQl"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Allow it**. interestingly observed two instances of counter-speech but hard to discriminate normal use from slang use. not suitable.


### 11) Term: **Aired**

```r
print_slang_tweets(11) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"On this day in 1981, MTV first aired! When I was in 7th grade ('83), I won a black satin #MTV jacket in a local raffle (which I adorned with multiple #bandbuttons back then) &amp; became the talk of JCMS. I still have the jacket today! #happy37thanniversary #ilovethe80s #mtvvjs https://t.co/7aSAXZmDxH"},{"1":"@chivhon i recorded it when it aired a while ago, so far i haven't seen it uploaded anywhere"},{"1":"I’m so excited for venture brothers to be back.. I saw bits of the first season when it originally aired over a decade ago and then last year marathoned the whole damn thing"},{"1":"@EzekielMutua @KTNNews No,we cannot pretend or sugar-coat issue that are real,they should be aired and arouse emotions for action to be taken,that kenya !!!"},{"1":"@RonnieRadke Dude u do realize that u aired out someone's private information on social media. Posting someone's phone numbers so like wrong. Dude you're so in hot water with the owner of the number."},{"1":"Hard to believe but they used to play music videos on MTV!\\n\\nOn this day in 1981, the Music Television channel debuted.\\n\\nCan you name the song for first video they aired?\\n\\n#TodayInHistory #OnThisDay #PopCulture https://t.co/RPPsEyFOFQ"},{"1":"A twitter girl will treat hating capitalism like a personality trait but then tell you everything that happened on keeping up with the kardashians since that bitch aired"},{"1":"@rbmfish LOOOOOOL Do u rly fink I care bowt getting aired? I k ur busy and obvz I’m gonna show u. I think I have a pic of me wearing one of them"},{"1":"@tvfan4882 @RealitySteve @people Oops, their wedding was in Dana Pt., then ABC aired it on a Valentine’s Day Special"},{"1":"@PhilipJMilton @JamesCleverly So your saying an event that had a Auschwitz survivor as lead speaker is wrong? Corbyn said some of the views aired he disagreed with."},{"1":"@jaergon But the act is being propogated by both sides. CNN aired a 2 hour Town Hall full of vitriol towards 2nd amendment supporters, and maligned NRA supporters and a sitting US Senator as accessories to murder.  Was that problematic at all in your opinion?"},{"1":"@AliyahMiah1 Never. I’m the one who talks bares but always gets aired."},{"1":"@comcastcares Yes! Season 3 episode 10 ”of the T” aired July 31st, but on xfiniry it shows episode 9 and then 11 that’s to be aired next week"},{"1":"@IsibayaMzansi  it is disappointing to listen to some of the tribal comments that are being aired on your series. The level of disrespect is quite alarming!!!"},{"1":"@Balfagalma @NiWachera Yes was aired on KTN,"},{"1":"@SaveTimelessHQ @urlgrl @SPTV USA! Watched in on NBC as it aired"},{"1":"@cbcfifth \\nThe CBC fifth estate here Canada don’t be bias Please aired from my story .let the Canadian people heard how the GOVERNMENT CANADA Justice function."}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Aired**. Hard to discriminate whether slang or literal. Slang version not observed. not suitable.

### 12) Term: **Bare**

```r
print_slang_tweets(12) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"It’s free so maybe they’ll do the bare minimum 😂"},{"1":"The old bank account is looking bare after buying the new car, need this fiesta sold so a can sleep at night knowing I’m no skinto 😂"},{"1":"Teen 'ripped out his 74-year-old relative's eyes with his bare HANDS' https://t.co/iRwNBXPU1r Mahad Aziz charged only with 1st degree assault."},{"1":"Heeeeeee bathong 😂😂😂😂 a re \"bare modimo o phala baloi\" 😂😂😂😂 keore a sampe a itumetse abe a phamolwa jaana😂😂😂😂 #TheQueenMzansi https://t.co/EgdKkfnLSc"},{"1":"My sentiments exactly,&amp; in fact if you've been reading my tweets you would know I've said that about 10 times already today in fact. If Trump is clean mr. Mueller's investigation can only bare that out. I think I spelled bare wrong lol.. https://t.co/gwzlQOoOYM"},{"1":"@TaliaMar should do a video where she tries to teach @miniminter a load of gymnastics moves, it'd be bare funny 😂😂"},{"1":"@FaboMus @StringerAtlas Ahhh memories\\nUsed to try to ball out in Turnham Green &amp; Goals back in the day like I was SN8\\nBare tellin our United bredrins how Samir will be a better talisman for us than Çesc 😂"},{"1":"@KingKongCourts Bmt. They could tweet the most generic bullshit like “the sky is blue” and bare people will be ready to kiss ass for no reason whatsoever 😂😂😂"},{"1":"@eide_per @edelmariehurra SLUTT Å BRY DERE OM HVA ANDRE SIKKER DERE KAN DRUKKE SÅ FANCY TING DERE BARE VIL"},{"1":"“Allow dem bare tings” Zayn knows his roadman vocabulary https://t.co/ahGAkrjc5h"},{"1":"@CasValentin Han er så ulidelig at høre på, han lyder som mig da jeg var 10 og dommeren ikke gav BIF frispark og straffe konstant.  \"nå så kan dommeren ikke lide os, han er bare dårlig i dag, selvfølgelig bliver det ikke dømt når det er os\""},{"1":"@smokerinngs LUCKY TBH I CAN STILL BARELY READ"},{"1":"@BiKeR626 Bare uzonyathela amadimoni 🤣"},{"1":"Or “Bare gyaldem”"},{"1":"Here in #Colombia, the hypocrisy of western cocaine users is laid bare | Iman… https://t.co/uZrGyVi3oz #DSNWorld"},{"1":"He’s not watching We Bare Bears or  Clarence in this household. https://t.co/otuk0bVR6N"},{"1":"\"Life is more then an human eye can see, \" - @justinbareilles https://t.co/DFeZD9dVir https://t.co/5ZpIYewVjv"},{"1":"@amstray Men får han kred for det? Selvsagt ikke. Skal bare \"tas\"."},{"1":"@tm_migo Nigga I’m talkin bout bald bare face lmao"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Bare**. Hard to discriminate whether slang or literal. Rare to see slang used on Twitter. not suitable.

### 13) Term: **Bands**

```r
print_slang_tweets(13) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@BrianStack153 Yeah I love that song. I think Modern English is one of the most underrated post-punk bands... and for me, their poppy stuff like 'I Melt With You' is right up there with any of the best Echo &amp; The Bunnymen singles ~"},{"1":"I COMPLETELY stand with @PattyxWalters mental illness is fucking so important and I just don’t understand why other bands have to “stab” at each other. Also Patty is an angel so if you say anything bad about him it just shows  that ur insecure about yourself. That’s all."},{"1":"@_Bands_FC Will I embrace it?"},{"1":"Chaotic good: buying the bands merch before the show starts and holding throughout the set"},{"1":"In case anyone's interested, here's #11-50 of my top artists of 2018 after seven months. REPLETE with Brooklyn-based bands https://t.co/XBwawylw4e"},{"1":"Winding down the summer’s Warped cycle is sad, but also very cool because bands start announcing fall tours. 👌🏻"},{"1":"Bands in 2018: release every single song individually at random points before the album actually comes out"},{"1":"@AuctorLector Actually just to follow bands originally"},{"1":"@theneedledrop The Velvet Underground is one of the most influential bands ever, and they helped to create genres such as alternative rock, indie rock, goth rock, etc."},{"1":"had to cut all my bands off from Zante for netball and I’ve never felt so naked in my life :(("},{"1":"If the last one was a sink, this is very much a swim... @VerdantBrew Arm Bands Pale Ale. It's glowing! Big peachy fruit hit, with a slight bitter end. Standard. 🍻@GoodBeerTweet #Beeroclock #notasinkpour https://t.co/kbfGE6c0aX"},{"1":"Yeah my bands on me no lieeeeeeeeeee."},{"1":"I liked a @YouTube video https://t.co/qghcorkkBG TOP 10 SYMPHONIC METAL BANDS"},{"1":"Saturday at @SteelhouseFest saw some of the worlds finest bands venture up a mountain to deliver a kickass show!! Click the link to check out our full REVIEW &amp; GALLERY from the day!\\n @glenn_hughes @MylesKennedyhttps://midlandsmetalheads.com/steelhouse-festival-2018-saturday-28th/"},{"1":"Day 1/30 in the t_aaronmusic #31in31challenge borrow a lil sum from one of my favorite bands @thisismoonchild amber.jeanne and flipped it. https://t.co/eWa8xAMKrF"},{"1":"I added a video to a @YouTube playlist https://t.co/oM8RclsbOK Harlem Spartans (Bis x Zico) - Bands [Music Video] (Prod By MK The"},{"1":"Don't forget #DevonHour we have regular Sunday night #jazz in #exmouth 🎼 Local #devon bands and artists from 6 every #Sunday! Book if you'd like to eat as these eve's fill up fast! https://t.co/InveVeFkvz https://t.co/tJOcrhKoVu"},{"1":"tampa.. there’s a party tmrrw night &amp; i hear rlly good bands are playing(ᵔᴥᵔ) https://t.co/jqVWGfLRMx"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Bands**. Hard to discriminate whether slang or literal. Slang usage not observed. not suitable.


### 14) Term: **Bando**

```r
print_slang_tweets(14) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@fernandatanaka_ @milenereis Um bando de cães raivosos."},{"1":"CUIDAR DA SUA PRÓPRIA VIDA NINGUÉM QUER NÉ??? bando de zé ruela"},{"1":"şarap çanağı tasdiklenme #TskAnkesörÇıkmazında kesişen bando işret"},{"1":"Os eleitores do Bolsonaro deveriam ser transferido para outro país, bando de  anencéfalo"},{"1":"Esse povo q que kaysar deixe d viver pra mostrar q sofre com a situação da família dele,se nem a mãe dele que vê ele triste,não vai ser vcs (Mal amados) q vão conseguir tirar a alegria de viver dele não,bando d carniças. \\nBrilha meu cristal,seja sempre feliz💚😊🙏 @KaysarDadour"},{"1":"@marionet111 @DeliaDG @ObservatorioDmc Hacerlo pedazos también es ser extremista, solo han dicho lo que él ha hecho, ni más ni menos, tal como a Baradit o Lopez... el problema está en que politizan el tema y ahí se quedan de un bando a otro apedreándose por sus soldados caídos (con Baradit hicieron defensa parecida)"},{"1":"@g1 Enquanto isso tem um bando de idiotas fazendo greve de fome por bandido corrupto!"},{"1":"@alemdofuteboll Tem que te bando não é fazer igual mtos times da Europa e do Brasil faz te um time titular top e reserva 2 que é bom kkkk mais a Internazionale  tá forte"},{"1":"@daylimns Isso é gente que não tem autenticidade, Aqui bando de estereotipadas do CARALHO 🖕"},{"1":"@_Sandro_Soares @gleisi Esse bando de mula estão reclamando que o processo nao foi pro Moro. Coerência nao existe aqui."},{"1":"@alexandregarcia Bando de Fdp ! Seres repugnantes"},{"1":"@vagablunts Yang ada poninya itu satu pasang sama baju celana dan sepatunya. Warna merah rambutnya ponian pake bando merah jugaaa"},{"1":"El tradicional Desfile de Correos da inicio con la lectura del bando por el Alcalde de San Salvador , Ernesto Muyshondt, dando inicio a las fiestas patronales de San Salvador en honor al Divino Salvador del Mundo. @PrensaAMSS https://t.co/8YwLzVVg0u"},{"1":"@jnflesch Se não vota pode acontecer o que nos Estados Unidos. Um bando de fanáticos racistas se juntou, foi votar e escolheu o Trump"},{"1":"No meio de um bando de repórter lixo, querendo derrubar o cara, ele da show kkķkkk! Não falou nenhuma mentira! https://t.co/iOjeJmUnoq"},{"1":"toptancı bando #TskAnkesörÇıkmazında yanaşabilme"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Bando**. all tweets are in foreign language. Slang usage not observed. not suitable.


### 15) Term: **Bredrins**

```r
print_slang_tweets(15) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"So you alleyoop tings with your bredrins? — LLOOOOOOOOOOOOOOOOOOOOOOOOL \\n\\n@renaissancenunz scream with me https://t.co/OU5x24vHG1"},{"1":"@FaboMus @StringerAtlas Ahhh memories\\nUsed to try to ball out in Turnham Green &amp; Goals back in the day like I was SN8\\nBare tellin our United bredrins how Samir will be a better talisman for us than Çesc 😂"},{"1":"@IsDatYouYeah And the fact you follow 400 bredrins but not me? Come on now lol"},{"1":"A lotttttt of allyuh bredrins is wotless lochos. Doh study my social climbing. Check them."},{"1":"All the bread I had to break, for my bredrins sake"},{"1":"Wait so mandem you ain’t telling your bredrins you love them when the opportunity arises?"},{"1":"If I take my bredrins advice sometimes, I’ll end up homeless yes"},{"1":"2 of my nans bredrins were round from across the road. definitely wanted me and were not afraid to make their feelings known. never fealt so vulnerable"},{"1":"One beauty of this game is i could be long all week while my bredrins short and we can both make our p’s as long as our setups are patterned right"},{"1":"@youngManii_ LOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL.  oi do u know whats so funny, these guys will be bredrins with black guys, but move MAAAD if they find out a sis is attracted to back guys."},{"1":"@hitmansonline I don't care about your bredrins, bredrins bring in, them man are destined for sinking #Getalonggang"},{"1":"Would you suck your bredrins dick for £250mil — With absolutely no hesitation https://t.co/UEzxMIVhKE"},{"1":"S/O to @_jackfowler_ bredrins for training him well! THE MAN SAID PLANTINNNNNN (as it should be pronounced, not plaintain!) bye #oluwajack hello #Delroy 🇯🇲🇯🇲🇯🇲 #loveisland @yasminevans @1xtra https://t.co/75iwB5FxtK"},{"1":"Wayss link one😂\\nAll my bredrins do is throw up big poll status on ig if we liming and never swing 🤷🏾‍♂️ https://t.co/xibIbm4fs4"},{"1":"bredrins teaching me how to use illustrator CC and all I getting is cuss no learning going on 😂"},{"1":"imagine bts start arguing with each other and out of no where one of them goes “if gang pull up u gonna back ur bredrins?” #BTSinLondon"},{"1":"@watevsara It sounds as if he gets off on the idea of his girl being with one of his bredrins, sounds fucked up me, I’m glad you’re out of that wtf"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Bredrins**. some referral to gang and mandem culture. some unrelated tweets. Most tweets contain references to UK so better geolocation. may be useful.

### 16) Term: **Bells**

```r
print_slang_tweets(16) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@lexcrispy Jingle bells"},{"1":"i been on since a yougin bitch , name ring bells bitch 😎"},{"1":"Happy #ColoradoDay Beaver Pond with Treasure Mountain, Crested Butte, Maroon Bells, Lower Blue Lake https://t.co/w8S2Qyj0Yo"},{"1":"Bells on the rooftops of Venice, Christmas Eve https://t.co/qn2okd3h8Z via @Audioboom"},{"1":"Música do comercial Victoria's Secret | Sleigh Bells | https://t.co/si5hqqMKBA | @victoriassecret"},{"1":"Can you recommend anyone for this #job? Store Associate, Part-time, Winners, Bells Corners - https://t.co/nbVdpHnhpj #Retail #Ottawa, ON #Hiring #CareerArc"},{"1":"@The_Trump_Train @realDonaldTrump @WhiteHouse Separation of religion and state ring any bells you ignorant bell end?"},{"1":"@WalkerStapleton Wouldn’t an oil rig be more appropriate as a backdrop than the Maroon Bells?"},{"1":"@FRESCOBinFamous 😂😂 me &amp; my dumb bells don’t want no static"},{"1":"School of Seven Bells - Heart is Strange"},{"1":"@POTUS Waiting for Putin with bells on huh, buddy?\\n\\nDon't forget eye contact, asshole.... https://t.co/S8jAWKbqzc"},{"1":"A few random thoughts. I went to Mass today after my appointment. The celebrant was the former pastor at my parish. It was good to see him. He has a unique rhythm, and he doesn't use any bells in Mass."},{"1":"@LilyLuWhoT @Pandafur @fairhope71 @danapixie @TiggyBean @BorisKitty @sanjeethecat @Elvis_cat @ShivaandJaya @clingycat @PurrbotKitty @Bea_Bells @3phibotticelli @MariaPulk @Mr_Pie @tweetypie54 @JinJinDoggy @timhoyt14 @GeorgeTheDuck @lucky_GSD @Meow_Girls @cobalttash @TattleCat @PuppyNumber7 @jazzydacat @CinniMini2 @cgibson1st @GinaTheMinPin @Hf_Fulvia @kittehboi @toughteddybear @Max_Doolittle @ChazzTheDog1 @ChrisGroove1 @JustAnotherTrnd @NutmegTorby @WinnieBeanBee @thebootyband @smoooovious @NoCrybabyDoGs Good to hear Luna has been OK! #CuddlePile #BorisPorch #LunasPorch"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Bells**. Hard to discriminate whether slang or literal. Slang usage not observed. not suitable.

### 17) Term: **Bun**

```r
print_slang_tweets(17) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@EscapeTheNight WHY!!!! My little cinnamon bun just died😭😭🤧 @RosannaPansino you were amazing while it lasted. May you live on! I know she’s not really dead but you get it"},{"1":"@bun__nn 나도 자고싶은데 못자겟어 자려고 노력은 계속하는중ㅋㅋㅋ으으..나는 잘수있다..잘수있다"},{"1":"Bun a cat and a dog give me a bludclart monkey https://t.co/vrR2sxZDfV"},{"1":"@bun__nn @tkvjaos 아까 금님이 자기전에 이불 다뺏을거라고햇는데 다뺏고 본인도 이불 세로로 반만 덮구잠 ㅋㅋㅋㅋㅋㅋㅋㅋㅋ(셀프뺏김"},{"1":"@bun__nn ㅋㅋㅋㅋ아깐 나뢈님이 보고있엇어! 이 팡인들!"},{"1":"10-) 300x dahi yapabilirsiniz. Borsada şuanda beklediğim coinleri ayrıca paylaşabilirim talep olursa. Ama bun coinden çok aldığımı ve satışa koyduğumu hatırlıyorum. Arada borsayı kontrol ve takip etmeniz gerekiyor. Bazı coinler delist olması durumunda zarar da etmeniz muhtemel."},{"1":"@coffeecup6891 @HeidiM_67 @baker_russell @naomigallego @MnemoniXs @Watt_N_Idiot @Vickie627 @rcasonr @AndersEigen @Rapture571 @Happywife151 @eagle1776n @funnyhaha444 @StephanieJMajor @LisaWinslow @JRMilward @KateofLate8 @Heather4amazon @solsukut @WildChild69 @TimW3811 @MollyBrown28 @Stephen_Faris @IvyShoots @Badababa @SylvieDParris @AtheistEngineer @MaryAnn59685931 @Ah_Science @periwinklewidow @rpfregeau @BlancheMonique1 @Ornery_Opinions @manny_manatee2 @postordinary @leisure3000 @bizzimomma2 @pixelprotectors @NARAL @JuliBunting @GeniusPhx @morrigansarcher Right I forgot she did that lol."},{"1":"#Bellerin not a man bun. Not a pony tail. Full Essex face lift. What is world coming to @Arsenal? #ARS #CHE #InternationalChampionsCup"},{"1":"@timhoyt14 Nite lickle bunny pal xxx"},{"1":"bizde buna benzer calisma yaptiriliyo mu acaba kalecilere? driller muthis... https://t.co/sQx2GfLiPt"},{"1":"#Oesterreich/#Burgenland: Es wird Zeit, den Badetag am #Neusiedlersee zu beenden/zu unterbrechen - Gewitter ziehen von Nordosten her auf. \\nRadar: https://t.co/p8ewuTd1GX, im Detail: https://t.co/dkUCurfspW (für Details in den bunten Kreis klicken). #wetter /PH"},{"1":"@tkvjaos @bun__nn ㅋㄱㄱㄱㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㄱ찌으니 강★림ㅋㅋㅋㄱ 잠이 안온다..노력하러 가본다.."},{"1":"Member când aveam coeficient bun de ajungeau otelul și numaistiu cine prin champions league? Într-un fel mă bucur că nu a mai luat steaua campionatul in ultimii ani."},{"1":"Friendship is friendship but if your boy gets bagged with an abundance of ounto and comes home same day then burn that bridge I'm sorry."},{"1":"Hawaiian bread burger bun, lawdt have mercy"},{"1":"@Anesthesiolog_y @metoffice Good God what a bunch of wimps! They should have Cheffed in the Aussie mining and construction camps like one in seventies and eighties when 40 degrees norm. even with somewhat primitive air-con! A Chef works in these current temps. all the time !"},{"1":"Bunch of Aggy old ladies 😂😂 https://t.co/D2eADx44V1"},{"1":"i work with a bunch of paigon's uno"},{"1":"@bun__nn 이번엔 번님이 커뮤를 보나..!"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Bun**. Hard to discriminate whether slang or literal. Slang usage not observed. not suitable.

### 18) Term: **Bruck**

```r
print_slang_tweets(18) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"10月24日に生まれたドライバー：Joachim Winkelhock（1960）、Fairuz Fauzy（1982）Christopher Bruck（1984）、山内英輝（1988）　亡くなったドライバー：Jo Siffert（1971）"},{"1":"@m_ueberall @golem Johnny Bruck ist 1995 gestorben – seither sind mehrere Zeichner für unsere Titelbilder verantwortlich. Dadurch ist natürlich auch die Bandbreite gestiegen ..."},{"1":"@bruck_mann No quise decir que hay personas que no la necesitan, es obvio que sí... pero no es plata que venga de buenos fondos. y la sociedad chilena escandaliza el evento como si fuera chiste, personalmente no me parece que se haga, no me siento identificada ni parte del show."},{"1":"The whole thread between Jason Bruck and Naomi Rose is a huge mess. I wanted to respond directly, but Twitter's character limit sucks, so I'll make a post on Tumblr instead since I have more freedom there."},{"1":"long nose ting with the 2 shells \\nyou can see the front bit bruck arff"},{"1":"Der 1. FC Sand zerbröselt am eigenen Strafraum. Zwei Tore kurz vor dem Pausenpfiff, @FSVErl_Bruck führt mit 3:0 | #Erlangen #Fussball #Bayernliga"},{"1":"Und des z'Bruck: Flashmob von Ordensfrauen gegen Sozialkürzungen - https://t.co/AWe3ji7f4S https://t.co/riLrb62q98"},{"1":"In etwas mehr als zwei Stunden geht es in der #Fußball #Bayernliga schon wieder rund! Ich schaue mir heute das Heimspiel des @FSVErl_Bruck an... https://t.co/WAt7qRaszY #Erlangen"},{"1":"Emancipate yourself from mental slavery 🇯🇲🇯🇲🇯🇲🇯🇲\\n.\\n.\\n.\\nSide note:  Emancipate yourself from bruck man to"},{"1":"33 Grad und es wird noch heißer! #Fussball wird beim @FSVErl_Bruck natürlich trotzdem gespielt. Zu Gast der 1. FC Sand | #Bayernliga #Erlangen https://t.co/M0jD6tHOW5"},{"1":"Trop contente d’avoir retrouveeee mes chéries hier soir, bruck m’as rappelé de beaux souvenirs"},{"1":"Dem charge you when you bruck and pay you when you rich..... make sense."},{"1":"(FSV Erlangen-Bruck - 1. Sand) - https://t.co/NhfyeMYT1Z"},{"1":"Thomas Roas trifft mit einem feinen Fernschuss zum 1:0 für den @FSVErl_Bruck - es folgt eine schwitzig-herzliche Umarmung mit Rafael Hinrichs | #Erlangen #Fussball #Bayernliga https://t.co/QGfhOe8ySW"},{"1":"Fall Sports begin on August 8th (one week from today)!  Have you registered?  If not, do so at https://t.co/fulDmXotIa  You must also have a physical before trying out.  Email Coach Bruck at keith.bruck@fcps.org if you have any questions."}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Bruck**. Hard to discriminate whether slang or literal. Slang usage not observed. not suitable.


### 19) Term: **Brukk**

```r
print_slang_tweets(19) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@TakasugiRyou brukk\\n\\ntubuhnya terhempas dengan sengaja pemuda itu menarik gadis agar sang adam bisa menindihnya seperti guling, disini ia dapat melihatmu dengan jelas dalam jarak sedekat itu, pemuda vampir ini sangat menggairahkan.\\nTidak, bukan saatnya berpikir itu.+"},{"1":"@tigabella -- ah mungkin saja dia hanya sedang bercanda dengan teman temannya , gumam ayla. ia memutuskan untuk keluar kamar dan berusaha mencari teman namun terdengar kembali teriakan sang gadis tadi disusul dengan \\n\\nBRUKK \\n\\n\"aduh... maaf maaf aku ga sengaja,.\"\\n\\nayla segera membantu --"},{"1":"Sepuluh!”\\n\\nTepat pada hitungan ke sepuluh, kami melompat keluar dari pintu mobil dan berguling ke sisi hutan.\\n\\nBRUKK! CRASH! BOOM!\\n\\nMobil yang kami tumpangi ditabrak dari belakang dan Jeep yang mengejar itu meledak. Beberapa potongan badan mobil (44/66)"},{"1":"Brukk loften."},{"1":"1 week to go for one summers biggest party BRUKK OFF 💯💯💯🔥🔥🔥 https://t.co/JTTUYwMjqU"},{"1":"Dem tump yah will brukk jawbone"},{"1":"Only 1 week to go for one of summers biggest party  BRUKK OFF it's gonna be madd ting badd ting Dancehall Defenda Sound playing on the night along side… https://t.co/KkjdpM6c2z"},{"1":"Il never understand this.. your brukk like darrrg but breeding every second 🤷🏾‍♀️🤷🏾‍♀️ https://t.co/gtLcIdmRUm"},{"1":"&lt; pelayan dengan nampan berisi segelas Cappuccino berjalan ke arahnya dan ... \\n\\nㅤㅤBrukk ...\\n\\nㅤㅤCha Eunsa membulatkan matanya tatkala pelayan tersebut menabrak tubuhnya hingga membuat minuman yang tengah ia bawa tumpah ke bajunya. Pelayan itu terlihat sama &gt;"},{"1":"-- seketika aura disekitar pemuda ini berubah.\\n\\nNtah apa yang akan terjadi selanjutnya. Ia bangkit dan memukul wajah Valeos dengan keras.\\n\\n'Brukk!!'\\n\\nMembuat penyihir kabut itu terjatuh. \\nGray hanya diam,tidak satupun kata terdengar dari bibirnya.\\n\\nHanya saja, sekarang --"},{"1":"Jadi ini oknum yg bikin kebangun tengah malem, tiba brukk nimpah muka gw :') https://t.co/bzCvhhKHrG"},{"1":"Mate my back is brukk"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Brukk**. Hard to discriminate whether slang or literal. Slang usage not observed. almost all tweets in foreign lang. not suitable.

### 20) Term: **Body**

```r
print_slang_tweets(20) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@sourcecoryon Nobody wants to see that...... \\U0001f928 Chase deserves better."},{"1":"My body is so sore and  I have no days off from the studio and gym"},{"1":"1 Peter 2:24\\n\\n24. Who his own self bare our sins in his own body on the tree, that we, being dead to sins, should live unto righteousness: by whose stripes ye were healed."},{"1":"Stop your bloodclart crying, the kid , the dog everybody dying"},{"1":"🎶Writing names on my hollow tips, plotting shit\\nMad violence who I'm gon' body, this hood politics\\nAcknowledge it, leave bodies chopped in garbage's 🎶 https://t.co/C8Jb3CsWYf"},{"1":"Him just drop me off.\\n\\nSuh hear me holding up my hand, \"These?\" Almost thinking another part of my body name hands. \\n\\nBut poopah Jesus this man a season up chicken. Weh him just seh. Me na process this a bloodclart."},{"1":"@nedpayne_echo Bomber has had that happen before, does he still wear his blow up body armour?"},{"1":"Kylie just too cold for his on body to handle. https://t.co/v0P6qWEXrW"},{"1":"Ive recieved so many vaccines in a short amount of time in order to get into this college program and i know better than to voice my opinion on this online but damn i am so sorry to my body 🆗🆒"},{"1":"I go hard on my ones don tings that nobody’s done https://t.co/82pdcSxT51"},{"1":"@Zoeyxtkop @lokilent @tiredhan @griffnsblake Prime example of somebody who should not own animals. Lemme squeeze your head into a small box until you're anxious &amp; when I let you out tell you \"you're stressing over nothing you won't remember it in a minute\""},{"1":"and I can decide by myself what I’ll put in my body like all these pills https://t.co/MakMxVALDU"},{"1":"@VickyNarni 💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋💋#Sexy #Superbabe Vicky ... love seeing you on TV and hope we'll do so for a long time to come. #Amazing #Hot body!"},{"1":"@erimcicekk Little tip regarding anything humans ingest:the evidence is pretty much always lacking. There is so many unknown factors in the human body. You often have to spend hundreds of millions involving hundreds of people over several years to get good data. Very rare to have data."},{"1":"Knowing that Even our body doesnt belong to us but we use animals skin for. Self issue"},{"1":"@caitquerc Body Talk is probably my favourite pop album of all time and this is making me feel things so hard while still being an absolute synthpop banger, I’m so happy"},{"1":"is addiction a disease or a choice? \\n\\n-you chose to do the drug(s).\\n-your body got hooked because you chose to do the drug(s) in the first place.\\n-your body is now dependent on that particular drug(s).\\n\\n...it’s completely different if a child was born addicted bc of the parent."}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Body**. Slang usage not observed. not suitable.


### 21) Term: **Bait**

```r
print_slang_tweets(21) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"Just ignore this, this was a post made by another 4chan bait account. The account was only made 2 days ago. https://t.co/4xIzl2PlY5"},{"1":"@matttrinh @douglasernst No... that's the new Arnold. And the little girl is the new John Connor. And Sarah Connor is herself... but now she's just Luke Skywalker-style bait for old fans who will just be there to promote the young girls and then die. It's clearly an all-female version of T2."},{"1":"Gruden responds to Papa saying Conley has a Vitamin D deficiency..... \"I haven't heard that.....we will get him some supplements\" (laughter)\\n\\nStop listening to these fake reporters #RaiderNation they aren't in the locker room. They are around for click bait &amp; headlines. 🤦🏿‍♂️"},{"1":"It is a pleasure to be working with @NorthernBaits, helping to build new relationships with UK retailers. &amp; fisheries in the UK &amp; France.\\n\\nIf you woul like further info on the range or how to go about setting up an account, nic@impactanglingmedia.co.uk\\nhttps://t.co/5NUuXhkrVm"},{"1":"@maralenenok Maybe if we play together you can bait my fishing rod for me! There's a glitch on Mac that means you can't add bait or tackle without a two-button mouse."},{"1":"Kimmel Embarrassed on His Own Show When Trying To Bait Kim Kardashian into Bashing Trump https://t.co/QMCaWrFUXa https://t.co/4nLugIVBmS"},{"1":"Still looking for the Big One??  We have the bait for you. Honey Go Catch me some dinner!! Sports Madness https://t.co/lqmNX5RGFd"},{"1":"famoso click bait, beit sla"},{"1":"Do you know how to make a hormone's Ok click bait to a serious vid on hormones https://t.co/slgqo6HP6P learned alot https://t.co/NcwTz28vsf https://t.co/MXQj7IczaE"},{"1":"@builditrightcan Oil pipelines are the gold standard of don't do it. The gold standard in \"how did we get trapped in this can't win game of lose and lose.\" The gold standard in bait and switch \"looks like prosperity but we're deeper in debt and sicker than dogs.\""},{"1":"@PrincessaPinot @elcholo1923 @PBMMW @CalifWines_US @PeterRanscombe1 @moevino @eddiewat @CalifWinesUK_IR @PCWineAwards @SigneSJohansen @davidc1863 Sorry that was click bait!"},{"1":"@Factinate No one cares who he lost his virginity to. I'm sure this is just another Click Bait ad anyway. Ain't clicking."},{"1":"This is really a non-story, border line click bait to gain attention off the popularity of AOC. https://t.co/4j0xvkR1Mh"},{"1":"pag nasa harap sobrang bait, parang santo pero pag nakatalikod sinasaksak ka na pala 😔"},{"1":"@SCancillar Trueeee! Hahaha grade 10 grade 12 at grade 11 lang maingay don sobrang bait ng mga estudyante ngayon hahahaha"},{"1":"@FoxNews @ChrisStirewalt I sorta see that snooty arrogance from you when you talk about Trump. Some in the media do not know how to figure out a businessman that is tougher than any weak politician. He says things just to mess with you and you take the bait. He’s not a politician. He gets things done"},{"1":"I will say this, though: if by some miracle Artavis Scott unseats Benjamin as the punt returner, it’s time to cut bait on Travis. He isn’t nearly productive enough as a receiver to justify his salary if he isn’t returning punts."}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Bait**. Slang usage not observed. not suitable.

### 22) Term: **Block**

```r
print_slang_tweets(22) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@ShikoNguru Soda baridi ya Fanta na mkate ile block that is everything"},{"1":"Updated: Crash in Walton on I-10 west at MM 77, left lane blocked. Last updated at 03:15:21PM. https://t.co/PbQyTk2bGF"},{"1":"@Fauxnine_ @ianmccourt Everyone! Block Anders."},{"1":"Can someone explain why some ex’s wanna lurk in the shadows, even thou there’s zero connections?! Do yourself a favor and move on, don’t be that creep😖 who changes their number just to text you?! #youlookfoolish #weallhadagoodlaugh #whythou #blocked...again. #foreverblocked"},{"1":"if you support pedophiles sTAY THE FUCK AWAY FROM ME AND BLOCK MY ASS THANKS"},{"1":"@glamourgirlca @BarryDavis_ @thehazelmae @Sportsnet She can ask you to unfollow.  Or she can just block you.  She may end up doing that at this pace anyway."},{"1":"@MGauquier @JoeJoyce2 @realDonaldTrump extremely fake, report and block."},{"1":"How to Make a Peak Quilt Block with 10\" Slicer and Crafty Gemini: https://t.co/lAJObJEllk via @YouTube"},{"1":"@DrRuler feel if you go on to older sets, might just want to do the whole block of  highlights.\\n\\nalso, I'm a big fan of settle the wreckage, an absolute blowout. also has been blanked hilariously by Shalai, Voice of Plenty more than once"},{"1":"Hi sylveon is trans. If sylveon being trans makes you so uncomfortable you have to block people please block me. Sylveon is trans sylveon is trans sylveon is trans https://t.co/KUYfDOgh9K"},{"1":"So that I symbolically can block Donald Trump. Felt good in my soul. #WhyIJoinedTwitter"},{"1":"$bac bulls still in control,yestrdays calls block alert paid https://t.co/JAbXk0hXVE"},{"1":"@dacheezblock @TSM_Myth @CoreyMenzinger *slaps across face* no none of that"},{"1":"@WDUZ 51-555-9 won’t get as much action from Hundley after his inability to block helps gets Rodgers hurt again..packers shouldn’t use him as a blocker..still..his hands are so so..he is a far cry from the Saints days"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Block**. Almost all tweets refer to 'blocking action'. Slang usage not observed. not suitable.


### 23) Term: **Bird**

```r
print_slang_tweets(23) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"I’m trying to find a specific bird by its call"},{"1":"@Natures_Voice @BBCSpringwatch A Wood #Pigeon egg 🥚 found on the front lawn this morning ... not the best nest making bird 🐦 - it was still sat on a few twigs in the tree none the wiser ... https://t.co/twf9PLqedG"},{"1":"Thanks for the mention! NomadicDreamz: Its a bird, its a plane no its the #Barcelona cable car! https://t.co/5Ei6nAYK39 via @wyldfamtravel https://t.co/hIIefNdAR8"},{"1":"Final days to save up to $400 off on your #STARWEST registration. Super Early Bird pricing ends this Friday, August 3! Learn more: https://t.co/m3q3sJSqSI #testing #softwaretesting https://t.co/K5y6XeYDPK"},{"1":"Deadly bird parasite not as bad as feared so far this year, but public should still be vigilant https://t.co/hKYk4L93gD https://t.co/TniatJevab"},{"1":"@mrandersonofus Early bird ends 9/3 at 4pm PST. Final deadline is 9/8 at 11am PST (also the deadline for week #1 selections)."},{"1":"'Close all kauri forests' urges Forest &amp; Bird, via @nzherald https://t.co/INOLSLu2JW"},{"1":"Teruntuk perempuan yg sedang berjuang melawan gejolak hati,  bersabarlah. Jodohmu akan datang sesegera mungkin. Jangan tergesa-gesa utk menjatuhkan hati pd lelaki yg sedang menjaga &amp; berjuang dgn jarak. Anda berhak atas laki-laki lain. \\nYhaaaaaa,  begitu. 😘\\n\\n-Early bird thought-"},{"1":"Report dead birds at 1-877-WNV-BIRD (1-877-968-2473) https://t.co/rhUkHgMs7E"},{"1":"@Dog_Sized_Bird Me every time any my friends play"},{"1":"@MichaelaOkla bird watching"},{"1":"Flappy bird is like Angry birds.. But im the one angry."},{"1":"Sometimes you just need a dinasour looking bird to brighten your day. I give you the Cassowary from New Guinea and Australia. This one is from the @SanAntonioZoo. https://t.co/3OYRkRpq76"},{"1":"\"Ay, but I meant not thee, I meant not her, whom all the pines of Ida shook to see, slide from that quiet heaven of hers and tempt\".... \"Rather o ye Gods of nature when she strikes through the thick blood of cattle and lambs are glad nosing the mother's udder, and the bird\"..."},{"1":"bird ass nigga"},{"1":"@JohnSwedlund @MarkRogersTV Eating Chicken makes you a nervous bird......."}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Bird**. Slang usage not observed. Even slang meaning is mild. not suitable.


### 24) Term: **Batty**

```r
print_slang_tweets(24) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"Dan Batty ready to prove Leeds United wrong as he steps into Hull City spotlight #hcafc https://t.co/610blHUwft"},{"1":"@batty_betty_98 ファーーーン！！(矛盾を感じた音)"},{"1":"@denisebatters Hey Batty Batters,  try actually running for office"},{"1":"@5yrs_late Lard mi #migarn nah seriously tho from lickle bass in the voice to bih ass. #🍆💦 #yambattygal #ffsinternet #yallwrongforthis #😉😉😉😉 #🤦🏾‍♂️🤦🏾‍♂️🤦🏾‍♂️🤦🏾‍♂️ #😂😂😂 #AfroRoxWorldwide https://t.co/Fxlj0Ujjkh"},{"1":"@sEc_SPN @BuckeyeGirrl @CoachHalejr @FoAmZ_X @SEC_Exposed U want me to take a pic of yalls batty games because they weren’t even close. How many times y’all get beat by 30"},{"1":"Instagram really went and took the novelty of batty pics away dinnit"},{"1":"@LorDefiance @Hidasan @shortyart Lucky dragon! Love that big batty booty! X3"},{"1":"Excited to share this item from my #etsy shop: Orange felt bat halloween decor, felt batty ornament, glow in the dark bat fang, felt fruit bat gift, spooky bat decor, pumpin kawaii bat. https://t.co/WoWqJxLEcX\\n#etsy #etsyshop #EtsyTMT #MakingIt #MakingItNBC https://t.co/c2yuyFxWxy"},{"1":"@batty_betty_98 いやごめん普通に酒じゃなくて100円のりんごの炭酸でええ"},{"1":"@batty_betty_98 『もしかしてホワイトステージ整地してくれた辻本メンバーですか？？』って聞かれんねんで\\n誕生日おめでとう酒たのしみにしてるわ"},{"1":"Siguiendo mi tradición, viñeta de hoy dedicada a l@s nuev@s seguidor@s @Zoe_Soler @JMRubenJM @tieneperejiles @RosaRufo @efrainreiser1 @Otro_Periodismo @acasadobufo @angelestesting @jmrtnsa @MariaTe20532675 @vidushi_i @Roy_Batty_Reply @vieyra_joe @Jgarrid25453818 @mercheroncero"},{"1":"@_alexgstone Meyler not good enough either in reality. Henriksen, Irvine, Batty all surpassed him much as I love him. Winston Churchill was a good leader, he can also move as well as Dawson. Abel is a real loss. The big loss. Best in the league. He wanted 100k wanted   week though 😂😳"},{"1":"@Sharessan That big bugbeaar boy is batty about that bitter bubbly brew!"},{"1":"@DefenderMothman A huge wind passed by, ruffling Batty's hair. A wind whipped up and a portal shot out, forming right in front of them."}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Batty**. Slang usage observed but no reference to crime. not suitable.


### 25) Term: **Bossman**

```r
print_slang_tweets(25) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@BossMan_Riq yeah u just talking"},{"1":"I liked a @YouTube video https://t.co/v8p5ZDunX4 Bossman watches AUS BattleRap - Creepy J vs NES (Round 2)"},{"1":"@HarveyEJAustin Absolute bossman! Why?"},{"1":"I liked a @YouTube video https://t.co/A79zOzxc1i Bossman watches AUS BattleRap - Creepy J vs NES (Round 1)"},{"1":"@LucasDigne Welcome bossman. 🤙🏻"},{"1":"@zachswon @venetianfest @TheSwonBrothers #ZachOfAllTrades #MultiTasking #Bossman #Smooth #NeverGetsOld #OkiesRock 🎶🎤🎸🥁🎹👍💖🤘 https://t.co/GGkiX4Vz6Y"},{"1":"This dude dead ass lied staright up, comfronted him with BossMan, nigga locked himself up in his crib... what a bitch bruh"},{"1":"@SkylarR_Darker @MGrey_Darker &lt; in deep water with Mr. Bossman himself."},{"1":"@LucasDigne Welcome to Everton Bossman 🔵"},{"1":"@SkylarR_Darker @MGrey_Darker &lt; pocket to start making calls, waving to Skylar and the bossman as I do."},{"1":"I liked a @YouTube video https://t.co/4d9wlzfzqg Bossman watches AUS BattleRap - Billz vs Carlos the Jackl (Round 2A)"},{"1":"@gayboyrari @lilrolex_ebooks in like 30 bossman"},{"1":"I liked a @YouTube video https://t.co/9JTakpR4Ms Bossman watches AUS BattleRap - Billz vs Carlos the Jackl (Round 1)"},{"1":"I liked a @YouTube video https://t.co/geSBXZrr7z Bossman watches N.E.R.D vs DirtWorm (UBL Battle) Round 1"},{"1":"Jerry says: Get Back to Work!\\n.\\n.\\n#bossman #shopbird #direstashop #shoplife #birdlife #beakbeak https://t.co/e16BQPPTgH"},{"1":"I liked a @YouTube video https://t.co/LLtUvv3tSh Bossman watches AUS BattleRap - Creepy J vs NES (Round 3)"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Bossman**. Slang usage observed but slang usage carries a positive sentiment. many references to YouTube music videos. not suitable.

### 26) Term: **Beef**

```r
print_slang_tweets(26) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"...Got a couple racks of beef ribs on deck! Just finished putting my secret dry rub on, now I'ma slow bake these bad boys in the oven for a few hours, then finish 'em off on that charcoal… https://t.co/ZQ6dmK8XsK"},{"1":"@OeauxMar @ItchyDropEmOff If you see a person you have real beef with 9 out of 10 they taking that shot because if the roles reverse they prolly not gonna let u slide"},{"1":"@john_setzler @TraegerGrills Beef chuck ribs?"},{"1":"@CanSpice @rwittstock @PJNewWest @laureljeanine @Obrassor @bikesnobnyc @jonathanxcote @stephenoshea Maybe you're right, but maybe commercial is not the destination it used to be. I have a beef with poor transit access to parks and beaches throughout the region. This ties in with low density around said parks and beaches..."},{"1":"Anyone have time to explain to me how this Booba &amp; Kaaris beef started? 😁"},{"1":"Fam say wallah Kaaris and Booba got into beef https://t.co/XLNtrhlzhg"},{"1":"@GOP is complicit. They are not even hiding it any more. Senate rejects effort to beef up states' election security spending  https://t.co/MVBD2gn69T"},{"1":"Primitive Technology Cooking skill BBQ Beef recipe Cooking skill https://t.co/BgyCvQYn9P https://t.co/lSRjJjt9Gt"},{"1":"#BREAKING: Senate #GOP rejects #Democrats effort to beef up #electionsecurity funding https://t.co/JCESeoMZtp  https://t.co/yAouRQKAov"},{"1":"Let that hurt go ... but you ain’t lettin yo weak ass beef towards me go ... you taking it out on Our baby &amp; that’s Wrong !"},{"1":"@ETDEUMPURITAS corned beef hash transcends being gross on just the vegan level, it should be universally disgusting"},{"1":"It’s Beef den where da biscuits at cuz we ready to eat 🤘🏾💪🏾"},{"1":"@eddiemarsan Was the 99% figure researched or is it just a nice number you`ve made up to beef up your predictable attack."},{"1":"@__56426__ @stevewallwork Any beef with @NewcastleGoals should be taken up with him, but I think the two letters will have exactly the same impact. None. Also, one was done in jest, and one was written in all seriousness, yet they were both somehow hilarious."},{"1":"@ryanlouvie22 @HunterMisse Happy Birthday to one of my other brothers😂 aka, the one who eats all my beef jerky"},{"1":"@_mykaila_ thought you wanted to get your nails done without beef 🤷🏼‍♀️ oh well. and i’ve been enjoying it, thanks :)"},{"1":"A wagyu beef burger on white bread with jack cheese and fried onions"},{"1":"Like bruh clearly went back to that album and made some changes after the Pusha beef but he left that one line alone lmaoooo"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Beef**. Slang usage observed but adopted by a wider population so the sample does not include gang reference. also has has a non-slang meaning that is commonly used. not suitable. 


### 27) Term: **Blanked**

```r
print_slang_tweets(27) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"\"The teleprompter's screen blanked out. A second later, a single line of text appears.\\n\\nSOMEONE IS HERE TO KILL YOU\" https://t.co/ZeaQgqCick"},{"1":"me, no more than 5min into any shift: NOTHING TO LOSE BUT YOUR CHAINS, COMRADES!\\n\\nme, no more than 5min after my weighted blanked with minky cover finally arrived: CAPITALMISM IS GOOD THO"},{"1":"the fact that we get to do this together 🙌🏾🙌🏾🙌🏾\\n///\\nyooooo @j_hairston_jr you blanked on this cover for @mycitycharlotte! thank you for the feature! 🙏🏾🙏🏾🙏🏾\\n///\\n///\\n#duppandswat #cltisdope… https://t.co/OmngMzdFPS"},{"1":"While we don't endorse Sharkey for reasons previously stated, his intention to run should at least be acknowledged by the authorities instead of blanked which is what they are currently doing https://t.co/FECuwXavRu"},{"1":"Completely mind-blanked. Couldn't remember the word 'speckled'. I'm just like 'spettled' doesn't sound right. 😂"},{"1":"Blanked Thoughts"},{"1":"whose fanbase was called melody i completely blanked out was it btob my brain is glitching pls"},{"1":"G herbo blanked on letter that might be my favorite song of 2018"},{"1":"@VapeMasterZero of course i have an offline copy, i am just alarmed that whej you try to reaccess them they are all blanked out"},{"1":"Tommy Robinson blanked on BBC 6 o clock news. Top story is elections in Zimbabwe.  Who gives a f#ck about Zimbabwe?  They kicked us out of there and the place has gone to shit. Why is this top news in my country?"},{"1":"I knew new computer setup was going suspiciously smoothly. It just blanked out, sent a pop-up saying 'check HDMI cable'. There's no HDMI cable. Had to TOATOA. Humph."},{"1":"@steffanwatkins I had an awesome response &amp; ive blanked out.  I promise it was good 😂😂😂"},{"1":"@JusAnothaCritiK @Caclifton35 @colemcdowell2 @Woj__Bombs @espn Kinda sounds like the exact same way Ohio state made it to the playoffs in 2016🤔   but the difference is bama didn't get blanked by clemson"},{"1":"Do you know the difference between a debit card vs. credit card? At first, this kid blanked. When I gave him a chance to think about it, he knew the answer. So many young Americans don’t… https://t.co/bpHeRVh4Qk"},{"1":"FINAL: Bullfrogs blanked by the Fond du Lac Dock Spiders in game one...\\n\\nWP - Austin Wagner (6-1)\\nLP - Ryan Gowens (1-2)\\n\\n➡️ UP NEXT: Bullfrogs play game two in Fond du Lac at 6:35 p.m. from Herr-Baker Field! #GoFrogs 🐸 https://t.co/ekPvIebmds"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Blanked**. Slang usage observed. No reference to crime. not suitable.

### 28) Term: **Beg **

```r
print_slang_tweets(28) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"Kerry Katona’s Fans Beg Her To Tidy Her Filthy Bedroom After Posting Sexy Selfie https://t.co/nQm00L3SiM"},{"1":"Dear future daughter, \\ndon’t ever beg for someone’s time. don’t ever beg for someone’s attention. and don’t ever beg for someone’s love."},{"1":"please don't disappoint us.. i beg you https://t.co/f5Fzln7wws"},{"1":"Lynn Anderson - I Beg Your Pardon, I Never Promised You A Rose Garden (B... https://t.co/SuUtypEgRk via @YouTube"},{"1":"@NewHopeBlake I beg for paper hearts or stay"},{"1":"@GovnarAli I beg all be lie"},{"1":"@ms_ting_ I beg to differ !!"},{"1":"@miss_isioma @olulagos @OgbeniDipo 😲Not today please. I beg you You, wait until Baba returns from his working holiday in London.😭 https://t.co/oUZJd14Qq9"},{"1":"Realtor friends. Please. I beg you. I know an “exclusive listing” might be novel for a couple of days, but if it’s been 2+ weeks and the house hasn’t sold, for the love of goodness put it on MLS."},{"1":"@amirkhanmma 3/3 sir please do it I beg u 🙏 these movie will change many life sir trust me. I assure you movie budget it not more then 10 CR but I believe it can break all records. I beg u to listen a story once pls..."},{"1":"THEY COST LIKE $20 AND I HAD TO BEG MY DAD FOR A SOLID 4 MONTHS UNTIL HE AGREED TO BUY ONE FOR ME AND I NEVER GOT IT IT'S BEEN 10 YEARS"},{"1":"@Perel1984 @hotdiggedydemon @wacom please, please I beg of you two, reunite… bring peace to the wisenverse"},{"1":"Ma boss I beg wer u Dey watch the game for !! https://t.co/N4WqJecnwB"},{"1":"never beg a person \\nto stay in your life ⚠️"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Beg **. slang usage is using this term as a noun, rather than a verb. need to use NLP to distinguish. not suitable.


### 29) Term: **Bumbaclart**

```r
print_slang_tweets(29) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@roomshamblez He takes two days to reply to me but he’s always active on insta I send him three messages and he replies to one about a bumbaclart cat like I have penis written on my forehead?"},{"1":"Jeff Sessions Did Another Bad Thing - Splinter - Separation of Church and State is going great right now, Jeffy. No need for your ‘stinkin’ task force. Bumbaclart. SAD. https://t.co/HiBmZFr3Ul"},{"1":"Wow. Car loans and loans when my name is not RBS. Bumbaclart. Juju is real. I bet the man is not working, init? https://t.co/QpSWUXHQCm"},{"1":"@conrjoe_ @rishh96 @callumhoeyy Rasclart bumbaclart eeedjat"},{"1":"been battling one episode for days now, and i’m glad to announce that i’m finally making this bumbaclart script my bitch."},{"1":"I just want to know why kojo funds thought it was acceptable to randomly say bumbaclart in check. That’s all"},{"1":"I’m using “I do say to you sir, I am no rarseclart bumbaclart, fucking eeeedait” everyday from now https://t.co/hIKRhHj113"},{"1":"@Conservative_JA Bumbaclart!!!"},{"1":"Walk &amp; live , Talk &amp; bumbaclart dead."},{"1":"How’s levi just asked me do I reckon jerk chicken and roast chicken come from the same sort of chickens, like theres a chicken in a coop somewhere shouting bumbaclart 😩"},{"1":"@Miss_Zelda_Zonk @mrichardshost Recently he's been proper bumbaclart, but his early stuff... phwoar. Jesus Walks is a BANGER"},{"1":"@beckyjaneryan Bumbaclart"},{"1":"my 6yo daughter was just overheard calling someone a bumbaclart, I'm not sure what it means &amp; I have no idea where she heard it.. was pretty funny hearing her Jamaican accent 😂"},{"1":"I mean them weed seed bumbaclart different hoes"},{"1":"Bumbaclart https://t.co/kdaU8byxmT"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Bumbaclart**. Jamaican origin. slang usage observed. used to refer to negative things or persons. possibly many tweets from the UK. not many references to crime.


### 30) Term: **Bludclart**

```r
print_slang_tweets(30) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@LeeGunner82 'Bludclart' 😂😂😂😂 easy wid dat g"},{"1":"pinch punch first bludclart day of the month, no raascleet returns 💕"},{"1":"Believe in your bludclart barber 😂👌 https://t.co/pgmjhaxAe6"},{"1":"WHAT A BLUDCLART WOW https://t.co/FgW9JBabrC"},{"1":"gwarn ah yuh bludclart? https://t.co/A1WbDa8cbD"},{"1":"ANNOUNCE THE BLUDCLART CONTRACT https://t.co/pWD7ucmluA"},{"1":"@Lzison BLUDCLART !!!!!!"},{"1":"Bludclart Paul he ain’t even relevant \\U0001f9d0\\U0001f928 https://t.co/xFagIMn2v7"},{"1":"That Tommie and Spice tune is fiiiiiire tu bludclart 😩😩🔥🔥 https://t.co/izhHgjJWv0"},{"1":"In Cape Verde. How can i order a meal with plantain and they bring me FUCKING BANANA. What kind of bludclart mockery. I’m disappointed in Africa."},{"1":"@alhan ayo blud...come tru wid dat next gasworks ep. now ah wah di bludclart ya ah deal wid"},{"1":"FIAT FUCKING 500 BLUDCLART FUCKING TWITTER"},{"1":"WHAT THE BLUDCLART"},{"1":"I am not the bludclart one."},{"1":"Ahlie, every bludclart second ad break 😒 https://t.co/bAAkCevpiy"},{"1":"What a finished club , a man can’t even spend time with his new born ????? Fam this isn’t even competitive games , it’s the bludclart pre seasons , these man lack any empathy https://t.co/epb8OKromG"},{"1":"@NorthBankJay Do you the think word ‘bludclart’ was uttered?"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Bludclart**. slang usage observed. very informal spelling. used to refer to extremely negative things or persons. many tweets from the UK but not many references to crime.


### 31) Term: **Bloodclart**

```r
print_slang_tweets(31) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"Hey Siri WHEEL UP THE BLOODCLART https://t.co/wMl2gJqxI6"},{"1":"eczema fi bloodclart dead"},{"1":"pull up the bloodclart"},{"1":"So you’re telling me a brother is dying and you wanna pull out your phone to record it? You see you ppl that records every and anything one day someone gonna lick your head off. \\n\\nNinja man is right unuh wicked bloodclart help deh people dem"},{"1":"Must be nice being asian in the summer ini, bloodclart weddings everyday kmt"},{"1":"@raymonddelauney MR Chucka bloodclart Umunna!\\nDoes NOT have any care for the young children.\\nActually! \\nHe's the worse of them all."},{"1":"How many bloodclart sponsored posts do you want to put on my feed @instagram"},{"1":"⁦@shenseea20⁩ bloodclart 💛🤘🏽❤️ https://t.co/GH8vGmv75T"},{"1":"Me ha gustado un vídeo de @YouTube (https://t.co/BkWewXXr4c - Monss Ft. TheNameIsIsh - Bloodclart)."},{"1":"@linkuptv @bignarstie Big up b nasty Hadouken fi dem bloodclart"},{"1":"More time me sidung an ah wonder is like a disease some people hah mek dem cyar Lou out yo bloodclart name duh."},{"1":"Believe in your Bloodclart Self 🏆🏆"},{"1":"#MusicBreak #NowPlaying \\nEBR - Bloodclart Mix Vol II - w/ tunes from @6blocc @Kromestar7 @CHIMPOMCR @ruskoofficial et al - https://t.co/cGKQ6oDy7h https://t.co/aZO1fwVtTn"},{"1":"@tiatsim dont expect that from me im sending a zip and u have to dl yhe whole bloodclart lol"},{"1":"Trash show. Fire bun that bloodclart whole island."}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Bloodclart**. slang usage observed. used to refer to extremely negative things or persons. many tweets from the UK, and some references to crime or violence.


### 32) Term: **Bally**

```r
print_slang_tweets(32) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@VitalVegas I got 11 nights in 4 separate places for $120 total \\nFirst 8 nights and last night are free only hotel that's cost me anything was Bally's"},{"1":"I'm at Bally's Casino &amp; Hotel - @ballysac in Atlantic City, NJ https://t.co/nA8JXAZGwz"},{"1":"Mohran bally Ty Lawan Gy Ch Amir Sultan Cheema and Ch Faisal Farooq Cheema PTi Song YouTube: https://t.co/xFVdylsFr7 via @YouTube"},{"1":"Jamaican Culture is positioned Globally now, we need the Artists/Performers/Promoters to think that way, we can’t only be catering to the yute’s pon the corner, it’s a business, which means you can’t approach it like a hustle..."},{"1":"@jlgolson @elonmusk @atari ms pac was not atari :) Bally-Midway"},{"1":"@that_ali_tho Bally bally😂"},{"1":"@ImranKhanPTI  @PTIofficial @ImranIsmailPTI  balochistan ka msg sir Ik k lye...sir ham ny thappa lgaya bally pr..nahi dekha candidate kn hy kesa hy..ap ny electables lye party me vo b manzoor..lekin CM balochistan k lye khuda k lye sardar rind jesy qatilo ko select na kren plz"},{"1":"I added a video to a @YouTube playlist https://t.co/4B7ZuPIg80 Mera Laung Gawacha HQ - Bally Sagoo"},{"1":"Hey @BSlickComposer I joined the B-squad, too! You can call me “Bally” lol"},{"1":"Kagaguhan talks with my bally!😂💙"},{"1":"@aajtak Jis ko Sone ki jagah nhi..\\nOh dusre ki chinta karte he...\\n\\nJo ak din Rahne kelia Ram-Rahim ke Dhera me Bally Dance karti he oh dusre ki chinta karte he....@Taslimarif"},{"1":"@howey_lee Just finished the book, cracking read and on the whole, hilarious... especially the bits about Bally...😄 Brought back some good memories... great read🔴⚪️🔴⚪️"},{"1":"@DreddByDawn Yeah, it's not like there's an 'political side' that THEY SIDED WITH that is so anti-sexy clothes that they made Bally produce 'boob cover panels' to give the arcade owners an option to make the backglass art 'less racy' for pinball machines in the 80's and 90's (Elvira games)"},{"1":"Model Town Daska\\n•\\n#Bally_Bally #Dahi_Bhally #Gool_Gappy \\n#Daska #Model_Town_Daska #Pani_Puri \\n#Tasty #Crispy #Amazing #Delicious #Hungry #Fast_Food #Halal_Food #Food #FoodPorn #ohfoodstagram #Snack @ballybally_db \\n\\n#معیار #ذائقہ #نفاست https://t.co/qozdwyPD0S"},{"1":"Model Town Daska\\n•\\n#Bally_Bally #Dahi_Bhally #Gool_Gappy \\n#Daska #Model_Town_Daska #Pani_Puri \\n#Tasty #Crispy #Amazing #Delicious #Hungry #Fast_Food #Halal_Food #Food #FoodPorn #ohfoodstagram #Snack\\n•\\n@ballybally_db \\n#معیار #ذائقہ #نفاست https://t.co/bIC0eUeQ5m"},{"1":"Mohran bally Ty Lawan Gy Ch Amir Sultan Cheema and Ch Faisal Farooq Chee... https://t.co/YgdIWslJoC via @YouTube"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Bally**. slang usage not observed. not suitable.


### 33) Term: **Ballie **

```r
print_slang_tweets(33) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@NicdaSilvaFans @Wentworth @Foxtel @showcaseaus I'm still not over ballie 😭"},{"1":"@Angel_ballie The devil is a liar! 😱😱😱"},{"1":"@calliebo_ballie Oh shoot, I already have those flavors for herbalife and I don't drink Shakeology :( sorry!!"},{"1":"@KelsoJenkins @calliebo_ballie Yaaassss"},{"1":"ball up @ Ballie Ballerson https://t.co/Fj55R3mtSJ"},{"1":"“If you’re falling for someone then fuck the labels.” They deserved a better ending 😭😭😭 #ballie https://t.co/PSJEod2nqv"},{"1":"Ik heb zin in het nieuwe ballie seizoen"},{"1":"Meet the co-sponsors of the @CMA_Docs  #IndigenousHealth #communityofinterest @drsusanshaw  @DKimmaliardjuk Dr. Ballie Redfern @KirlewMichael @lexy_regush, mix of Indigenous and non-Indigenous physicians, learners and fellows.  Reach out through DM to #joinus #MDnotrequired!"},{"1":"@calliebo_ballie About time 🙌🏻"},{"1":"| Si te digo que pensé en el Ballie con el bofetón y ni recuerdo si Bea golpeó a Allie en algún momento"},{"1":"@Angel_ballie 😂😂😂 yangu tell me more ... did we twerk ? #Zfw"},{"1":"Happy birthday to this beautiful, bold, overhand-serving, peace-making 7-year old! Love you so much Allie bo ballie ❤️ @ Disneyland https://t.co/wtOlrFNOa1"},{"1":"@MarlanSoliar How the ballie used to raise his hand to you 😹"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Ballie **. slang usage not observed. not suitable.



### 34) Term: **Corn**

```r
print_slang_tweets(34) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"Whenever something is extremely cheesy or corny I just call it elote. It's literally corn covered in cheese."},{"1":"@seanmdav I used to go there a lot, ever since the first scare a few years ago I have not been back. My go to was rare steak, black bean, light on the rice and I love the corn stuff (and of course the guac)"},{"1":"We’re attending the John Deere LEAD conference this week, gathering agronomic and economic insights to share with you in upcoming stories. Sizing and Positioning Residue from harvest thru planting for best emergence, early growth and ear count in corn-on-corn. #corn #grow18 https://t.co/h8VNfSsLRY"},{"1":"I hope when @Nitascornerr makes a coffee it tastes like Bun https://t.co/WMz0G2sI11"},{"1":"girl virgin the best site for free porn lesbiean fucking soft corn porn syrian erotic film sex positions  https://t.co/1k7CXwAJGH"},{"1":"@jack_of_wands I've got my bread and corn, I'm ready."},{"1":"Can you recommend anyone for this #job? Store Associate, Part-time, Winners, Bells Corners - https://t.co/nbVdpHnhpj #Retail #Ottawa, ON #Hiring #CareerArc"},{"1":"@ItsMeTinaD I have to say... I was sitting having a beer about 50 metres away when a crowd came around the corner (I knew he was here due to searching about the police presence earlier) and I dashed down to see, never thought I would get that close (250mm away). I could have been anyone..."},{"1":"RAGA MA VI RENDETE CONTO CHE FRA POCO VEDREMO IL CONFRONTO FRA LARA E MICHAEL\\n\\nVOGLIO I POP CORN\\n#TemptationIsland"},{"1":"As long as the hail stays away, we will be putting these eggplant on tonight’s #burgernight, along with kimchi aioli &amp; a smoky corn &amp; black bean relish. https://t.co/uIOxepyUuZ"},{"1":"Great day at pottery painting camp 🎨 today at Ardcarne Garden Centre....the kids painted unicorns 🦄, dogs 🐕 &amp; cats🐱... And we even had time to decoupage 🌈 some pots. 😍😍"},{"1":"@pocusdorcus @PottsAndPlum Bless you, Ebbie. We were given a very thorough check over after our corn field wander - paws, ears, eyes, even unmentionables. We send gentle leans and lots of love. 💕 xx"},{"1":"Did you hear that, “Anobika sadza pa corner” a hard working man in this difficult economy left for dead. Itai henyu inguva chete. https://t.co/p0WTnCdAa2"},{"1":"I saw a whoooooole lot of corn today and it made me think of @Goobers515. #🌽"},{"1":"Meal: L.I.T, JC Fries, Chilled corn chowder soup, Roasted Pork over pimento cheese grits and German chocolate cake. #2018restaurantweek #dudleys #foodie #foodporn @ Lexington, Kentucky https://t.co/w7HYK8sWPe"},{"1":"@WMP_Myth United Corn of Wall when"},{"1":"Hey @CornwallHour I want to give a shout out to Jim at @Jimagination1 He makes fabulous bespoke wooden items, anything from coasters to chairs and also rather fabulous wooden signs which he laser cuts. I can't wait to get mine painted and put together. #cornwallhour"},{"1":"@Coolflare3 1) https://t.co/gYbqOxO7Zw\\nCan also use assists to meaty and not press anything for 10f DPs(j.A adds landing recov) can also swap to the assist to bait DP as well\\n\\n2) don't really need it for the corner, but it'll still work\\n\\n3) once you have the timing, it'll beat everything"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Corn**. slang usage not observed. not suitable.


### 35) Term: **Crash**

```r
print_slang_tweets(35) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"I don't have the patience for games like I use to, back in the day I completed the first crash bandicoot, now I look at it and I'm on literally the road to fucking nowhere. What happened to me. Lol"},{"1":"Video from Aeroméxico passenger shows plane crash and aftermath https://t.co/DfTdIlvAAA"},{"1":"#AeroMexico #Durango #Crash #POV #Landing #CrashLanding #Mexico https://t.co/ZUT5VJZ0ZP"},{"1":"New post (Bubba Wallace on his Pocono crash: 'I didn't know if I was going die or not') has been published on Nascar Fans - https://t.co/4vM0ENVWnW - https://t.co/mB2bFaQDJS https://t.co/FG7N9nTCtz"},{"1":"@NewsDayZimbabwe So we can conclude by saying chihuri was  far much better than matanga he never called the army to crash protestors. Ayidealer navo ega. Huori chete"},{"1":"Updated: Crash in Walton on I-10 west at MM 77, left lane blocked. Last updated at 03:14:16PM. https://t.co/5FadGyLJny"},{"1":"A 39-year-old Fort Payne, Alabama, woman was killed and two others were injured early Wednesday in a two-vehicle crash in neighboring Jackson County. https://t.co/IPWtxEXFsH"},{"1":"What I thought was lingering conference crud seems to actually be a delayed exertion crash, so please excuse my flakiness for a few weeks until my brain is fully functional again &lt;3"},{"1":"Updated: Crash in Walton on I-10 west at MM 77, left lane blocked. Last updated at 03:15:21PM. https://t.co/PbQyTk2bGF"},{"1":"I picked up a college girl my 20 year old little brother met on Tinder and let her crash at my house (my brother lives in another state) and she's such a cool woman I think I like my BROTHER  more as a person just for surrounding himself with cool independent women"},{"1":"We had this big fella crash our lunch. He took some tacos to go. https://t.co/9PQVCS4k4I"},{"1":"another great day with #startstaygrow☀️ thank you to to @gopuff and @vanguard innovation studio for letting us crash your Wednesday!! 😎 https://t.co/aA1jHwB1L4"},{"1":"Sternly Launching and Walking Edie Falco and Mario Crashes Paper Mario: Color Splash"},{"1":"@Singapore_crash アロンチョいないし💢💢💢💢💢💢💢"},{"1":"Driver flees deadly Volusia crash scene after getting out to look at victim, FHP says https://t.co/L9nMxsZ9I3 https://t.co/EzCkVvsAM9"},{"1":"Mexico plane crash: No deaths at Durango airport https://t.co/e9oKDca9tD"},{"1":"No fatalities in Mexican plane crash https://t.co/fCl4DYlyXR via @Global-Update-News"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Crash**. slang usage not observed. not suitable.



### 36) Term: **Creps**

```r
print_slang_tweets(36) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@jovan697 Bro sturdy creps on smart pants is awfullll LOOOOLL, and I agree with you. Can’t believe what these man make waves"},{"1":"Gofres hechos con amor y eso se nota, no sólo en su sabor sino también en su presentación, un poco artista es cada uno de nuestros Gofres \"chulos\"\\n#gofres #creps #muffins #gofrespresentacion #gofresrellenos https://t.co/Ott0nzZMO5"},{"1":"Sarsour gives me the creps https://t.co/Tg3aUXtfly"},{"1":"j’ai tellement hâte de commencer les creps là"},{"1":"Let me step into your shoes: You quit your job and let me run a building.\\n\\nWhat do you mean 'no'? I thought we were swapping creps?"},{"1":"What's worse black socks with white creps or white socks with black creps?"},{"1":"@adidasUK Hi there I’m ready to receive my free creps"},{"1":"@marinacisa Fer molt d'exercici aquesta setmana perque t'espera un finde de menjar gofres, patates fregides i creps 😛"},{"1":"Hmmm. What's that smell? It smells like a new IKEA Season #footfetish #creps #shoes #ikea @IKEAUK https://t.co/kUAYBMY9od"},{"1":"Ngl I'm proud I stopped buying creps"},{"1":"Séance matinale (7h) au CREPS PACA site d’Antibes pour Isaïa CORDINIER, Thomas DURAND  (Shark’s Antibes) et Kilian TILLIE (Gonzaga University - USA) 🏀 https://t.co/UklcaraU3E"},{"1":"might have to commit a fashionable crime and wear nike joggers with adidas creps today I cba x"},{"1":"I don’t wanna be famous I just want companies to send me creps"},{"1":"I guess it’s a god send no where in town had the creps I wanted because that would of set my back another 2 bills but im  pissed about that too tbh 😒"},{"1":"@tylerrayprosper wants some new summer creps and he needs your advice!\\n\\nTweet us some trainer suggestions? What's your favourite summer crep to wear with a White Tee?\\n\\nHit up the studio WhatsApp 07376 199 688 \\n\\n#MidMorningWithTy 10AM-1PM \\n\\nLISTEN LIVE:\\nhttps://t.co/Ns5XZs7xO8"},{"1":"Gotta shoot my shot to get some creps ykno https://t.co/gToYKDtWLR"},{"1":"Ya tenemos las tapas ganadoras de esta 7ª edición #Tapeando por #Bogarra en la #Sierradelsegura, y el diploma a la mejor tapa y la mejor presentación ha sido para Hotel Val de Pinares con su tapa: \"Creps de rabo de toro y boletus con crujiente de pistacho\". Enhorabuena!!! https://t.co/2RVUkpRitS"},{"1":"All what people know on this app is Alexandra McQueen creps, doesn’t your shoe game expand from those clown creps ? Lool"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Creps**. slang usage observed but not related to crime. not suitable

### 37) Term: **Case **

```r
print_slang_tweets(37) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"T2: functional and physiologic imaging often complements structural. Use it early and often This is a nice review of multimodal integration with case examples. #NeuroRad #PediNeuroRad #Epilepsy #AJNR\\nhttps://t.co/sAvhGmiYy6"},{"1":"@AngieBlimkie86 @BootsandHearts I will try and be at the merch area after the show if they’ll allow it! I’ll announce on stage if that’s the case :)"},{"1":"SOLD! Thank You! #Baby #SeaTurtleFabricToy #GalaxyS9 #case &gt; https://t.co/T5IZ8ln16m https://t.co/nyq5MRxUVd"},{"1":"That tweet has my tongue firmly in my cheek, just in case anyone thinks I am really being a whiny twat! 😉"},{"1":"@AppleSupport - ipad silicone pro case is tearing at corners, apple is saying its common wear and tear...case is under a year old."},{"1":"In case you missed today’s pick https://t.co/33b26H1IHc"},{"1":"My   application  //\\n  is   under   a   false   name   just  //\\n  in   case   they   hold   grudges  //\\n\\n- TimothyDaw"},{"1":"@jaiprakashshah2 @AmitShah @AmitShahOffice If WB say \"Nay\" That would call for a case of \" Administration can't be run I.A.W law... Calls for Prez rule' it's not healthy for any provincial Govt. @NSA_AjitDoval @rajnathsingh  one must know how Law is flexible @Swamy39 @sagenaradamuni strike the iron when red hot."},{"1":"Steve Madden Luggage 3 Piece Softside Spinner Suitcase Set Collection (One Size, Harlo Gray) -  USD - 249.99\\nhttps://t.co/x5Z2b1C0Og #new #newluggage #luggage #travel https://t.co/pWoQS2HYz4"},{"1":"❄️FROZEN YOGHURT BITES❄️🍓\\n\\nI whip these little mouthfuls of frozen yoghurt out for the kids as a dessert! They call them ‘yoghurt cakes’.\\nYoghurt in a silicone case topped with frozen berries of your choice, then freeze! boom! #yoghurt #kidssnacks #frozenyoghurt #dessert https://t.co/6XVrCLnkj1"},{"1":"The trims that loud I might go Angelica’s for cocktails on my ones, showcase the ting"},{"1":"Add a sterling silver Bow or Flower charm to a charm bracelet or chunky necklace to showcase your favorite charming memories. #VintageCostumeJewelry \\nhttps://t.co/3Tfb2yxgLH\\n(Tweeted via https://t.co/UgyBb8Ecem) https://t.co/wAywBX7tTa"},{"1":"@zachpettet Hello Zach, we'res sorry for the delay. We trust that our staff are working diligently to ensure safety is prioritize at all times. We invite you to review this link for a better insight on our re-protection policy in case of a flight disruption: https://t.co/h5AMcRh0ka . /Harry"},{"1":"#RoseanneBarr is a very bright person. She thinks, she teaches and she knows more than she is given credit for. We need to start listening to the many things she has told us over the years. In case you haven’t noticed, @therealroseanne keeps being proven right!"},{"1":"Indefinite detention of FAMILIES potentially for years, merely for crossing a border - which in any case except tribal territory denotes  land stolen by colonies/USA anyway. Disgusting in sooo many ways! https://t.co/Rk8Q2nRJDJ"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Case **. slang usage not observed. not suitable.


### 38) Term: **Chunky**

```r
print_slang_tweets(38) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"Have you heard ‘Chunky Money ft. Chucke Gunz’ by @papa_hnic on #SoundCloud? #np https://t.co/nRoGPnvpP1"},{"1":"Fluorite Necklace - Flower Pendant - Purple Green Gemstone Jewellery - Sterling Silver - Chunky Jewelry - Mod Flower - Multicolor https://t.co/nzFFZTo7IX via @Etsy"},{"1":"@oakobaresi77 Love a chunky man xx"},{"1":"Olive Branch Chunky Tapenade is a traditional recipe transformed into an exciting range of flavours  sourced from independent, artisan farmers around Greece. Use them to create effortless canapés or as a healthy and tasty ingredient for a quick &amp; simple dinner solution. https://t.co/uvBKQ7cI52"},{"1":"@K1Soulo Lool anything is better than having chunky juice 😭"},{"1":"Sphinx Signed Amber Glass Open Back Vintage Necklace - Mid Century modern - Chunky Angular Stones https://t.co/bmuXXMq1hf"},{"1":"Black &amp; White Bracelet - Pearl Gemstone Jewelry - Onyx Beaded Jewelery - Chunky - Sterling Silver - Fashion https://t.co/fazOh3dKPV via @Etsy"},{"1":"A good looking chunky boy 😍 https://t.co/jfZUXtoPbU"},{"1":"Chunky scares me to but I face my fears head on mouth and all 😂 https://t.co/glTSl1SqUs"},{"1":"Is this the same Dancing Party that Russ Abbot gatecrashed in his chunky jumper? \\n#Huaaaaaghhhh #xmcab"},{"1":"@Q13FOXKiggins What an adorable chunky monkey! 😍"},{"1":"Crochet Chunky Cowl tutorial https://t.co/RebP6188ba via @DIY Momma .Net"},{"1":"My baby breathes loud and hard bc she’s so chunky😂 I love it! It’s s’cute 😩😩💓💓💓💓💓"},{"1":"@emilytreveyxx My fav is your glowing I think this translates to getting well Chunky everywhere! 😂😂😂"},{"1":"my bf getting chunky and I love it.."},{"1":"Kit Kat Chunky is the Terry Crews of chocolate bars"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Chunky**. slang usage referring to violence not observed. not suitable.


### 39) Term: **Chase**

```r
print_slang_tweets(39) %>% rmarkdown:::print.paged_df()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["text"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"@Chase_Brody_ \"No.\""},{"1":"Chase your dreams and work hard every single day Free Thinkers"},{"1":"Just purchased my fidget spinner boards @CapnPetesPE !!  So excited!  What a great resource!"},{"1":"@DeanMarrYT @Birchletree @Laz3rC @Chase_Doggie @WigglytuffSSB @NathanRLouie @TanaIsMyWaifu @RiptideAC @JStringle @Aurum7HRN  https://t.co/GqVe5wbUHC"},{"1":".@WellsFargo @JPMorgan and @Chase: stop funding Trump’s heartless and inhumane detention machine that indefinitely detains immigrants and abuses them! https://t.co/m8lss9D47d https://t.co/RxO4KA8AQe"},{"1":"J1GOLD: [4:17] NOW PLAYING: Ouyang Fei Fei - Koi no Tsuiseki (Love Chase) (1972) ~ 13 listeners  #jpop #enka"},{"1":"@Chase_Retriever It’ll be way better than Green Lantern"},{"1":"Apple Pay Now Available at Nearly 16,000 Cardless Chase ATMs via @MacHashNews https://t.co/tp0XOPOWzS"},{"1":"When you hear the Mosquitos on WIFC make sure you text SPLASH to 39327 for your chance to win a pair of tickets to our Last Chance Splash Bash at Noah's Ark in Wisconsin Dells next Wednesday! You can also text ARK to 39327 to get a discount on tickets you purchase. ~Nikki"},{"1":"@JYSexton Watching yall chase your own tails like dogs as yall continue to hunt for Russian Ghosts is fun"},{"1":"China property: Authorities dashed hopes they're set to ease up on housing prices. Shenzhen imposed new 3yr min holding period. Local authorities also suspended home purchases by corps and orgs\\nCN CNHCYcredit Shcomp (-/=)"},{"1":"I am looking for a buyer on 2410 PINE CHASE CIR #SaintCloud #FL  #realestate https://t.co/ahRlzF0F1Q https://t.co/PrsbzUkWDI"},{"1":"hentai porn site naked girl iphone conners' adult adhd amber chase porn hotgirl porn tub teen fucked  https://t.co/lVTgDULDYl"},{"1":".@WellsFargo @JPMorgan and @Chase: stop funding Trump’s heartless and inhumane detention machine that indefinitely detains immigrants and abuses them! https://t.co/k64XPKua6V https://t.co/JY5ql8nBX2"},{"1":"Reading this article on a murder suicide in Astoria on Monday. Four ppl dead. Why does the article feel the need to say the guns were likely “purchased and carried legally.” Are the ppl any less dead??"},{"1":"Fuck the haters ,Ima chase a bag🤑"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

**Comments:** term used to query twitter API was **Chase**. slang usage referring to violence not observed. not suitable.

# 3. Gang slang from Ebony Reid's Doctoral Thesis (PDF)

I am only using the glossary of London gang slangs but the full thesis can be found here [https://bura.brunel.ac.uk/bitstream/2438/14817/1/FulltextThesis.pdf]. Rather than scraping tweets using rtweet, I will do the twitter search manually as is gives more control overall. The end result will be in the csv file.



```r
glossary_text <- 'Ackee- refers to a former road man who has converted to Islam
Baby mother- the mandem use the term to describe the mother of their child
Bad up- to treat someone in a disrespectful manner. It also refers to being
victimised or victimising others
Bait- being too obvious
Boy dem- the police
Bruk- refers to having no money
Bussing a skank- to dance
Fuckery- the mandem often used this term to describe criminal activity or
violence
Garms- clothing
Grind- refers to working hard in the illegal drug economy
Gwarning with tings- doing well on road
Head back lick off- shot in the head
Hype- exaggerated/over the top behaviour
Nuff- a lot
Prick- dick head/idiot
Stunting/Stunter- to show off, a person who shows off
Take set on you- refers to being targeted and, potentially victimised by rivals
from neighbouring estates
Wasteman- useless, poor, unsuccessful
Warring- to fight, ongoing conflict' #just copy paste from pdf

glossary <- glossary_text %>% 
    str_split(pattern = "\n") %>% #split rows
    .[[1]] %>%  #select elements in the list
    as.tibble()%>% #convert to a tibble
    separate(value, into=c("slang", "meaning"), sep="-") %>% #separate text into two columns using -
    filter(!is.na(meaning))#remove excess rows

write_csv(glossary, "../data/ereid_glossary.csv")
```


