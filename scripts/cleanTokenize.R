options( java.parameters = "-Xmx4g" )
library(RWeka)
library(tm)
library(SnowballC)
library(dplyr)
library(ggplot2)
options(scipen=999)

# build corpus:
setwd("/Users/steffenhartleib/Google_Drive/Capstone/corpus/samples")
con = file.path("/Users/steffenhartleib/Google_Drive/Capstone/corpus/samples")
docs <- Corpus(DirSource(con))
dir()

#TEST ONLY
setwd("/Users/steffenhartleib/desktop/testCorpus") # test
cont <- file.path("/Users/steffenhartleib/desktop/testCorpus")
docs <- Corpus(DirSource(cont))

# write functions to clean data: 

removePunct1 <- content_transformer(function(file) gsub("[^[:alpha:]['’-]", " ", file)) # remove no's and punctuation except "-" and "'"
removePunct2 <- content_transformer(function(file) gsub("['’-](?=\\s)|(?<=\\s)['’-]"," ", file ,perl=TRUE))#remove "-" and "'" with spaces around it # remove no's and punctuation except "-" and "'"
removePunct3 <- content_transformer(function(file) gsub("-[^[:alpha:]]"," ", file ,perl=TRUE))
removePunct4 <- content_transformer(function(file) gsub("[^[a-z]-"," ", file ,perl=TRUE))
removePunct5 <- content_transformer(function(file) gsub("\\s['’-]"," ",file, perl = TRUE))
removePunct6 <- content_transformer(function(file) gsub("^['’-]"," ",file, perl = TRUE))
removePunct7 <- content_transformer(function(file) gsub("[\\[\\]]"," ", file, perl = TRUE))
fixUS <- function(file) gsub("\\su\\ss\\s"," u.s. ", file, perl = TRUE)
fixUSA <- function(file) gsub("\\su\\ss\\sa\\s"," u.s.a. ", file, perl = TRUE)
fixUK <- function(file) gsub("\\su\\sk\\s"," u.k. ", file, perl = TRUE)
removeProf <-  content_transformer(function(file) { gsub(("a55|a55hole|aeolus|ahole|anal|analprobe|anilingus|anus|areola
                                                          |areole|arian|aryan|ass|assbang|assbanged|assbangs|asses|assfuck
                                                          |assfucker|assh0le|asshat|assho1e|ass hole|assholes|assmaster
                                                          |assmunch|asswipe|asswipes|azazel|azz|b1tch|babe|babes|ballsack
                                                          |bang|banger|barf|bastard|bastards|bawdy|beaner|beardedclam|beastiality
                                                          |beatch|beater|beaver|beer|beeyotch|beotch|biatch|bigtits|big tits|bimbo
                                                          |bitch|bitched|bitches|bitchy|blow job|blow|blowjob|blowjobs|bod|bodily
                                                          |boink|bollock|bollocks|bollok|bone|boned|boner|boners|bong|boob|boobies
                                                          |boobs|booby|booger|bookie|bootee|bootie|booty|booze|boozer|boozy|bosom|bosomy
                                                          |bowel|bowels|bra|brassiere|breast|breasts|bugger|bukkake|bullshit|bull shit
                                                          |bullshits|bullshitted|bullturds|bung|busty|butt|butt fuck|buttfuck|buttfucker
                                                          |buttfucker|buttplug|c.0.c.k|c.o.c.k.|c.u.n.t|c0ck|c-0-c-k|caca|cahone|cameltoe
                                                          |carpetmuncher|cawk|cervix|chinc|chincs|chink|chink|chode|chodes|cl1t|climax
                                                          |clit|clitoris|clitorus|clits|clitty|cocain|cocaine|cock|c-o-c-k|cockblock
                                                          |cockholster|cockknocker|cocks|cocksmoker|cocksucker|cock sucker|coital
                                                          |commie|condom|coon|coons|corksucker|crabs|crack|cracker|crackwhore|crap
                                                          |crappy|cum|cummin|cumming|cumshot|cumshots|cumslut|cumstain|cunilingus
                                                          |cunnilingus|cunny|cunt|cunt|c-u-n-t|cuntface|cunthunter|cuntlick|cuntlicker
                                                          |cunts|d0ng|d0uch3|d0uche|d1ck|d1ld0|d1ldo|dago|dagos|dammit|damn|damned|damnit
                                                          |dawgie-style|dick|dickbag|dickdipper|dickface|dickflipper|dickhead|dickheads
                                                          |dickish|dick-ish|dickripper|dicksipper|dickweed|dickwhipper|dickzipper|diddle
                                                          |dike|dildo|dildos|diligaf|dillweed|dimwit|dingle|dipship|doggie-style
                                                          |doggy-style|dong|doofus|doosh|dopey|douch3|douche|douchebag|douchebags|douchey
                                                          |drunk|dumass|dumbass|dumbasses|dummy|dyke|dykes|ejaculate|enlargement|erect
                                                          |erection|erotic|essohbee|extacy|extasy|f.u.c.k|fack|fag|fagg|fagged|faggit
                                                          |faggot|fagot|fags|faig|faigt|fannybandit|fart|fartknocker|felch|felcher|felching
                                                          |fellate|fellatio|feltch|feltcher|fisted|fisting|fisty|floozy|foad
                                                          |fondle|foobar|foreskin|freex|frigg|frigga|fubar|fuck|f-u-c-k|fuckass
                                                          |fucked|fucked|fucker|fuckface|fuckin|fucking|fucknugget|fucknut|fuckoff|fucks
                                                          |fucktard|fuck-tard|fuckup|fuckwad|fuckwit|fudgepacker|fuk|fvck|fxck|gae|gai
                                                          |ganja|gay|gays|gey|gfy|ghay|ghey|gigolo|glans|goatse|godamn|godamnit|goddam
                                                          |goddammit|goddamn|goldenshower|gonad|gonads|gook|gooks|gringo|gspot|g-spot
                                                          |gtfo|guido|h0m0|h0mo|handjob|hard on|he11|hebe|heeb|hell|hemp|herp
                                                          |herpes|herpy|hitler|hiv|hobag|hom0|homey|homo|homoey|honky|hooch|hookah|hooker
                                                          |hoor|hootch|hooter|hooters|horny|hump|humped|humping|hussy|hymen|inbred|incest
                                                          |injun|j3rk0ff|jackass|jackhole|jackoff|jap|japs|jerk|jerk0ff|jerked|jerkoff
                                                          |jism|jiz|jizm|jizz|jizzed|junkie|junky|kike|kikes|kill|kinky|kkk|klan|knobend
                                                          |kooch|kooches|kootch|kraut|kyke|labia|lech|leper|lesbo|lesbos|lez|lezbian
                                                          |lezbians|lezbo|lezbos|lezzie|lezzies|lezzy|lmao|lmfao|loin|loins|lube|lusty
                                                          |mams|massa|masterbate|masterbating|masterbation|masturbate|masturbating
                                                          |masturbation|maxi|menses|meth|m-fucking|mofo|molest
                                                          |moolie|moron|motherfucka|motherfucker|motherfucking|mtherfucker|mthrfucker
                                                          |mthrfucking|muff|muffdiver|murder|muthafuckaz|muthafucker|mutherfucker
                                                          |mutherfucking|muthrfucking|nad|nads|naked|napalm|nappy|nazi|nazism|negro
                                                          |nigga|niggah|niggas|niggaz|nigger|nigger|niggers|niggle|niglet|nimrod
                                                          |ninny|nipple|nooky|nympho|opiate|opium|oral|orally|organ|orgasm|orgasmic|orgies
                                                          |orgy|ovary|ovum|ovums|p.u.s.s.y.|paddy|paki|pcp|pecker|pedo|pedophilia
                                                          |pedophiliac|pee|peepee|penetrate|penial|penile|penis|perversion|peyote
                                                          |phalli|phallic|phuck|pillowbiter|pimp|pinko|piss|pissed|pissoff|piss-off
                                                          |pms|polack|pollock|poon|poontang|porn|porno|pornography|pot|potty|prick
                                                          |prig|prostitute|prude|pube|pubic|pubis|punkass|punky|puss|pussies|pussy
                                                          |pussypounder|puto|queaf|queef|queef|queer|queero|queers|quicky|quim|racy
                                                          |rape|raped|raper|rapist|raunch|rectal|rectum|rectus|reefer|reetard|reich
                                                          |retard|retarded|revue|rimjob|ritard|rtard|r-tard|rum|rump|rumprammer|ruski
                                                          |s.h.i.t.|s.o.b.|s0b|sadism|sadist|scag|scantily|schizo|schlong
                                                          |screw|screwed|scrog|scrot|scrote|scrotum|scrud|scum|seaman|seamen|seduce
                                                          |semen|sex|sexual|sh1t|s-h-1-t|shamedame|shit|s-h-i-t|shite|shiteater|shitface
                                                          |shithead|shithole|shithouse|shits|shitt|shitted|shitter|shitty|shiz|sissy
                                                          |skag|skank|slave|sleaze|sleazy|slut|slutdumper|slutkiss|sluts|smegma|smut
                                                          |smutty|snatch|sniper|snuff|s-o-b|sodom|souse|soused|sperm|spic|spick|spik|spiks
                                                          |spooge|spunk|steamy|stfu|stiffy|stoned|strip|stroke|stupid|suck|sucked
                                                          |sucking|sumofabiatch|t1t|tampon|tard|tawdry|teabagging|teat|terd|teste|testee
                                                          |testes|testicle|testis|thrust|thug|tinkle|tit|titfuck|titi|tits|tittiefucker
                                                          |titties|titty|tittyfuck|tittyfucker|toke|toots|tramp|transsexual|trashy|tubgirl|
                                                          |turd|tush|twat|twats|ugly|undies|unwed|urinal|urine|uterus|uzi|vag|vagina|valium|
                                                          |viagra|virgin|vixen|voyeur|vulgar|vulva|wad|wang|wank|wanker|wazoo
                                                          |wedgie|weed|weenie|weewee|weiner|weirdo|wench|wetback|wh0re|wh0reface|whitey
                                                          |whiz|whoralicious|whore|whorealicious|whored|whoreface|whorehopper|whorehouse
                                                          |whores|whoring|wigger|womb|woody|wop|wtf|x-rated|xxx|yeasty|yobbo|zoophile"),"",
                                                         file)
})



#file <- gsub("\\si\\s" , " I ", file, perl = TRUE) # fix "I"
#file <- gsub("\\si'"," I'", file, perl = TRUE) #  fix "I'"
#file <- gsub("^i\\s","I ", file, perl = TRUE)  # fix "I" at beginning of string
#file <- gsub("^i'","I'", file, perl = TRUE)  # fix "I'" at beginning of string

#apply function to files in corpus:
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removePunct1)
docs <- tm_map(docs, removePunct2)
docs <- tm_map(docs, removePunct3)
docs <- tm_map(docs, removePunct4)
docs <- tm_map(docs, removePunct5)
docs <- tm_map(docs, removePunct6)
docs <- tm_map(docs, removePunct7)
docs <- tm_map(docs, content_transformer(fixUSA))
docs <- tm_map(docs, content_transformer(fixUS))
docs <- tm_map(docs, content_transformer(fixUK))
docs <- tm_map(docs, removeProf)
docs <- tm_map(docs,stripWhitespace)
#docs <- tm_map(docs, content_transformer(UNK))

# Words in a data frame
tdm <- TermDocumentMatrix(docs)
tdmMatrix <- as.matrix(tdm)
freq <- rowSums(tdmMatrix)
df.freq = as.data.frame(freq)
df.freq$terms <- row.names(df.freq)

# Write words  to file
setwd("/Users/steffenhartleib/Google_Drive/Capstone/corpus/clean/tokens")
write.csv(df.freq,"words.csv")


#biGrams in a data frame

options(mc.cores=1)
biGramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 2, max = 2,delimiters = " "))       
tdm2 <- TermDocumentMatrix(docs, control = list(tokenize = biGramTokenizer))
tdm2Matrix <- as.matrix(tdm2)
freq <- rowSums(tdm2Matrix)
df.biGram = as.data.frame(freq)
df.biGram$terms <- row.names(df.biGram)
write.csv(df.biGram,"biGrams.csv")
head(df.biGram,100)


# triGrams in a dataframe
options(mc.cores=1)
triGramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 3, max = 3, delimiters = " "))       
tdm3 <- TermDocumentMatrix(docs, control = list(tokenize = triGramTokenizer))
tdm3Matrix <- as.matrix(tdm3)
freq <- rowSums(tdm3Matrix)
df.triGram = as.data.frame(freq)
df.triGram$terms <- row.names(df.triGram)
write.csv(df.triGram,"triGrams.csv")

# 4grams in a data frame
options(mc.cores=1)
fourGramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 4, max = 4, delimiters = " "))      
tdm4 <- TermDocumentMatrix(docs, control = list(tokenize = fourGramTokenizer))
tdm4Matrix <- as.matrix(tdm4)
freq <- rowSums(tdm4Matrix)
df.4Gram <- as.data.frame(freq)
df.4Gram$terms <- row.names(df.4Gram)
write.csv(df.4Gram,"4Grams.csv")

# 5grams as data frame REPLICATE 4gram script - then do 2 and 3 grams
options(mc.cores=1)
fiveGramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 5, max = 5, delimiters = " "))
tdm5 <- TermDocumentMatrix(docs, control = list(tokenize = fiveGramTokenizer))
tdm5Matrix <- as.matrix(tdm5)
freq <- rowSums(tdm5Matrix)
df5 <- as.data.frame(freq)
df5$terms <- row.names(df5)
write.csv(df5,"5Grams.csv")






