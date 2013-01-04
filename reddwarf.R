# RED DWARF SCRIPT ANALYSIS
# Myles Harrison

library(tm)

# Read into a corpus
corpus <- Corpus(DirSource('./scripts', '*.txt'))

# Perform processing
print('Processing corpus...')
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

# Create Term Document Matrix
print('Processing terms....')
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)

# Get terms
words <- tdm$dimnames$Terms

# MAIN CHARACTER TERMS
# Lister terms
listerTerms <- c('dave','davy','daves','davey','david','lister','listers','listy','listys','listie')
lister <- colSums(m[listerTerms,])

# Rimmer terms
rimmerTerms <- c('rimmer','rimmers','rimmerss','rimmsy','rimmsie','rimsy','rimmerworld','arnold','arn','arnie','arniell','arnolds')
rimmer <- colSums(m[rimmerTerms,])

# Holly terms
hollyTerms <- c('holly', 'hol', 'holl', 'hollys')
holly <- colSums(m[hollyTerms,])

# Cat terms
catTerms <- c('cat', 'cats')
cat <- colSums(m[catTerms,])

# Kryten terms
krytenTerms <- c('kryten','kryt','krytie','kryts')
kryten <- colSums(m[krytenTerms,])

# Kochanski terms
kochanskiTerms <- c('christine', 'kristine', 'kris', 'kochanski', 'kochansi', 'kochanskis', 'krissie','krissies')
kochanski <- colSums(m[kochanskiTerms,])

termsMatrix <- rbind(lister, rimmer, holly, cat, kryten, kochanski)

# OTHER CHARACTER TERMS
#hollister, chen, selby, petersen, ackerman, baxter
others <- m[c('hollister','chen','selby','petersen','ackerman','baxter','ace','birdman','pete'), ]

#non-humans
#skutters
skutterTerms <- c('skutter','skutters','bob','bobs')
skutters <- colSums(m[skutterTerms,])

#simulants
simulantTerms <- c('simulant','simulants')
simulants <- colSums(m[simulantTerms,])

#gelf
gelfTerms <- c('gelf','gelfs')
gelf <- colSums(m[gelfTerms,])

# other non-humans
nonhumans <- m[c('dispenser', 'toaster', 'queeg','legion','camille','cassandra', 'able','hudzen','epideme','polymorph'),]

termsMatrix <- rbind(termsMatrix, others, skutters, nonhumans, simulants, gelf)

# OTHER TERMS
time <- m['time',] # time / time travel 
space <- m['space',] # space
# starbug
starbugTerms <- c('starbug', 'starbugs', 'starbuf') 
starbug <- colSums(m[starbugTerms,])
# the dwarf
dwarfTerms <- c('dwarf','dwarfs','dwarves')
dwarf <- colSums(m[dwarfTerms,])
# slang
smegTerms <- c('smeg','smegging','smeghead', 'smegged','smegger', 'smegheads', 'smegs')
smeg <- colSums(m[smegTerms,])
git <- m['git',]
# vindaloo
vindalooTerms <- c('vindaloo','vindaloos','vinadloovian','vindaloovian','vindalooed', 'curry')
vindaloo <- colSums(m[vindalooTerms,])
# lager
lagerTerms <- c('beer','beers','beerski','lager','pint')
lager <- colSums(m[lagerTerms,])

termsMatrix <- rbind(termsMatrix, time, space, dwarf, starbug, smeg, git, vindaloo, lager)
termsMatrix <- t(termsMatrix)

# Fix row names
row.names(termsMatrix) <- strsplit(row.names(termsMatrix), '.txt')

# Output to file
print('Writing output...')
write.csv(termsMatrix, file='term_data.csv')
print('Done.')
