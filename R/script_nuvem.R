# Nuvem de palavras
# R 4.0.1
# Renata Muylaert 2020

if(!require(rvest)) install.packages("rvest", dependencies = TRUE)
if(!require(wordcloud)) install.packages("wordcloud", dependencies = TRUE)
if(!require(wordcloud2)) install.packages("wordcloud2", dependencies = TRUE)
if(!require(RColorBrewer)) install.packages("RColorBrewer", dependencies = TRUE)

library(rvest) # baixa infos da web
library(wordcloud) # cria nuvens simples
library(RColorBrewer) # paletas legais
library(htmlwidgets) # salva em html
library(webshot) # ajuda a salvar

webshot::install_phantomjs()

s <- read_html("https://marcoarmello.wordpress.com/2020/08/19/coronavirus6/")

t <- html_text(html_nodes(s, 'p,h1,h2,h3'))

tc <- do.call("paste", c(as.list(t[1:37]), sep = " "))

tcp <- gsub("[[:punct:]]", "", tc)

tcpl <- tolower(tcp)

tsplit <- unlist(strsplit(tcpl, " "))

corpus <- data.frame(table(tsplit))

corpus <- corpus[rev(order(corpus$Freq)),]

corpus$tsplit <- as.character(corpus$tsplit)

remover <- nchar(corpus$tsplit) < 3

corpus <- corpus[!remover,]

padroes_indesejados <- c("como", "que", "com", "uma", "para", "sobre", "por", "até")

corpus <- corpus[!corpus$tsplit %in% padroes_indesejados,]

# Criando a nuvem

wordcloud(words = corpus$tsplit,
                    freq = corpus$Freq,
                    min.freq = 1,
                    max.words = 500,
                    random.order = FALSE,
                    rot.per= 0.35,
                    colors = brewer.pal(8, "Dark2"))

# Verificando em qual pasta você vai salvar sua nuvem

getwd()

# Exportando a nuvem simplesinha

tiff(filename = "nuvem1.tif", width = 20, height = 10, res = 600, units = "cm")
wordcloud(words = corpus$tsplit,
          freq = corpus$Freq,
          min.freq = 1,
          max.words = 500,
          random.order = FALSE,
          rot.per= 0.35,
          colors = brewer.pal(8, "Dark2"))
dev.off()

# Criando uma nuvem com wordcloud2

library(wordcloud2)

nuvem2 <- wordcloud2(data = corpus,
                 size = 1.4,
                 color='random-dark',
                 backgroundColor = "mistyrose",
                 minRotation = 0.2,
                 rotateRatio = 0.8)

# Visualizando a nuvem

nuvem2

# Salvando a nuvem em html e png

saveWidget(nuvem2, "nuvem2.html", selfcontained = F)

webshot("nuvem2.html", file = "nuvem2.png", cliprect = "viewport")

# Para salvar a nuvem por clique (caso a função webshot não funcione no seu PC
# por alguma limitação de administradores), clique em Export > Save as image > Save)


