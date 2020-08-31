# Nuvem de palavras
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

s <- read_html("https://marcoarmello.wordpress.com/2020/08/19/coronavirus6/") #você pode trocar pela URL do texto que pretende analisar

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

# Criando a nuvem simples

wordcloud(words = corpus$tsplit,
                    freq = corpus$Freq,
                    min.freq = 1,
                    max.words = 500,
                    random.order = FALSE,
                    rot.per= 0.35,
                    colors = brewer.pal(8,'Dark2'))

# Veja em qual pasta salvará as nuvens

getwd()

# Exportando a nuvem simplezinha

tiff(filename = "nuvem1.tif", width = 20, height = 10, res = 600, units = "cm")
wordcloud(words = corpus$tsplit,
          freq = corpus$Freq,
          min.freq = 1,
          max.words = 500,
          random.order = FALSE,
          rot.per= 0.35,
          colors = brewer.pal(8, "Dark2"))
dev.off()

# Criando uma nuvem mais elaborada com wordcloud2

library(wordcloud2)

nuvem2 <- wordcloud2(data = corpus,
                 size = 1.4,
                 color=brewer.pal(11, "Set3"),
                 backgroundColor = "darkcyan",
                 minRotation = 0.2,
                 rotateRatio = 0.8)

# Visualizando a nuvem elaborada

nuvem2

# Salvando a nuvem elaborada em html e png

saveWidget(nuvem2, "nuvem2.html", selfcontained = F)

webshot("nuvem2.html", file = "nuvem2.png", cliprect = "viewport")

# Para salvar a nuvem por clique (caso a função webshot não funcione no seu PC por alguma limitação de administradores), clique em Export > Save as image > Save)


