# remotes::install_github("jhelvy/renderthis")
# renderthis::to_pdf("__NOME_DEL_FILE__.html")

pagedown::chrome_print(input="index.html", 
                       output="slides.pdf",
                       options = list(paperWidth = 11, paperHeight = 8.5))




