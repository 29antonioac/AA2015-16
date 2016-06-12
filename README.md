# AA2015-16
Aprendizaje Automático de la Universidad de Granada

Están tanto los cuestionarios como los trabajos compilados en pdf. Los cuestionarios están en formato LaTeX y los trabajos están escritos con R Markdown.

Para compilar los cuestionarios, sólo hay que ejecutar `pdflatex archivo.tex`, y para compilar los trabajos hay que hacer desde una sesión de R

```R
library(knitr)
knit("fichero.Rmd")
```

O bien abrirlo con Rstudio y pulsar "Knit PDF".
