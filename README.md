# descriptius
Algunes funcions útils per obtenir descriptius a R

## Taula de freqüències adaptada al català

FREQ(x, w, user.missing, plot = FALSE,...)

Aqueta funció és la traducció al català de la funció freq() del paquet descr. El paquet es carrega automàticament.

## Descriptius de variables numèriques, es pot ponderar

elmeudesc(x,w=NULL)

## Descriptius de les variables d'una pregunta múltiple

multfreqtable(data, question.prefix, valor)

A question.prefix s'espera una cadena de text a passar a grep(). Valor és el valor a comptar.

## Taula de contingència (percentatges) en què se selecciona una de les opcions de resposta d'una columna.

fertaula(df,x,y,seleccio=NULL)

A selecció s'espera una cadena amb els valors de les columnes (y) a seleccionar.
