---
opening: 'Dear Editor,'

closing: Yours sincerely,

address: '**Obj**: Submission of manuscript *Title* by ... Daniele Arosio'

fontsize: 12pt

return-address:

documentclass: letter

include-before: 
    \begin{letter}{
    | OBJ
    | non so}

include-after:
    \vspace{44pt}
    Yours sincerely,
    \vspace{24pt}
    \center{ \flushleft \includegraphics[scale=0.6]{$HOME/.pandoc/firma/firma.pdf}
                 \newline Daniele Arosio }
    \end{letter}

geometry: a4paper,mag=1000,truedimen,
    left=1.4truein,top=2truein,right=0.8truein,
    bottom=0.8truein

header-includes: 
    \topmargin -15mm
    \def\wsumathlogo{\hspace*{-28mm} \vspace{0mm} \includegraphics[scale=0.1]{/home/dan/docs/arte/Cover/cnr/logo.png}}
    \usepackage{fancyhdr}
    \usepackage{color}
    \definecolor{cnr}{cmyk}{1,.7,.08,.54}
    \pagestyle{fancy}
    \fancyhf{}
    \lhead{\wsumathlogo \color{cnr} \fontspec{GeosansLight} 
		\Large \hspace*{6mm} \vspace*{-10mm} National Research Council of Italy}
    \rhead{Institute of Biophysics \\
		{\scriptsize Bruno Kessler Foundation -- Via Sommarive 18 -- 38138 Trento, Italy\\
		Daniele Arosio -- +39 0461 314607 -- daniele.arosio@cnr.it}}
	

date: \date{\flushleft\today}

linestretch: 1.7

mainfont: Georgia
...

Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod
tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At
vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren,
no sea takimata sanctus est Lorem ipsum dolor sit amet.
