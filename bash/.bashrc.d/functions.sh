# shellcheck shell=bash

cp2marzola_mm() {
    rsync -avzP -e 'ssh -p 23456' "$*" mmedia@marzola:/home/MM/
}

povo() {
    if [ -e Povo ]; then rm Povo; fi
    wget https://www.yr.no/en/print/forecast/2-6618631/Italy/Trentino-Alto%20Adige/Province%20of%20Trente/Povo
    okular Povo
}

TNv() {
    rm valanghe_it.pdf
    date="$(date -I)"
    wget https://avalanche.report/albina_files/"$date"/"$date"_it.pdf
    okular "$date"_it.pdf
}

grepp() {
 if test -z "$1" || test -z "$2" ; then
   echo "USAGE: grepp searchterm
     filetosearch";
 else
   perl -00ne "print if /$1/i" < "$2"
 fi }

fss() {
    firefox "http://google.com/search?q=$(xsel -p -o)&ie=UTF-8&oe=UTF-8"
}

ed() {
    if [ "$#" -lt 2 ]; then
        echo "USAGE: ediff <FILE 1> <FILE 2>"
    else
        quoted1=$(printf "%s" "$1" | sed 's/\\/\\\\/g; s/"/\\"/g')
        quoted2=$(printf "%s" "$2" | sed 's/\\/\\\\/g; s/"/\\"/g')
        emacsclient -c -a emacs -e "(ediff \"$quoted1\" \"$quoted2\")"
    fi
}

ranger_cd() {
    tempfile="$(mktemp -t tmp.XXXXXX)"
    /usr/bin/ranger --choosedir="$tempfile" "${@:-$(pwd)}"

    if [ -f "$tempfile" ] && [ "$(cat -- "$tempfile")" != "$(pwd -P)" ]; then
        cd -- "$(cat "$tempfile")" || return 1
    fi

    rm -f -- "$tempfile"
}

pdf_myReduce() {
    gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dNOPAUSE -dQUIET -dBATCH -dPDFSETTINGS=/ebook -sOutputFile="$2" "$1"
}
# -dDownsampleColorImages=true -dColorImageResolution=150
pdf_myReduce2() {
    gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.5 -dFILTERIMAGE -dNOPAUSE -dQUIET -dBATCH -dPDFSETTINGS=/screen -sOutputFile="$2" "$1"
}
