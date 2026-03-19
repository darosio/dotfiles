# shellcheck shell=bash

# AI containers (~/ai-containers/{searxng,vane,khoj,...})
aic() {
  local base="$HOME/ai-containers"
  local services="searxng vane khoj mcp-searxng"
  local action="${1:-help}" svc="${2:-}"

  case "$action" in
    up)
      for s in ${svc:-$services}; do
        [ -f "$base/$s/podman-compose.yml" ] || continue
        echo "Starting $s..."
        (cd "$base/$s" && podman-compose up -d)
      done
      ;;
    down)
      for s in ${svc:-$services}; do
        [ -f "$base/$s/podman-compose.yml" ] || continue
        echo "Stopping $s..."
        (cd "$base/$s" && podman-compose down)
      done
      ;;
    restart)
      aic down "$svc"
      aic up "$svc"
      ;;
    ps)
      podman ps -a --format 'table {{.Names}}\t{{.Status}}\t{{.Ports}}'
      ;;
    logs)
      [ -z "$svc" ] && echo "Usage: aic logs <service>" && return 1
      podman logs -f "$svc"
      ;;
    *)
      echo "Usage: aic {up|down|restart|ps|logs} [service]"
      echo "Services: $services"
      echo "  aic up          — start all"
      echo "  aic up vane     — start one"
      echo "  aic down khoj   — stop one"
      echo "  aic ps          — status"
      echo "  aic logs searxng"
      ;;
  esac
}

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
  if test -z "$1" || test -z "$2"; then
    echo "USAGE: grepp searchterm
     filetosearch"
  else
    perl -00ne "print if /$1/i" < "$2"
  fi
}

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

pdfllm() {
  local model="" template="" args=()
  local OPTIND opt
  while getopts "m:t:h" opt; do
    case "$opt" in
      m) model="$OPTARG" ;;
      t) template="$OPTARG" ;;
      *)
        echo "Usage: pdfllm [-m model] [-t template] <file.pdf> [prompt]"
        echo "  -m  model (default: llm default, set with: llm models default)"
        echo "  -t  template (e.g. fabric:extract_wisdom)"
        return 1
        ;;
    esac
  done
  shift $((OPTIND - 1))
  if [ -z "$1" ]; then
    echo "Usage: pdfllm [-m model] [-t template] <file.pdf> [prompt]"
    return 1
  fi
  [ -n "$model" ] && args+=(-m "$model")
  [ -n "$template" ] && args+=(-t "$template")
  local pdf="$1"
  shift
  python -c "
import sys, pymupdf
doc = pymupdf.open(sys.argv[1])
print('\n'.join(p.get_text() for p in doc))
" "$pdf" | llm "${args[@]}" "$@"
}

pdffabric() {
  local pattern="extract_wisdom" model=""
  local OPTIND opt
  while getopts "p:m:h" opt; do
    case "$opt" in
      p) pattern="$OPTARG" ;;
      m) model="$OPTARG" ;;
      *)
        echo "Usage: pdffabric [-p pattern] [-m model] <file.pdf>"
        echo "  -p  fabric pattern (default: extract_wisdom)"
        echo "  -m  model (default: fabric default, set with: fabric -d)"
        return 1
        ;;
    esac
  done
  shift $((OPTIND - 1))
  if [ -z "$1" ]; then
    echo "Usage: pdffabric [-p pattern] [-m model] <file.pdf>"
    return 1
  fi
  python -c "
import sys, pymupdf
doc = pymupdf.open(sys.argv[1])
print('\n'.join(p.get_text() for p in doc))
" "$1" | fabric ${model:+--model "$model"} --pattern "$pattern"
}

ragask() {
  local model="" collection="biblio" limit=10
  local OPTIND opt
  while getopts "m:c:l:h" opt; do
    case "$opt" in
      m) model="$OPTARG" ;;
      c) collection="$OPTARG" ;;
      l) limit="$OPTARG" ;;
      *)
        echo "Usage: ragask [-m model] [-c collection] [-l chain-limit] <prompt>"
        echo "  -m  model (default: llm default model)"
        echo "  -c  RAG collection (default: biblio)"
        echo "  -l  chain limit (default: 10)"
        return 1
        ;;
    esac
  done
  shift $((OPTIND - 1))
  if [ -z "$1" ]; then
    echo "Usage: ragask [-m model] [-c collection] [-l chain-limit] <prompt>"
    return 1
  fi
  local args=()
  [ -n "$model" ] && args+=(-m "$model")
  args+=(-T "RAG(\"$collection\")" --chain-limit "$limit")
  llm "${args[@]}" "$*"
}

pdfembed() {
  local collection="biblio" model="qwen3-embedding"
  local OPTIND opt
  while getopts "c:m:h" opt; do
    case "$opt" in
      c) collection="$OPTARG" ;;
      m) model="$OPTARG" ;;
      *)
        echo "Usage: pdfembed [-c collection] [-m model] <dir|file.pdf...>"
        echo "  -c  llm collection name (default: biblio)"
        echo "  -m  embedding model (default: qwen3-embedding)"
        return 1
        ;;
    esac
  done
  shift $((OPTIND - 1))
  if [ -z "$1" ]; then
    echo "Usage: pdfembed [-c collection] [-m model] <dir|file.pdf...>"
    return 1
  fi
  local tmpfile
  tmpfile="$(mktemp)"
  trap 'rm -f "$tmpfile"' RETURN
  local files=()
  for arg in "$@"; do
    if [ -d "$arg" ]; then
      while IFS= read -r -d '' f; do
        files+=("$f")
      done < <(find "$arg" -name '*.pdf' -print0 | sort -z)
    else
      files+=("$arg")
    fi
  done
  if [ ${#files[@]} -eq 0 ]; then
    echo "No PDF files found"
    return 1
  fi
  for f in "${files[@]}"; do
    local id
    id="$(basename "$f" .pdf)"
    python -c "
import sys, pymupdf
doc = pymupdf.open(sys.argv[1])
print('\n'.join(p.get_text() for p in doc))
" "$f" > "$tmpfile"
    llm embed "$collection" "$id" -m "$model" -i "$tmpfile" --store
    echo "embedded: $id"
  done
  echo "Done — ${#files[@]} files into collection '$collection'"
}

tree_size() {
  if [ -z "$1" ]; then
    echo "Usage: tree-size <level>"
    echo "       level: The maximum depth to display folder sizes."
    return 1
  fi

  level="$1"
  find . -maxdepth "$level" -type d -exec bash -c 'depth=$(echo "$1" | awk -F/ "{print NF-1}"); indent=$(printf "%$((depth*4))s" ""); size=$(du -sh "$1" 2>/dev/null | awk "{print \$1}"); name=$(basename "$1"); echo -e "${indent}├── [ ${size:-    }]  ${name}";' _ {} \; | sed 's/├/│/g; s/└/├/g; $!N;s/│   /│   /; s/├/└/; s/\[ \]/[    ]/'
}
