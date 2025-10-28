#!/bin/bash

SOURCE="$HOME/vigolana-MM/PHOTOs/PICTURE-fino09/PIETRO/selAGO09"
DESTINATION="$HOME/FamilyMedia"

mkdir -p "$DESTINATION/Movies" "$DESTINATION/Photos"

find "$SOURCE" -type f | while read -r file; do
  echo "Processing: $file"
  year_month=$(exiftool -d "%Y-%m" -DateTimeOriginal "$file" | cut -f2 -d ":" | sed 's/^ //' 2> /dev/null)
  day_time=$(exiftool -d "%d_%H%M%S" -DateTimeOriginal "$file" | cut -f2 -d ":" | sed 's/^ //' 2> /dev/null)

  type=$(echo "$file" | grep -iE "\.jpg|\.jpeg")
  if [[ ! -z "$type" ]]; then
    target_dir="$DESTINATION/Photos/$year_month"
  else
    target_dir="$DESTINATION/Movies/$year_month"
  fi
  mkdir -p "$target_dir"

  extension="${file##*.}"
  new_name="${day_time}.${extension}"
  # Ensure unique filename
  counter=1
  while [ -e "$target_dir/$new_name" ]; do
    new_name="${day_time}_$counter.${extension}"
    ((counter++))
  done

  cp "$file" "$target_dir/$day_time"
done
