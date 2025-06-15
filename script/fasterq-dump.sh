#!/bin/bash
set -e
THREADS=8

[ -f "SRR_Acc_List.txt" ] || { echo "SRR_Acc_List.txt was not found."; exit 1; }

echo "Start downloading..."

while read -r SRR; do
  echo "Processing: $SRR"

  # SRAファイルを先に取得
  prefetch "$SRR"

  mkdir -p "$SRR" && cd "$SRR"

  # 取得した .sra ファイルを指定して展開
  fasterq-dump --split-files -e "$THREADS" --progress "../ncbi/public/sra/${SRR}.sra"
  pigz -p "$THREADS" "${SRR}_1.fastq" "${SRR}_2.fastq"
  rm "${SRR}_1.fastq" "${SRR}_2.fastq"

  cd ..
done < SRR_Acc_List.txt

echo "Done."
