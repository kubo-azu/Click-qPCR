# Easy-qPCR: ユーザーガイド（日本語版）

## 概要

Easy-qPCRはリアルタイムPCR (qPCR) データの直接的な解析のために設計された，ユーザーフレンドリーなShinyウェブアプリケーションです。**本ツールは、ローカルへのインストールを必要とせず，ウェブブラウザから直接 [https://kubo-azu.shinyapps.io/Easy-qPCR/](https://kubo-azu.shinyapps.io/Easy-qPCR/) にて利用可能です。

** Ct値を含む簡潔なCSVファイルをアップロードすることで，ΔCtおよびΔΔCtの計算，統計処理と作図に加えて，統計サマリーと論文品質のプロットの両方をダウンロードすることができます。ローカルでの実行や改変を希望するユーザーのために，ソースコードも利用可能です（下記の「インストールと使用法」セクションを参照ください）。

本ツールは一般的なqPCRデータ解析ワークフローを簡素化し，広範なプログラミング知識を必要とせずに研究者がよりアクセスしやすくすることを目的としています。

**[任意: 本ツールが論文に関連付けられている場合は、ここに一文追加してください。例: 「本ツールは、私たちのXXXX論文（アクセプトされ次第リンク追加）で説明されています。」]**


## 特徴

* **インタラクティブなデータアップロード:** CSV形式のqPCRデータを簡単にアップロードできます。
    * テンプレートCSVファイルをダウンロードできます。

* **データプレビュー:** アップロードしたデータの先頭部分を表示します。

* **ΔCt解析:**
    * リファレンス遺伝子とターゲット遺伝子を選択します。
    * 比較のためのコントロール群と処置群を選択します。
    * 相対発現量 (2<sup>-ΔCt</sup>) を計算します。
    * 2群間の統計的有意差についてウェルチのt検定を実行します。
    * 結果を平均値 ± 標準偏差を示す棒グラフとして可視化し，個々のデータポイントを重ねて表示します。

* **ΔΔCt解析:**
    * メインのΔCt解析セクションで選択されたリファレンス遺伝子を自動的に使用します。
    * ターゲット遺伝子，コントロール群，処置群を選択します。
    * コントロール群に対するfoldーchange値 (2<sup>-ΔΔCt</sup>) を計算します。
    * 統計的有意差についてWelchのt検定を実行します。
    * 結果を平均fold-change ± 標準偏差を示す棒グラフとして可視化し，個々のデータポイントを重ねて表示します。

* **解析結果のダウンロード:**
    * 統計概要テーブルをCSV形式でダウンロードできます。
    * プロットをPNG形式でダウンロードできます。


## インストールと使用法

### 必要なもの

* R (バージョン 4.4.2 以降を推奨)
* RStudio (使いやすさのため推奨しますが、Rコンソールから実行する場合は必須ではありません)
* 以下のRパッケージ（およびそれらの依存パッケージ）:
    * `shiny`
    * `dplyr`
    * `ggplot2`
    * `tidyr`
    * `DT`
    * `RColorBrewer`

これらのパッケージは、Rで以下のようにインストールできます:

```R
install.packages(c("shiny", "dplyr", "ggplot2", "tidyr", "DT", "RColorBrewer"))
```


### アプリケーションの実行

Option 1: GitHubから直接実行

Easy-qPCRは、RまたはRStudio内で shiny::runGitHub() 関数を使用してGitHubから直接実行できます:

```R
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
shiny::runGitHub("kubo-azu/Easy-qPCR")
```


Option 2: リポジトリをローカルにクローンする

1. このリポジトリをユーザのローカルマシン（お使いのPC）にクローンします:

```sh
git clone [https://github.com/kubo-azu/Easy-qPCR.git](https://github.com/kubo-azu/Easy-qPCR.git)
```

2. Rでクローンしたディレクトリに移動するか，RStudioで Easy-qPCR.Rproj ファイルを開きます。

3. `renv` を使用している場合（再現性のために推奨）は、R環境を復元します:

```R
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()
```

4. アプリケーションを実行します:

```R
shiny::runApp()
```

## アップロード用ファイルの形式について

CSVファイルとして，以下に示すの4つの列を持つデータを準備してください:

 - sample: Unique sample identifier (e.g., Mouse_A, CellLine_1).
 - group: Experimental group or condition (e.g., Control, Treatment_X).
 - gene: Gene name (e.g., Gapdh, Actb, YourGeneOfInterest).
 - Ct: Threshold Cycle value (numeric).

※ Excelでやるのが簡単です

各行は、1つのサンプルにおける1つの遺伝子のCt値を表すようにしてください。

Ct値にテクニカルリプリケートがある場合，それらの平均値をご自身で計算し，本ツールではその平均値を使用してください。

テンプレートCSVはEasy-qPCRのサイドバーからダウンロード可能です。

## How to Use

1. データのアップロード:「Upload CSV File」をクリックしてデータファイルを選択するか，「Use Example Data」をクリックしてサンプルデータセットをロードします。アップロードされたデータのプレビューが表示されます。

2. ΔCt Analysis:
 - リファレンス遺伝子（ハウスキーピング等）を選択 (e.g., Gapdh)
 - ターゲット遺伝子を選択
 - "Group 1" を選択（通常はコントロールとする群）
 - "Group 2" を選択（通常は処置群）
 - "Analyze" をクリック

3. ΔΔCt Analysis:
 - リファレンス遺伝子については，最初に選択したものが自動的に使用されます
 - ターゲット遺伝子を選択
 - Fold-change値の計算時に基準としたい群を選択
 - Fold-change値の計算対象としたい群を選択
 - "Run ΔΔCt Analysis" をクリック


## ライセンス

Easy-qPCRはMIT Licenseに従います。（See the LICENSE file for details）
