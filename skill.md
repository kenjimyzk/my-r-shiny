# Shiny アプリでの日本語フォント設定 (macOS対策)

macOS 上で R のプロット（特に Base R の `plot()` や `hist()`）を行う際、日本語が「トーフ（□）」のように文字化けすることがあります。これを防ぐために、OS が macOS であるかを判定し、適切な日本語フォント（ヒラギノ角ゴシックなど）を指定します。

## 実装パターン

### Base R プロットの場合

`renderPlot` 内で描画を行う**直前**に、`par()` 関数を使ってフォントファミリーを指定します。

```r
# 共通関数として定義しておくと便利です
set_font <- function() {
  if (Sys.info()["sysname"] == "Darwin") {
    par(family = "HiraKakuProN-W3") # ヒラギノ角ゴシック ProN W3
  }
}

server <- function(input, output) {
  output$myPlot <- renderPlot({
    
    # プロット作成前に実行
    set_font()
    
    # プロット描画
    plot(
      x, y,
      main = "日本語のタイトル",
      xlab = "日本語のX軸",
      ylab = "日本語のY軸"
    )
  })
}
```

### ggplot2 の場合 (参考)

`ggplot2` を使用する場合は、`theme()` 関数内でフォントを指定します。

```r
library(ggplot2)

# Mac用フォント設定
font_family <- if (Sys.info()["sysname"] == "Darwin") "HiraKakuProN-W3" else ""

ggplot(data, aes(x, y)) +
  geom_point() +
  labs(title = "日本語タイトル") +
  theme_gray(base_family = font_family) # テーマにフォントを適用
```

## 注意点

*   この設定は macOS (`Darwin`) に特化しています。Windows や Linux ではデフォルトで表示されることが多いですが、表示されない場合は `Meiryo` (Win) や `Noto Sans CJK JP` (Linux) などを指定する必要があります。
*   より汎用的な解決策として `showtext` パッケージを使用する方法もありますが、パッケージのインストールが必要です。
