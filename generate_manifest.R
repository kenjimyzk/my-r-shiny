if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}

# Shinyアプリのメインファイル名を指定（エラーログに基づき "example.R" に変更）
app_primary_doc <- "app.R"

if (file.exists(app_primary_doc)) {
  # メインファイルを明示的に指定して manifest.json を生成する
  # これにより、他のファイルが誤って Quarto ドキュメントとして判定されるのを防ぎます
  rsconnect::writeManifest(appDir = ".", appPrimaryDoc = app_primary_doc)

  if (file.exists("manifest.json")) {
    message("成功: manifest.json が生成されました。")
  } else {
    stop("エラー: manifest.json が生成されませんでした。")
  }
} else {
  warning(paste0(
    "メインファイル '",
    app_primary_doc,
    "' が見つかりません。ファイル名を確認してください。"
  ))
  # フォールバック: ファイル指定なしで実行（エラーが再発する可能性があります）
  rsconnect::writeManifest(appDir = ".")
}
