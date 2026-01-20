library(shiny)

# UI定義
ui <- fluidPage(
  titlePanel("IS-LM モデル分析"),

  sidebarLayout(
    sidebarPanel(
      h4("財市場 (IS曲線)"),
      helpText("IS式: Y = C + I + G"),
      numericInput("C0", "基礎消費 (C0)", value = 100),
      numericInput("I0", "独立投資 (I0)", value = 100),
      sliderInput("G", "政府支出 (G)", min = 0, max = 1000, value = 200),
      sliderInput("T", "租税 (T)", min = 0, max = 1000, value = 100),
      sliderInput(
        "c",
        "限界消費性向 (c)",
        min = 0.1,
        max = 0.95,
        value = 0.8,
        step = 0.05
      ),
      numericInput("b", "投資の利子弾力性 (b)", value = 20),

      hr(),
      h4("貨幣市場 (LM曲線)"),
      helpText("LM式: M/P = L(Y, r)"),
      sliderInput(
        "M",
        "名目マネーサプライ (M)",
        min = 100,
        max = 2000,
        value = 600
      ),
      numericInput("P", "物価水準 (P)", value = 1, min = 0.1, step = 0.1),
      numericInput("k", "貨幣需要の所得感応度 (k)", value = 0.5, step = 0.1),
      numericInput("h", "貨幣需要の利子感応度 (h)", value = 20, step = 5)
    ),

    mainPanel(
      plotOutput("islmPlot"),
      verbatimTextOutput("equilibrium_text"),
      wellPanel(
        h5("モデルの方程式:"),
        p("IS曲線: r = (A - (1-c)Y) / b"),
        p("LM曲線: r = (kY - M/P) / h"),
        tags$small("ただし、A (独立支出) = C0 + I0 + G - cT")
      )
    )
  )
)

# サーバーロジック
server <- function(input, output) {
  # 均衡点の計算を行うリアクティブ式
  calc_equilibrium <- reactive({
    # パラメータの取得
    C0 <- input$C0
    I0 <- input$I0
    G <- input$G
    T_val <- input$T
    c_val <- input$c
    b <- input$b
    M <- input$M
    P <- input$P
    k <- input$k
    h <- input$h

    # 独立支出 A
    A <- C0 + I0 + G - (c_val * T_val)

    # 均衡国民所得 Y* の導出
    # IS: r = (A - (1-c)Y) / b
    # LM: r = (kY - M/P) / h
    # 連立方程式を解く:
    # (A - (1-c)Y) / b = (kY - M/P) / h
    # h(A - (1-c)Y) = b(kY - M/P)
    # hA - h(1-c)Y = bkY - b(M/P)
    # hA + b(M/P) = Y * (bk + h(1-c))

    numerator <- (h * A) + (b * (M / P))
    denominator <- (b * k) + (h * (1 - c_val))

    Y_star <- numerator / denominator

    # 均衡利子率 r* の導出 (LM式に代入)
    r_star <- ((k * Y_star) - (M / P)) / h

    list(Y = Y_star, r = r_star, A = A)
  })

  output$islmPlot <- renderPlot({
    eq <- calc_equilibrium()

    # 表示範囲の動的設定
    xmax <- max(eq$Y * 1.5, 1000)
    ymax <- max(eq$r * 1.5, 10) # 最低でも10%までは表示
    if (ymax < 0) {
      ymax <- 10
    } # 負の場合の調整

    # Mac用の日本語フォント設定
    if (Sys.info()["sysname"] == "Darwin") {
      par(family = "HiraKakuProN-W3")
    }

    # キャンバスの作成
    plot(
      0,
      0,
      type = "n",
      xlim = c(0, xmax),
      ylim = c(0, ymax),
      xlab = "国民所得 (Y)",
      ylab = "利子率 (r, %)",
      main = "IS-LM 分析: 均衡点のシフト"
    )
    grid()

    # IS曲線の描画
    # r = (A - (1-c)x) / b
    curve(
      (eq$A - (1 - input$c) * x) / input$b,
      from = 0,
      to = xmax,
      add = TRUE,
      col = "blue",
      lwd = 3
    )

    # LM曲線の描画
    # r = (kx - M/P) / h
    curve(
      (input$k * x - (input$M / input$P)) / input$h,
      from = 0,
      to = xmax,
      add = TRUE,
      col = "red",
      lwd = 3
    )

    # 均衡点のプロット
    points(eq$Y, eq$r, pch = 19, cex = 2, col = "black")

    # ラベル追加
    text(
      eq$Y,
      eq$r,
      labels = paste0("E\n(", round(eq$Y, 1), ", ", round(eq$r, 1), "%)"),
      pos = 4,
      offset = 1
    )

    legend(
      "topright",
      legend = c("IS曲線 (財市場)", "LM曲線 (貨幣市場)"),
      col = c("blue", "red"),
      lwd = 3,
      bg = "white"
    )
  })

  output$equilibrium_text <- renderText({
    eq <- calc_equilibrium()
    paste0(
      "均衡国民所得 (Y*): ",
      round(eq$Y, 2),
      "\n",
      "均衡利子率 (r*): ",
      round(eq$r, 2),
      " %"
    )
  })
}

# アプリケーションの実行
shinyApp(ui = ui, server = server)
