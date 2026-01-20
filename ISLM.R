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
      tabsetPanel(
        tabPanel(
          "財市場 (45度線分析)",
          plotOutput("goodsPlot", width = "600px", height = "600px")
        ),
        tabPanel(
          "貨幣市場",
          plotOutput("moneyPlot", width = "600px", height = "600px")
        ),
        tabPanel(
          "IS-LM分析",
          plotOutput("islmPlot", width = "600px", height = "600px")
        )
      ),
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
    # 入力値が揃っているか確認（数値入力欄が空の場合のエラー防止）
    req(
      input$C0,
      input$I0,
      input$G,
      input$T,
      input$c,
      input$b,
      input$M,
      input$P,
      input$k,
      input$h
    )

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

  # 共通の日本語フォント設定関数
  set_font <- function() {
    if (Sys.info()["sysname"] == "Darwin") {
      par(family = "HiraKakuProN-W3")
    }
  }

  # 1. 財市場 (45度線分析)
  output$goodsPlot <- renderPlot({
    eq <- calc_equilibrium()
    set_font()

    # 総支出 AE = C + I + G
    # C = C0 + c(Y - T)
    # I = I0 - b * r (ここで r は均衡利子率を使用)
    I_val <- input$I0 - (input$b * eq$r)
    Intercept <- input$C0 - (input$c * input$T) + I_val + input$G

    xmax <- max(eq$Y * 1.5, 1000)
    ymax <- xmax # 45度線なので正方形に近い方が見やすい

    plot(
      0,
      0,
      type = "n",
      xlim = c(0, xmax),
      ylim = c(0, ymax),
      xlab = "国民所得 (Y)",
      ylab = "総支出 (AE)",
      main = "財市場の均衡 (45度線分析)"
    )
    grid()
    abline(0, 1, col = "gray", lty = 2) # 45度線

    # AE曲線: AE = Intercept + cY
    curve(Intercept + input$c * x, add = TRUE, col = "blue", lwd = 2)

    # 均衡点
    points(eq$Y, eq$Y, pch = 19, col = "black", cex = 1.5)
    text(eq$Y, eq$Y, labels = paste0("Y* = ", round(eq$Y, 0)), pos = 4)
    legend(
      "topleft",
      legend = c("45度線 (Y=AE)", "総支出 (AE)"),
      col = c("gray", "blue"),
      lty = c(2, 1),
      lwd = 2
    )
  })

  # 2. 貨幣市場
  output$moneyPlot <- renderPlot({
    eq <- calc_equilibrium()
    set_font()

    # 横軸: 名目マネーサプライ M, 縦軸: 利子率 r
    # 貨幣需要 (名目): Md = P * (kY - hr) => r = (kY - Md/P)/h
    # グラフ用逆関数 r(M) = (kY/h) - (1/Ph)*M

    # 描画範囲
    M_max <- max(input$M * 1.5, 1000)
    r_max <- max(eq$r * 2, 10, na.rm = TRUE)
    if (r_max < 0) {
      r_max <- 10
    }

    plot(
      0,
      0,
      type = "n",
      xlim = c(0, M_max),
      ylim = c(0, r_max),
      xlab = "名目貨幣供給量 (M)",
      ylab = "利子率 (r, %)",
      main = "貨幣市場の均衡"
    )
    grid()

    # 貨幣供給曲線 (垂直)
    abline(v = input$M, col = "red", lwd = 3)

    # 貨幣需要曲線 (所与のY*の下での需要)
    # r = (k*Y* - M/P) / h
    # 変数xは名目貨幣量Mに相当
    curve(
      (input$k * eq$Y - (x / input$P)) / input$h,
      from = 0,
      to = M_max,
      add = TRUE,
      col = "green",
      lwd = 2
    )

    # 均衡点
    points(input$M, eq$r, pch = 19, col = "black", cex = 1.5)
    text(input$M, eq$r, labels = paste0("r* = ", round(eq$r, 1), "%"), pos = 4)
    legend(
      "topright",
      legend = c("貨幣供給 (Ms)", "貨幣需要 (L)"),
      col = c("red", "green"),
      lwd = c(3, 2)
    )
  })

  output$islmPlot <- renderPlot({
    eq <- calc_equilibrium()

    # 表示範囲の動的設定
    ymax <- max(eq$r * 1.5, 10, na.rm = TRUE) # 最低でも10%までは表示
    if (is.na(ymax) || ymax < 0) {
      ymax <- 10
    } # 負またはNAの場合の調整

    # 表示範囲の上限
    xmax <- max(eq$Y * 1.5, 1000, na.rm = TRUE)

    # Mac用の日本語フォント設定
    set_font()

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
