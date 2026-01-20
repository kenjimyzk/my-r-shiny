library(shiny)

# UI定義
ui <- fluidPage(
  titlePanel("中心極限定理 (CLT) のシミュレーション"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "dist",
        "母集団の分布:",
        choices = c(
          "一様分布 (Uniform)" = "unif",
          "指数分布 (Exponential)" = "exp",
          "ベータ分布 (Beta 0.5, 0.5)" = "beta"
        )
      ),

      sliderInput("n", "サンプルサイズ (n):", min = 1, max = 200, value = 5),

      sliderInput(
        "bins",
        "ヒストグラムのビンの数:",
        min = 10,
        max = 100,
        value = 50
      ),

      hr(),
      helpText(
        "サンプルサイズ n を大きくすると、元の分布がどのような形であっても、標本平均の分布は正規分布に近づきます。"
      )
    ),

    mainPanel(
      plotOutput("cltPlot")
    )
  )
)

# サーバーロジック
server <- function(input, output) {
  output$cltPlot <- renderPlot({
    # シミュレーション設定
    k <- 5000 # 試行回数
    n <- input$n # サンプルサイズ

    # 乱数生成とパラメータ設定
    if (input$dist == "unif") {
      # 一様分布 U(0,1): 平均=0.5, 分散=1/12
      mat <- matrix(runif(n * k), nrow = k)
      mu_pop <- 0.5
      sigma_pop <- sqrt(1 / 12)
      dist_name <- "一様分布"
    } else if (input$dist == "exp") {
      # 指数分布 Exp(1): 平均=1, 分散=1
      mat <- matrix(rexp(n * k, rate = 1), nrow = k)
      mu_pop <- 1
      sigma_pop <- 1
      dist_name <- "指数分布"
    } else {
      # ベータ分布 Beta(0.5, 0.5): U字型分布
      mat <- matrix(rbeta(n * k, shape1 = 0.5, shape2 = 0.5), nrow = k)
      mu_pop <- 0.5
      # Beta分散: alpha*beta / ((alpha+beta)^2 * (alpha+beta+1))
      var_pop <- (0.5 * 0.5) / (1^2 * 2) # = 0.125
      sigma_pop <- sqrt(var_pop)
      dist_name <- "ベータ分布"
    }

    # 各行（各試行）ごとの平均を計算
    sample_means <- rowMeans(mat)

    # Macの場合、日本語フォントを指定
    if (Sys.info()["sysname"] == "Darwin") {
      par(family = "HiraKakuProN-W3")
    }

    # ヒストグラムの描画
    hist(
      sample_means,
      breaks = input$bins,
      probability = TRUE,
      col = "lightblue",
      border = "white",
      main = paste0("標本平均の分布 (n = ", n, ", 母集団: ", dist_name, ")"),
      xlab = "標本平均",
      ylab = "確率密度"
    )

    # 理論的な正規分布曲線の追加 (中心極限定理の確認用)
    # 平均の標準誤差 SE = sigma / sqrt(n)
    se <- sigma_pop / sqrt(n)

    curve(
      dnorm(x, mean = mu_pop, sd = se),
      add = TRUE,
      col = "#E7553C",
      lwd = 3
    )

    legend(
      "topright",
      legend = c("シミュレーション", "理論的正規分布"),
      col = c("lightblue", "#E7553C"),
      lwd = c(10, 3),
      bty = "n"
    )
  })
}

# アプリケーションの実行
shinyApp(ui = ui, server = server)
