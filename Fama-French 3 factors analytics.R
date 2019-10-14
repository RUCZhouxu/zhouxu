#Fama-French三因子模型应用
#加载包
install.packages("tidyverse")
install.packages("lubridate")
install.packages("readxl")
install.packages("highcharter")
install.packages("tidyquant")
install.packages("timetk")
install.packages("tibbletime")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("scales")
install.packages("broom")
install.packages("purrr")
install.packages("tidyr")
install.packages("magrittr")
install.packages('DBI')
install.packages('RMySQL')
library(DBI)
library(RMySQL)
library(tidyverse)
library(lubridate)
library(readxl)
library(highcharter)
library(tidyquant)
library(timetk)
library(tibbletime)
library(quantmod)
library(PerformanceAnalytics)
library(scales)
library(broom)
library(purrr)
library(tidyr)
library(magrittr)

#数据导入
data<-read.csv("C://Users//Administrator//Desktop//Fama-French.csv",header=T) 
colnames(data)[1] <- "date"
data$date <- as.character(data$date)
for (i in 1:length(data$date)){data$date[i]<- paste(substr(data$date[i],1,4),substr(data$date[i],5,6),"01",sep="-")}
data$date <- as.Date(data$date,format="%Y-%m-%d")
head(data,3)
#日期处理
data%>%
  select(date) %>%
  mutate(date = lubridate::rollback(date)) %>%
  head(1)
data %>%
  select(date) %>%
  mutate(date = lubridate::rollback(date + months(1))) %>%
  head(1)
#完整处理
data<-
  read.csv("C://Users//Administrator//Desktop//Fama-French.csv",header=T) %>%
  rename(date = X1) %>%
  mutate_at(vars(-date), as.numeric) %>%
  mutate(date =
           ymd(parse_date_time(date, "%Y%m"))) %>%
  mutate(date = rollback(date + months(1)))
head(data, 3)
#将投资组合设置为8只股票的组合
#600000（浦发银行）权重20%
#600008（首创股份）加权15%
#600011（华能国际）加权15%
#600018（上港集团）加权10%
#600022（山东钢铁）加权10%
#600030（中信证券）加权10%
#600048（保利地产）加权10%
#600054（黄山旅游）加权10%

#提取数据库中数据

symbols<-c("600000","600008","600011","600018","600022","600030","600048","600054")
data1<-list()  #创建空数据框
for (i in 1:8){ 
  mydb= dbConnect(MySQL(),user='ktruc002', password='35442fed', dbname='cn_stock_quote', host='172.19.3.250') 
  SQL_statement<-paste("SELECT  `day`,`close` 
FROM `cn_stock_quote`.`daily_adjusted_quote`
WHERE code=",symbols[i],"and day >='2008-12-31'<='2018-09-30'
ORDER BY 'day' DESC ")
  aa <- dbGetQuery(mydb,SQL_statement)
  colnames(aa)[2]<-paste("x",symbols[i],sep="",collaspe="")
  data1[[i]]=aa
}
stockdata<-data1%>%reduce(merge)
  prices<-xts(stockdata[,-1],order.by = as.Date(stockdata[,1]))
#8项资产的比例，顺序与symbols中的相同
  w <- c(0.20,0.15,0.15,0.10,0.10,0.10,0.10,0.10)
  head(prices,3)
  
#使用tidyverse将数据转换为月度回报
  asset_returns_dplyr_byhand <-
    prices %>%
    to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
    #将索引转换为日期
    data.frame(date = index(.)) %>%
    #删除索引，因为它已转换为行名称
    remove_rownames() %>%
    gather(asset, prices, -date) %>%
    group_by(asset) %>%
    mutate(returns = (log(prices) - log(lag(prices)))) %>%
    select(-prices) %>%
    spread(asset, returns) 

  #除去NA项
  asset_returns_dplyr_byhand <-
    asset_returns_dplyr_byhand %>%
    na.omit()   
  #tidyverse要求使用long格式或整洁格式的数据，其中每个变量都有自己的列，而不是wide格式
  #为了使资产回报整洁，我们需要一个名为“date”的列、一个名为“asset”的列和一个名为“returns”的列
  #asset_returns_long有3列，每个列对应一个变量:日期、资产、回报
  asset_returns_long <-
    asset_returns_dplyr_byhand %>%
    gather(asset, returns, -date) %>%
    group_by(asset)
  #使用tq_portfolio()将asset_returns_long转换为投资组合回报
  portfolio_returns_tq_rebalanced_monthly <-
    asset_returns_long %>%
    tq_portfolio(assets_col = asset,
                 returns_col = returns,
                 weights = w,
                 col_rename = "returns",
                 rebalance_on = "months")
##回到第九章
  ##left_join使我们能够将这些数据对象合并在一起，然后将任何不匹配的行消除。我们还将FF数据转换为十进制格式，
  ##并创建一个新的列称为R_excess，使R_excess=returns - RF  
  ff_portfolio_returns <-
    portfolio_returns_tq_rebalanced_monthly %>%
    left_join(data, by ="date") %>%
    mutate(MKT_RF = RiskPremium,
           SMB =SMB,
           HML =HML,
           RF = Rf,
           R_excess = round(returns - Rf, 4)) %>%select(-returns, -Rf)
  ff_portfolio_returns <-
    ff_portfolio_returns%>%
    na.omit()   
##开始建模。用之前算出的portfolio return,三因子,通过lm函数开始拟合beta。此外，我们设置95％的系数的置信区间，然后重命名。
##使用来自broom包的tidy()函数清理结果
  
  ff_dplyr_byhand <-
    ff_portfolio_returns %>%
    do(model =
         lm(R_excess ~ MKT_RF + SMB + HML,
            data = .)) %>%
    tidy(model, conf.int = T, conf.level = .95) %>%
    rename(beta = estimate)
  ##转换数据格式，调整小数位。表格显示的分别是beta的预测值，标准差，pvalue和最高最低置信区间
  ff_dplyr_byhand %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    select(-statistic, -std.error)
  #通过ggplot将FF可视化
  ##添加置信区间,用filter函数过滤掉 “（Intercept）”）截距，通过geom_errorbar展现出带有误差棒的条形图。
  ##用ggplot2设置图表主题格式布局，包括标题，附注，坐标轴名称。通过theme设置主题。
  ff_dplyr_byhand %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    filter(term != "(Intercept)") %>%
    ggplot(aes(x = term,
               y = beta,
               shape = term,
               color = term)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low,
                      ymax = conf.high)) +
    labs(title = "FF 3-Factor Coefficients",
         subtitle = "balanced portfolio",
         x = "",
         y = "coefficient",
         caption = "data source: Fama-French website") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0))
  #回归结果显示，HML因子和MKT_RF因子贡献了主要的收益来源，而SMB因子的beta值基本在零左右，该因子对组合收益的贡献比较小。
  #### 使用tidyverse和tibble-time滚动Fama-French
  #检验滚动式Fama-French结果并探索该模型在不同时间段内情况。
  #以下工作特定用于FF，但通常可以应用于我们希望滚动拟合的任何多元线性回归模型。
  #我们首先使用tibbletime中的rollify（）函数定义一个滚动模型
  # 选择 24个月作为滚动窗口
  window <- 24
  # define a rolling ff model with tibbletime
  rolling_lm <-
    rollify(.f = function(R_excess, MKT_RF, SMB, HML) {
      lm(R_excess ~ MKT_RF + SMB + HML)
    }, window = window, unlist = FALSE)
  #####我们将资产组合的收益率导入rolling函数
  rolling_ff_betas <-
    ff_portfolio_returns %>%
    mutate(rolling_ff =
             rolling_lm(R_excess,
                        MKT_RF,
                        SMB,
                        HML)) %>%
    slice(-1:-23) %>%
    select(date, rolling_ff)
  head(rolling_ff_betas, 3)
  
  #现在，我们有了一个名为rolling_ff_betas的新数据框，我们可以使用map（rolling_ff，tidy）函数整理rolling_ff列，
  ##然后使用unnest语法将map类型展开成多行，这与CAPM的工作非常相似，只是FF有多个自变量。
  rolling_ff_betas <-
    ff_portfolio_returns %>%
    mutate(rolling_ff =
             rolling_lm(R_excess,MKT_RF,
                        SMB,
                        HML)) %>%
    mutate(tidied = map(rolling_ff,
                        tidy,
                        conf.int = T)) %>%
    unnest(tidied) %>%
    slice(-1:-23) %>%
    select(date, term, estimate, conf.low, conf.high) %>%
    filter(term != "(Intercept)") %>%
    rename(beta = estimate, factor = term) %>%
    group_by(factor)
  head(rolling_ff_betas, 3)
  ###现在，我们3个因子中的每一个都有滚动的beta和置信区间。
  #我们可以应用相同的代码逻辑来提取模型的滚动R2，唯一的区别是我们称为glance（）而不是tidy（）。
  rolling_ff_rsquared <-
    ff_portfolio_returns %>%
    mutate(rolling_ff =
             rolling_lm(R_excess,
                        MKT_RF,
                        SMB,
                        HML)) %>%
    slice(-1:-23) %>%
    mutate(glanced = map(rolling_ff,
                         glance)) %>%
    unnest(glanced) %>%
    select(date, r.squared, adj.r.squared, p.value)
  head(rolling_ff_rsquared, 3)
  #我们已经提取了滚动beta和滚动模型结果，现在我们进行可视化。
  #### 使用ggplot将Fama-French可视化
  #我们首先使用ggplot（）绘制滚动因子beta图像。这使我们对每个因子的解释能力如何随时间变化有了直观上的感觉
  #：设x轴为时间轴，y轴变量为beta，用折线图对24个月的滚动因子beta进行可视化。
  #图形描述的是滚动回归计算的beta值，揭示了一些有趣的趋势。总体上看，SMB和HML都徘徊在零附近，而MKT因子则徘徊在1左右。
  ##这与我们的带有置信区间的beta曲线一致。同时。HML因子MB因子eta值波动比较大，而MKT_RF因子的beta值则波动比较小。
  rolling_ff_betas %>%
    ggplot(aes(x = date,
               y = beta,
               color = factor)) +
    geom_line() +
    labs(title= "24-Month Rolling FF Factor Betas") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90))
  #现在，我们对滚动R2进行可视化
  #我们首先使用timetk中的tk_xts（）函数将R2转换为xts对象
  rolling_ff_rsquared_xts <-
    rolling_ff_rsquared %>%
    tk_xts(date_var = date, silent = TRUE)
  highchart(type = "stock") %>%
    hc_add_series(rolling_ff_rsquared_xts$r.squared,
                  color = "cornflowerblue",
                  name = "r-squared") %>%hc_title(text = "Rolling FF 3-Factor R-Squared") %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_navigator(enabled = FALSE) %>%
    hc_scrollbar(enabled = FALSE) %>%
    hc_exporting(enabled = TRUE)
  #虽然结果图比较不稳定，但R2从未真正离开介于0.9到0.95这个区间。我们可以调整y轴的最小和最大值。
  highchart(type = "stock") %>%
    hc_add_series(rolling_ff_rsquared_xts$r.squared,
                  color = "cornflowerblue",name = "r-squared") %>%
    hc_title(text = "Rolling FF 3-Factor R-Squared") %>%
    hc_yAxis( max = 2, min = 0) %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_navigator(enabled = FALSE) %>%
    hc_scrollbar(enabled = FALSE) %>%
    hc_exporting(enabled = TRUE)
  
  #如上图所示，当y轴范围扩大时，我们的R2在投资组合的有效期内波动性总体小了很多。具体来看，2017年之前，
 # R-Squared的值基本在0.7以上，说明三因子模型对组合收益率的解释力度很好，但是，2018年开始，
  #R-Squared的值快速下降，最低降至0.3以下，说明这段时期三因子模型对组合的解释力度比较差。
  
  
  
  
  
  
  
  
  
  
  


