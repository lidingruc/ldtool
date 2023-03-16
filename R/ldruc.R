
##################################################
# 在数据分析中常用的一些包和函数
##################################################
# stata命令可能是目前统计软件中最简洁的
# R功能相当强大，但输出简单，频数表和列联表等常见输出实现起来代码量非常大
# 这非常不友好
# 统计了自己最近撰写的几千行stata命令，发现下面的主命令使用最频繁：
# tab tabout reg
# replace rename label clonevar recode gen drop destring
# foreach local ci
# 其中gen keep drop replace merge等用data.table或dplyr就好
# reg也很简单
# 但recode、label在R中不算很友好
# 常见recode函数比较
# car::recode
# fct_recode
# expss::recode 类似spss，功能复杂 ~  %thru% 按不够简洁
# sjmisc::rec  沿袭spss风格，比较简洁，最好，可以数字变因子
# sjlabelled 包中关于变量标签和取值标签的函数
#* set_label   # var_labels
#* get_labels
#* set_labels  # val_labels

# 最常用tab，在R中要实现同样的输出，代码量大，最不好用
# 在stata中 tab意味着 tab1 tab2 tabstat tabout都是很分类汇总相关的命令
# 是频次表、列联表、分类汇总的核心
# tab y;tab y x ,(row,col,cell);table a b,c(mean c);weight;by

# 比较了几个常见的R包中频次表、连列表、分类分总函数，试图找到最简洁的
# 初步发现expss包做频次表和列联表最方便，分类汇总表格可使用tidyverse来做
# 未来可以尝试开发与stata的tab命令媲美的R包，整合这几个函数
# 使用最少参数做出最常用表格，提升R代码制作常见表格的效率

# -------------------------------------------
#   分类汇总描述性统计包
#   程序包    / 简洁度 /效率与结果  / 稳健性 /   加权  /
# ------------/--------/------------/----------------- / --------/
# base        / 简洁   /高 + 基础   /  很好  /   无     / table xtabs ftable
# tidyverse   / 适中   /高 + 基础   /   很好 /    wt   /count summarise
# crosstable  / 适中   /高 + 表好   /  一般  /    无   /crosstable 1-2-3
# gtsummary   / 复杂   /高 + 表好   /  很好  /         /tbl_summary  tbl_cross
# janitor     / 简洁   /不知 + 好   /  很好  /    无   / tabyl 1-2-3
#**expss      / 简洁   /高 + 表好   /  一般  / weight  /fre cro_cases cro_cpct
#* sjPlot     / 简洁   /高 + 图好   /  较好  / weights /frq(1) sjtab(2)
#summarytools / 适中   /较好        /  较好  / weights /freq ctable descr
##################################################


# 加载包
library(tidyverse)  # dplyr 管道函数
library(magrittr)   # for %>%  %$%
library(data.table)
library(janitor)    # 清理变量名 clean_names 得到方便R使用的变量名
library(sjmisc)     # for rec 因子变量重新编码，可以将数字变量编码为分类变量
library(sjlabelled)
library(sjstats)
library(MASS)
library(sjPlot)
library(DT)          # 用于在Viewer里面展示表格 暂时不用
library(expss)       # https://github.com/gdemin/expss
library(summarytools)# 描述性分析包
library(ggplot2)


# 自定义函数名，少写一些字
#' @title recode variable using rec in sjmisc.
#' @name rec
#' @description 用来进行变量的重编码.映射规则比较简单。
#' @export
rec  <-  sjmisc::rec

#' @title make frequency table using sjmisc library.
#' @name freq
#' @description 用来制作频数表，使用sjmisc::frq函数
#' @export
freq  <-  sjmisc::frq

#' @title make frequency table using expss library
#' @name fre
#' @description 使用expss::fre制作频数表
#' @export
fre  <- expss::fre

st_options(ctable.prop = "n")
#tabn <- sjPlot::tab_xtab# expss::cro_cases # tab of cases n
#' @title make  count table using ctable function in summarytools library.
#' @name tabf
#' @description 使用summarytools::ctable制作简单的频数表
#' @export
tabf <- summarytools::ctable

#tabf <- expss::cro_cases

#' @export
group_by <- dplyr::group_by

#' @title make frequency graph using plot_frq in sjPlot.
#' @name gtab
#' @description 使用sjPlot::plot_frq制作频数条形图
#' @export
gtab <- sjPlot::plot_frq # 单变量条形图

# 设定默认参数后其他参数没法设定了

## 自定义一个查看数据基本信息（变量名、标签、类型）的函数

#' @title Describe the data and display the varible name,label,type and position information in dataview.
#' @name des
#' @description 创建一个新dataframe描述数据集的变量名、变量标签及格式信息。
#' @param dfile a dataframe.
#' @export
#'
des <- function (dfile) {
  lbl = sapply(dfile, attr, 'label')
  fmt = sapply(dfile, class)
  if (is.list(lbl)) {
    lbl[sapply(lbl, is.null)] = ''
    lbl[sapply(lbl, length) > 1] = ''
    lbl = unlist(lbl)
  }
  Encoding(lbl) = 'UTF-8'

  if (is.list(fmt)) {
    fmt[sapply(fmt, is.null)] = ''
    fmt[sapply(fmt, length) > 1] = 'double' # 加标签的都默认为数字型
    fmt = unlist(fmt)
  }

  dfile_var = data.frame(id=seq_len(ncol(dfile)),var =names(dfile), lbl = lbl,fmt=fmt)
  rownames(dfile_var) <- NULL
  View(dfile_var)
}

# 多选题 https://stackoverflow.com/questions/9265003/analysis-of-multiple-response
# 目前没法加权,交互分析也不行，如果能够复制stata的mrtab比较好
#' @title Frequence tabll of Multiple response variable .
#' @name mrtab
#' @description 进行多选题分析，类似stata中的mrtab
#' @param data A dataframe.
#' @param question.prefix  The common strings of that series of multiple response vaiables
#' @export
#'
mrtab= function(data, question.prefix) {
  z = length(question.prefix)
  temp = vector("list", z)

  for (i in 1:z) {
    a = grep(question.prefix[i], names(data))
    b = sum(data[, a] != 0)
    d = colSums(data[, a] != 0)
    e = sum(rowSums(data[,a]) !=0)
    f = as.numeric(c(d, b))
    temp[[i]] = data.frame(question = c(sub(question.prefix[i],
                                            "", names(d)), "Total"),
                           freq = f,
                           percent = (f/b)*100,
                           percentofcases = (f/e)*100 )
    names(temp)[i] = question.prefix[i]
  }
  temp
}

#注意r当中的round函数与stata中的round对于0.5的处理是有区别的
#R中将它归为最近的偶数（even）一侧。
#https://statisticsglobe.com/r-round-ceiling-floor-trunc-signif-function-example
#https://stackoverflow.com/questions/12688717/round-up-from-5
#https://datacornering.com/round-roundup-rounddown-trunc-in-r/
#四舍五入，向上取整，与stata保持一致

#' @title Round 0.5 up.
#' @name  round2
#' @description 常见的四舍五入而不是根据舍位的奇数偶数决定
#' @param x A vector
#' @param n Number of decimals you want to keep.
#' @export
#'
round2 = function(x, n) {  # Function to always round 0.5 up
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# 设置expss列联表的输出窗口，更短的函数
#' @title table output in viewer.
#' @name  frev
#' @description  设定fre函数结果在viewer输出
#'
library(expss)
#' @export
frev <-function ()
{
  options(expss.output = "viewer")
}
#' @title table output in txt.
#' @name  fret
#' @description  设定fre函数结果在viewer输出
#' @export

fret<- function ()
{
  options(expss.output = "default")
}

