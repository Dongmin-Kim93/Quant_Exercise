#10.2 마법공식
library(stringr)
library(dplyr)
##먼저  퀄리티의 지표인 매출총이익과 
##밸류 지표인 PBR을 통해 둘 사이의 관계를 확인
KOR_value = read.csv('data/KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)
KOR_fs = readRDS('data/KOR_fs.Rds')
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 

data_pbr = KOR_value['PBR']

if (lubridate::month(Sys.Date()) %in% c(1,2,3,4)) {
  num_col = ncol(KOR_fs[[1]]) - 1
} else {
  num_col = ncol(KOR_fs[[1]])
}

data_gpa = 
  (KOR_fs$'매출총이익' / KOR_fs$'자산')[num_col] %>%
  setNames('GPA')

cbind(data_pbr, -data_gpa) %>%
  cor(method = 'spearman', use = 'complete.obs') %>% round(4)

cbind(data_pbr, data_gpa) %>%
  mutate(quantile_pbr = ntile(data_pbr, 5)) %>% #ntile : 분위수로 나누어줌
  filter(!is.na(quantile_pbr)) %>%
  group_by(quantile_pbr) %>%
  summarise(mean_gpa = mean(GPA, na.rm = TRUE)) %>%
  ggplot(aes(x = quantile_pbr, y = mean_gpa)) +
  geom_col() + 
  xlab('PBR') + ylab('GPA')

#10.2.2 마법공식 이해하기
##(1) 이율 : 기업수익을 기업가치로 나누는 값 ~= PER 역수 (Value 지표)
##(2) 투하자본 수익률 : 기업 수익을 투자한 자본으로 나눈 값~=ROE (Quality)
## 랭킹 합 기준 상위 30개 종목을 1년간 보유 후 매도

###이익수익률 = EBIT / EV
library(stringr)
library(dplyr)
KOR_value = read.csv('data/KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)
KOR_fs = readRDS('data/KOR_fs.Rds')
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

if ( lubridate::month(Sys.Date()) %in% c(1,2,3,4) ) {
  num_col = ncol(KOR_fs[[1]]) - 1
} else {
  num_col = ncol(KOR_fs[[1]]) 
}

# 분자
magic_ebit = (KOR_fs$'지배주주순이익' + KOR_fs$'법인세비용' +
                KOR_fs$'이자비용')[num_col]

# 분모
magic_cap = KOR_value$PER * KOR_fs$'지배주주순이익'[num_col]
magic_debt = KOR_fs$'부채'[num_col]
magic_excess_cash_1 = KOR_fs$'유동부채' - KOR_fs$'유동자산' +
  KOR_fs$'현금및현금성자산'
magic_excess_cash_1[magic_excess_cash_1 < 0] = 0
magic_excess_cash_2 =
  (KOR_fs$'현금및현금성자산' - magic_excess_cash_1)[num_col]

magic_ev = magic_cap + magic_debt - magic_excess_cash_2

# 이익수익률
magic_ey = magic_ebit / magic_ev

#퀄리티 지표인 투하자본수익률 계산
magic_ic = ((KOR_fs$'유동자산' - KOR_fs$'유동부채') +
              (KOR_fs$'비유동자산' - KOR_fs$'감가상각비'))[num_col]
magic_roc = magic_ebit / magic_ic

#마법공식 포트폴리오 구성
invest_magic = rank(rank(-magic_ey) + rank(-magic_roc)) <= 30

KOR_ticker[invest_magic, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`이익수익률` = round(magic_ey[invest_magic, ], 4),
         `투하자본수익률` = round(magic_roc[invest_magic, ], 4))

#10.3 이상치 데이터 제거 및 팩터의 결합
library(magrittr)
library(ggplot2)

KOR_value = read.csv('data/KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)

max(KOR_value$PBR, na.rm = TRUE)

KOR_value %>%
  ggplot(aes(x = PBR)) +
  geom_histogram(binwidth = 0.1)
# 위 코드 실행지 PBR이 600대인 이상치 존재
# 10.3.1 트림(Trim): 이상치 데이터 삭제
#3 상하위 1% 데이터 삭제

library(dplyr)

value_trim = KOR_value %>%
  select(PBR) %>%
  mutate(PBR = ifelse(percent_rank(PBR) > 0.99, NA, PBR),
         PBR = ifelse(percent_rank(PBR) < 0.01, NA, PBR))

value_trim %>%
  ggplot(aes(x = PBR)) +
  geom_histogram(binwidth = 0.1)

# 10.3.2 윈저라이징(Winsorizing): 이상치 데이터 대체

value_winsor = KOR_value %>%
  select(PBR) %>%
  mutate(PBR = ifelse(percent_rank(PBR) > 0.99,
                      quantile(., 0.99, na.rm = TRUE), PBR),
         PBR = ifelse(percent_rank(PBR) < 0.01,
                      quantile(., 0.01, na.rm = TRUE), PBR))

value_winsor %>%
  ggplot(aes(x = PBR)) +
  geom_histogram(binwidth = 0.1)

#10.3.3 팩터의 결합 방법
library(tidyr)

KOR_value %>%
  mutate_all(list(~min_rank(.))) %>%
  mutate_all(list(~scale(.))) %>% #정규화
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(. ~ key)  

#10.4 멀티팩터 포트폴리오
#퀄리티: 자기자본이익률, 매출총이익, 영업활동현금흐름
#밸류: PER, PBR, PSR, PCR
#모멘텀: 3개월 수익률, 6개월 수익률, 12개월 수익률

library(xts)
library(stringr)

KOR_fs = readRDS('data/KOR_fs.Rds')
KOR_value = read.csv('data/KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)
KOR_price = read.csv('data/KOR_price.csv', row.names = 1,
                     stringsAsFactors = FALSE) %>% as.xts()

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

if( lubridate::month(Sys.Date()) %in% c(1,2,3,4)) {
  num_col = ncol(KOR_fs[[1]])-1
} else {
  num_col = ncol(KOR_fs[[1]])
}
#퀄리티 지표 계산
quality_roe = (KOR_fs$'지배주주순이익' / KOR_fs$'자본')[num_col]
quality_gpa = (KOR_fs$'매출총이익'/KOR_fs$'자산')[num_col]
quality_cfo = 
  (KOR_fs$'영업활동으로인한현금흐름' / KOR_fs$'자산')[num_col]

quality_profit = 
  cbind(quality_roe, quality_gpa, quality_cfo) %>%
  setNames(., c('ROE', 'GPA', 'CFO'))

factor_quality = quality_profit %>%
  mutate_all(list(~min_rank(desc(.)))) %>% #높을 수록 좋으니 desc 붙임
  mutate_all(list(~scale(.))) %>%
  rowSums()

factor_quality %>% 
  data.frame() %>%
  ggplot(aes(x = `.`)) +
  geom_histogram()

# 밸류 지표 계산
factor_value = KOR_value %>%
  mutate_all(list(~min_rank(.))) %>%   #낮을 수록 좋으니 desc 안붙임
  mutate_all(list(~scale(.))) %>%
  rowSums()

factor_value %>%
  data.frame() %>%
  ggplot(aes(x=`.`)) +
  geom_histogram()

# 모멘텀 지표 계산
library(PerformanceAnalytics)
library(dplyr)

ret_3m = Return.calculate(KOR_price) %>% xts::last(60) %>%
  sapply(., function(x) {prod(1+x) - 1})
ret_6m = Return.calculate(KOR_price) %>% xts::last(120) %>%
  sapply(., function(x) {prod(1+x) - 1})
ret_12m = Return.calculate(KOR_price) %>% xts::last(252) %>%
  sapply(., function(x) {prod(1+x) - 1})
ret_bind = cbind(ret_3m,ret_6m,ret_12m) %>% data.frame()

factor_mom = ret_bind %>%
  mutate_all(list(~min_rank(desc(.)))) %>%
  mutate_all(list(~scale(.))) %>%
  rowSums()

factor_mom %>%
  data.frame() %>%
  ggplot(aes(x=`.`)) +
  geom_histogram()

#퀄리티, 밸류, 모멘텀 팩터 간의 랭크의 서로 간 상관관계
library(corrplot)

cbind(factor_quality, factor_value, factor_mom) %>%
  data.frame() %>%
  setNames(c('Quality', 'Value', 'Momentum')) %>%
  cor(use = 'complete.obs') %>%
  round(., 2) %>%
  corrplot(method = 'color', type = 'upper',
           addCoef.col = 'black', number.cex = 1,
           tl.cex = 0.6, tl.srt = 45, tl.col = 'black',
           col =
             colorRampPalette(c('blue', 'white', 'red'))(200),
           mar=c(0,0,0.5,0))

# 계산된 팩터 토대로 최종 포트폴리오 구성
# 각 팩터에 동일 비중인 0.33 곱해서 더함

factor_qvm = 
  cbind(factor_quality, factor_value, factor_mom) %>%
  data.frame() %>%
  mutate_all(list(~scale(.))) %>%
  mutate(factor_quality = factor_quality * 0.33,
         factor_value = factor_value * 0.33,
         factor_mom = factor_mom * 0.33) %>%
  rowSums()

invest_qvm = rank(factor_qvm) <= 30

library(tidyr)

quality_profit[invest_qvm, ] %>%
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(. ~ key, scale = 'free', ncol = 1) +
  xlab(NULL)

KOR_value[invest_qvm, ] %>%
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(. ~ key, scale = 'free', ncol = 1) +
  xlab(NULL)

ret_bind[invest_qvm, ] %>%
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(. ~ key, scale = 'free', ncol = 1) +
  xlab(NULL)

KOR_ticker[invest_qvm, ] %>%
  select('종목코드', '종목명') %>%
  cbind(round(quality_gpa[invest_qvm, ], 2)) %>% ####
  cbind(round(KOR_value$PBR[invest_qvm], 2)) %>% ####
  cbind(round(ret_12m[invest_qvm], 2)) %>%
  cbind(KOR_ticker$종가[invest_qvm]) %>%
  setNames(c('종목코드', '종목명', 'gpa', 'PBR', '12M', '가격')) %>% 
  data.frame()
  

cbind(quality_profit, KOR_value, ret_bind)[invest_qvm, ] %>% 
  apply(., 2, mean) %>% round(3) %>% t()
