library(httr)
library(rvest)
library(readr)
library(stringr)
library(jsonlite)
library(xts)
library(lubridate)
library(dplyr)
library(magrittr)

# 최근 영업일 구하기
url = 'https://finance.naver.com/sise/sise_deposit.nhn'

biz_day = GET(url) %>%
  read_html(encoding = 'EUC-KR') %>% #GET으로 불러온후 인코딩
  html_nodes(xpath =
               '//*[@id="type_0"]/div/ul[2]/li/span') %>%
  html_text() %>%    #텍스트 데이터만 추출
  str_match(('[0-9]+.[0-9]+.[0-9]+') ) %>%
  str_replace_all('\\.', '')

# 산업별 현황 OTP 발급
gen_otp_url =
  'http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx'
gen_otp_data = list(
  name = 'fileDown',
  filetype = 'csv',
  url = 'MKD/03/0303/03030103/mkd03030103',
  tp_cd = 'ALL',
  date = biz_day, # 최근영업일로 변경
  lang = 'ko',
  pagePath = '/contents/MKD/03/0303/03030103/MKD03030103.jsp')
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

# 산업별 현황 데이터 다운로드
down_url = 'http://file.krx.co.kr/download.jspx'
down_sector = POST(down_url, query = list(code = otp),
                   add_headers(referer = gen_otp_url)) %>%
  read_html() %>%
  html_text() %>%
  read_csv()

ifelse(dir.exists('data'), FALSE, dir.create('data'))
write.csv(down_sector, 'data/krx_sector.csv')

# 개별종목 지표 OTP 발급
gen_otp_url =
  'http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx'
gen_otp_data = list(
  name = 'fileDown',
  filetype = 'csv',
  url = "MKD/13/1302/13020401/mkd13020401",
  market_gubun = 'ALL',
  gubun = '1',
  schdate = biz_day, # 최근영업일로 변경
  pagePath = "/contents/MKD/13/1302/13020401/MKD13020401.jsp")

otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

# 개별종목 지표 데이터 다운로드
down_url = 'http://file.krx.co.kr/download.jspx'
down_ind = POST(down_url, query = list(code = otp),
                add_headers(referer = gen_otp_url)) %>%
  read_html() %>%
  html_text() %>%
  read_csv()

write.csv(down_ind, 'data/krx_ind.csv')

down_sector = read.csv('data/krx_sector.csv', row.names = 1,
                       stringsAsFactors = FALSE)
down_ind = read.csv('data/krx_ind.csv',  row.names = 1,
                    stringsAsFactors = FALSE)

intersect(names(down_sector), names(down_ind)) # 중복된 Column 확인
setdiff(down_sector[, '종목명'], down_ind[ ,'종목명']) #한쪽에만 있는 항목

# 한쪽에만 있는 항목 (일반적이지 않은 종목들) 제거
# merge는 by를 기준으로 두 데이터를 합침
# all 이 TRUE : 합집합 반환, FALSE : 교집합 반환
KOR_ticker = merge(down_sector, down_ind,
                   by = intersect(names(down_sector),
                                  names(down_ind)),
                   all = FALSE)
# 시가총액.원. 으로 정렬, -는 내림차순 설정
KOR_ticker = KOR_ticker[order(-KOR_ticker['시가총액.원.']), ]
print(head(KOR_ticker))

# 스팩, 우선주 종목 제외

KOR_ticker = KOR_ticker[!grepl('스팩', KOR_ticker[, '종목명']), ]  
KOR_ticker = KOR_ticker[str_sub(KOR_ticker[, '종목코드'], -1, -1) == 0, ]

rownames(KOR_ticker) = NULL
write.csv(KOR_ticker, 'data/KOR_ticker.csv')



sector_code = c('G25', 'G35', 'G50', 'G40', 'G10',
                'G20', 'G55', 'G30', 'G15', 'G45')
data_sector = list()

for (i in sector_code) {
  
  url = paste0(
    'http://www.wiseindex.com/Index/GetIndexComponets',
    '?ceil_yn=0&dt=',biz_day,'&sec_cd=',i)
  data = fromJSON(url)
  data = data$list
  
  data_sector[[i]] = data
  
  Sys.sleep(1)
}


data_sector = do.call(rbind, data_sector)

write.csv(data_sector, 'data/KOR_sector.csv')

#####Ch.6 금융 데이터 수집



KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
print(KOR_ticker$'종목코드'[1])
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

ifelse(dir.exists('data/KOR_price'), FALSE,
       dir.create('data/KOR_price'))

for(i in 1 : nrow(KOR_ticker) ) {
  
  price = xts(NA, order.by = Sys.Date()) # 빈 시계열 데이터 생성
  name = KOR_ticker$'종목코드'[i] # 티커 부분 선택
  
  # 오류 발생 시 이를 무시하고 다음 루프로 진행
  tryCatch({
    # url 생성
    url = paste0(
      'https://fchart.stock.naver.com/sise.nhn?symbol='
      ,name,'&timeframe=day&count=500&requestType=0')
    
    # 이 후 과정은 위와 동일함
    # 데이터 다운로드
    data = GET(url)
    data_html = read_html(data, encoding = 'EUC-KR') %>%
      html_nodes("item") %>%
      html_attr("data") 
    
    # 데이터 나누기
    price = read_delim(data_html, delim = '|')
    
    # 필요한 열만 선택 후 클렌징
    price = price[c(1, 5)] 
    price = data.frame(price)
    colnames(price) = c('Date', 'Price')
    price[, 1] = ymd(price[, 1])
    
    rownames(price) = price[, 1]
    price[, 1] = NULL
    
  }, error = function(e) {
    
    # 오류 발생시 해당 종목명을 출력하고 다음 루프로 이동
    warning(paste0("Error in Ticker: ", name))
  })
  
  # 다운로드 받은 파일을 생성한 폴더 내 csv 파일로 저장
  write.csv(price, paste0('data/KOR_price/', name,
                          '_price.csv'))
  
  # 타임슬립 적용
  Sys.sleep(2)
}


KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6,side = c('left'), pad = '0')

ifelse(dir.exists('data/KOR_fs'), FALSE,
       dir.create('data/KOR_fs'))
ifelse(dir.exists('data/KOR_value'), FALSE,
       dir.create('data/KOR_value'))

for(i in 1 : nrow(KOR_ticker) ) {
  
  data_fs = c()
  data_value = c()
  name = KOR_ticker$'종목코드'[i]
  
  # 오류 발생 시 이를 무시하고 다음 루프로 진행
  tryCatch({
    
    Sys.setlocale('LC_ALL', 'English')
    
    # url 생성
    url = paste0(
      'http://comp.fnguide.com/SVO2/ASP/'
      ,'SVD_Finance.asp?pGB=1&gicode=A',
      name)
    
    # 이 후 과정은 위와 동일함
    
    # 데이터 다운로드 후 테이블 추출
    data = GET(url,
               user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                          AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36')) %>%
      read_html() %>%
      html_table()
    
    Sys.setlocale('LC_ALL', 'Korean')
    
    # 3개 재무제표를 하나로 합치기
    data_IS = data[[1]]
    data_BS = data[[3]]
    data_CF = data[[5]]
    
    data_IS = data_IS[, 1:(ncol(data_IS)-2)]
    data_fs = rbind(data_IS, data_BS, data_CF)
    
    # 데이터 클랜징
    data_fs[, 1] = gsub('계산에 참여한 계정 펼치기',
                        '', data_fs[, 1])
    data_fs = data_fs[!duplicated(data_fs[, 1]), ]
    
    rownames(data_fs) = NULL
    rownames(data_fs) = data_fs[, 1]
    data_fs[, 1] = NULL
    
    # 12월 재무제표만 선택
    data_fs =
      data_fs[, substr(colnames(data_fs), 6,7) == "12"]
    
    data_fs = sapply(data_fs, function(x) {
      str_replace_all(x, ',', '') %>%
        as.numeric()
    }) %>%
      data.frame(., row.names = rownames(data_fs))
    
    
    # 가치지표 분모부분
    value_type = c('지배주주순이익', 
                   '자본', 
                   '영업활동으로인한현금흐름', 
                   '매출액') 
    
    # 해당 재무데이터만 선택
    value_index = data_fs[match(value_type, rownames(data_fs)),
                          ncol(data_fs)]
    
    # Snapshot 페이지 불러오기
    url =
      paste0(
        'http://comp.fnguide.com/SVO2/ASP/SVD_Main.asp',
        '?pGB=1&gicode=A',name)
    data = GET(url,
               user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                      AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'))
    
    # 현재 주가 크롤링
    price = read_html(data) %>%
      html_node(xpath = '//*[@id="svdMainChartTxt11"]') %>%
      html_text() %>%
      parse_number()
    
    # 보통주 발행주식수 크롤링
    share = read_html(data) %>%
      html_node(
        xpath =
          '//*[@id="svdMainGrid1"]/table/tbody/tr[7]/td[1]') %>%
      html_text() %>%
      strsplit('/') %>%
      unlist() %>%
      .[1] %>%
      parse_number()
    
    # 가치지표 계산
    data_value = price / (value_index * 100000000/ share)
    names(data_value) = c('PER', 'PBR', 'PCR', 'PSR')
    data_value[data_value < 0] = NA
    
  }, error = function(e) {
    
    # 오류 발생시 해당 종목명을 출력하고 다음 루프로 이동
    data_fs <<- NA
    data_value <<- NA
    warning(paste0("Error in Ticker: ", name))
  })
  
  # 다운로드 받은 파일을 생성한 각각의 폴더 내 csv 파일로 저장
  
  # 재무제표 저장
  write.csv(data_fs, paste0('data/KOR_fs/', name, '_fs.csv'))
  
  # 가치지표 저장
  write.csv(data_value, paste0('data/KOR_value/', name,
                               '_value.csv'))
  
  # 1초간 타임슬립 적용
  Sys.sleep(1)
}

dart_api = Sys.getenv("dart_api_key")

#6.3.2 고유번호 다운로드


codezip_url = paste0(
  'https://opendart.fss.or.kr/api/corpCode.xml?crtfc_key=', dart_api)

codezip_data = GET(codezip_url)
print(codezip_data)
# 다운받은 내용을 확인해보면, 바이너리 형태의 데이터가 첨부되어 있음.

codezip_data$headers[["content-disposition"]]
#headers의 “content-disposition” 부분을 확인해보면 CORPCODE.zip 파일이 첨부되어있음
# 해당 파일의 압축을 풀어 첨부된 내용 확인
tf = tempfile(fileext = '.zip') # 빈 .zip 파일 생성

writeBin(                       #바이너리 형태의 파일을 저장
  content(codezip_data, as = "raw"), #content를 통해 첨부파일 내용을 raw형태로 저장
  file.path(tf)   # 파일명 tf
)

nm = unzip(tf, list = TRUE)
print(nm)

#          Name   Length Date
#1 CORPCODE.xml 15744735 <NA>
code_data = read_xml(unzip(tf, nm$Name))
print(code_data)

#HTML 태그를 이용해 고유번호, 종목명, 거래소 상장 티커 를 추출한 후 하나의 데이터로 합침
corp_code = code_data %>% html_nodes('corp_code') %>% html_text()
corp_name = code_data %>% html_nodes('corp_name') %>% html_text()
corp_stock = code_data %>% html_nodes('stock_code') %>% html_text()

corp_list = data.frame(
  'code' = corp_code,
  'name' = corp_name,
  'stock' = corp_stock,
  stringsAsFactors = FALSE
)

nrow(corp_list)
head(corp_list) #stock 열이 빈 종목은 거래소에 상장되지 않은 종목
# 비상장종목들 날리고 csv로 저장
corp_list = corp_list[corp_list$stock !=" ",]
write.csv(corp_list, 'data/corp_list.csv')




KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
corp_list =  read.csv('data/corp_list.csv', row.names = 1)

KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

corp_list$'code' =
  str_pad(corp_list$'code', 8, side = c('left'), pad = '0')

corp_list$'stock' =
  str_pad(corp_list$'stock', 6, side = c('left'), pad = '0')

ticker_list = KOR_ticker %>% left_join(corp_list, by = c('종목코드' = 'stock')) %>%
  select('종목코드', '종목명', 'code')

ifelse(dir.exists('data/dart_fs'), FALSE, dir.create('data/dart_fs'))

bsns_year = 2019
reprt_code = '11011'

for(i in 1 : nrow(ticker_list) ) {
  
  data_fs = c()
  name = ticker_list$code[i]
  
  # 오류 발생 시 이를 무시하고 다음 루프로 진행
  
  tryCatch({
    
    # url 생성
    url = paste0('https://opendart.fss.or.kr/api/fnlttSinglAcntAll.json?crtfc_key=',
                 dart_api, 
                 '&corp_code=', name,
                 '&bsns_year=', bsns_year,
                 '&reprt_code=', reprt_code,'&fs_div=CFS'
    )
    
    # JSON 다운로드
    fs_data_all = fromJSON(url) 
    fs_data_all = fs_data_all[['list']]
    
    # 만일 연결재무제표 없어서 NULL 반환시
    # reprt_code를 OFS 즉 재무제표 다운로드
    if (is.null(fs_data_all)) {
      
      url = paste0('https://opendart.fss.or.kr/api/fnlttSinglAcntAll.json?crtfc_key=',
                   dart_api, 
                   '&corp_code=', name,
                   '&bsns_year=', bsns_year,
                   '&reprt_code=', reprt_code,'&fs_div=OFS'
      )
      
      fs_data_all = fromJSON(url) 
      fs_data_all = fs_data_all[['list']]
      
    }
    
    
    # 데이터 선택 후 열이름을 연도로 변경
    yr_count = str_detect(colnames(fs_data_all), 'trm_amount') %>% sum()
    yr_name = seq(bsns_year, (bsns_year - yr_count + 1))
    
    fs_data_all = fs_data_all[, c('corp_code', 'sj_nm', 'account_nm', 'account_detail')] %>%
      cbind(fs_data_all[, str_which(colnames(fs_data_all), 'trm_amount')])
    
    colnames(fs_data_all)[str_which(colnames(fs_data_all), 'amount')] = yr_name
    
  }, error = function(e) {
    
    # 오류 발생시 해당 종목명을 출력하고 다음 루프로 이동
    data_fs <<- NA
    warning(paste0("Error in Ticker: ", name))
  })
  
  # 다운로드 받은 파일을 생성한 각각의 폴더 내 csv 파일로 저장
  
  # 재무제표 저장
  write.csv(fs_data_all, paste0('data/dart_fs/', ticker_list$종목코드[i], '_fs_dart.csv'))
  
  # 1초간 타임슬립 적용
  Sys.sleep(1)
}


### 7. 주가 정리하기

#7.1 주가 정리하기
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

price_list = list()

for (i in 1 : nrow(KOR_ticker)) {
  
  name = KOR_ticker[i, '종목코드']
  price_list[[i]] =
    read.csv(paste0('data/KOR_price/', name,
                    '_price.csv'),row.names = 1) %>%
    as.xts()
  
}

price_list = do.call(cbind, price_list) %>% na.locf()
colnames(price_list) = KOR_ticker$'종목코드'

write.csv(data.frame(price_list), 'data/KOR_price.csv')

#7.2 재무제표 정리하기


KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

data_fs = list()

for (i in 1 : nrow(KOR_ticker)){
  
  name = KOR_ticker[i, '종목코드']
  data_fs[[i]] = read.csv(paste0('data/KOR_fs/', name,
                                 '_fs.csv'), row.names = 1)
}

fs_item = data_fs[[1]] %>% rownames()
length(fs_item)
# 첫번째 항목인 매출액 기준 데이터 정리
select_fs = lapply(data_fs, function(x) {
  # 해당 항목이 있으면 데이터를 선택
  if ( '매출액' %in% rownames(x)) {
    x[which(rownames(x) == '매출액'), ]
    
    #해당 항목이 존재하지 않을 시, NA로 된 데이터 프레임 달성
  } else {
    data.frame(NA)
  }
})

select_fs = bind_rows(select_fs)
#bind_rows는 열 개수가 다를 경우 나머지 부분을 NA로 처리해줌

select_fs = select_fs[!colnames(select_fs) %in%
                        c('.','NA.')]
select_fs = select_fs[,order(names(select_fs))]
rownames(select_fs) = KOR_ticker[, '종목코드']

print(head(select_fs))
## ↑ 이것으로 전 종목의 매출액 데이터가 연도별로 정리됨

# for loop 구문을 이용해 모든 재무항목에 대한 데이터 정리

fs_list = list()

for( i in 1 : length(fs_item)) {
  select_fs = lapply(data_fs, function(x) {
    #해당 항목이 있을 시 데이터를 선택
    if (fs_item[i] %in% rownames(x) ) {
      x[which(rownames(x) == fs_item[i]), ]
      
      # 해당 항목이 존재하지 않을 시, NA로 된 데이터 프레임 생성
    } else {
      data.frame(NA)
    }
  })
  
  # 리스트 데이터를 행으로 묶어줌
  select_fs = bind_rows(select_fs)
  
  # 열 이름이 '.' 혹은 'NA.' 인 지점은 삭제 (NA 데이터)
  select_fs = select_fs[!colnames(select_fs) %in%
                          c('.', 'NA.')]
  
  # 연도 순별로 정리
  select_fs = select_fs[,order(names(select_fs))]
  # 행이름을 티커로 변경
  rownames(select_fs) = KOR_ticker[, '종목코드']
  
  #리스트에 최종 저장
  fs_list[[i]] = select_fs
  
}

#리스트 이름을 재무 항목으로 변경
names(fs_list) = fs_item

# 위 과정을 거치면 fs_list에 총 237리스트가 생성됨
# 각 리스트에는 해당 재무 항목에 대한 전 종목의 연도별 데이터가 정리되어있음

# 리스트형태 그대로 저장하기 위해 Rds 파일로 저장
# (연결프로그램을 R studio로 설정)
saveRDS(fs_list, 'data/KOR_fs.Rds')


#7.3 가치지표 정리하기
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
KOR_ticker$'종목코드' = 
  str_pad(KOR_ticker$'종목코드',6,side=c('left'),pad='0')

data_value = list()

for (i in 1 : nrow(KOR_ticker)){
  
  name = KOR_ticker[i, '종목코드']
  data_value[[i]] =
    read.csv(paste0('data/KOR_value/', name,
                    '_value.csv'), row.names = 1) %>%
    t() %>% data.frame() # 전치 행렬 
}

data_value = bind_rows(data_value)
print(head(data_value))

data_value = data_value[colnames(data_value) %in%
                          c('PER','PBR','PCR','PSR')]

data_value = data_value %>%
  mutate_all(list(~na_if(., Inf)))

rownames(data_value) = KOR_ticker[, '종목코드']
print(head(data_value))
write.csv(data_value, 'data/KOR_value.csv')

