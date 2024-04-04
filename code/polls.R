# 메타 분석을 위한 패키지 로드
library(meta)

# meta 패키지 ----------------------------------------------------------------


# 여론조사 데이터 입력 (예시)
study <- c("Survey 1", "Survey 2", "Survey 3", "Survey 4", "Survey 5")
n1 <- c(500, 600, 550, 700, 800)  # 후보 1의 표본 크기
n2 <- c(500, 600, 550, 700, 800)  # 후보 2의 표본 크기
p1 <- c(0.45, 0.50, 0.48, 0.52, 0.55)  # 후보 1의 지지율
p2 <- c(0.40, 0.42, 0.44, 0.46, 0.48)  # 후보 2의 지지율

# 각 여론조사의 효과 크기 (차이) 및 분산 계산
diff <- p1 - p2
var_diff <- p1*(1-p1)/n1 + p2*(1-p2)/n2

# 메타분석 수행
meta_result <- metagen(TE = diff, seTE = sqrt(var_diff), studlab = study, sm = "MD", fixed = FALSE, random = TRUE)

# 메타분석 결과 출력
print(summary(meta_result))

# 메타분석 결과 시각화 (포리스트 플롯)
forest(meta_result, xlim = c(-0.2, 0.2), atransf = FALSE, slab = study, comb.fixed = TRUE, comb.random = TRUE)



# metafor 패키지 -------------------------------------------------------------

# 필요한 패키지 설치 및 불러오기
if (!require(metafor)) install.packages("metafor")
library(metafor)

# 예시 데이터 생성
# 여기서는 후보 A와 B의 지지율 차이와 표준편차를 임의로 설정합니다.
# 실제 데이터를 사용할 때는 이 부분을 교체해야 합니다.
data <- data.frame(
 study = 1:10,
 effect_size = rnorm(10, mean = 0.05, sd = 0.10), # 지지율 차이
 se = rnorm(10, mean = 0.05, sd = 0.02) # 표준오차
)

# 무작위 효과 모델을 사용하여 메타 분석 수행
res <- rma(yi = effect_size, sei = se, data = data, method = "REML")

# 결과 요약
summary(res)

# 숲 그래프(포레스트 플롯) 생성
forest(res)


