# AI(머신러닝,딥러닝)프로젝트 기반 빅데이터분석과정 A, 2팀

## 프로젝트 소개
격하게 변하는 서울시 아파트 가격에 영향을 미치는 데이터 수집 및 요인 파악
## 개발기간
* 23.04.12 ~ 23.05.02

## 목차(Contents)

01. 팀 구성 및 역할

02. 배경 및 목적

03. 가설 설정

04. 데이터 수집 & 전처리

05. 데이터 시각화

06. 데이터 분석

07. 결론

08. 자체 평가 의견

### 멤버구성
 - 팀장 : 나석원 - 데이터 수집 및 전처리, 데이터 분석, 데이터 시각화, PPT 작성
 - 팀원 : 김현경 - 데이터 수집 및 전처리, DB 설계, PPT 작성
 - 팀원 : 김현욱 - 데이터 수집 및 전처리, DB 설계, PPT 발표
 - 팀원 : 송호준 - 데이터 수집 및 전처리, 시각화, PPT 작성
 - 팀원 : 이준규 - 데이터 수집 및 전처리, 시각화, PPT 발표

### 개발 환경

![종류](https://user-images.githubusercontent.com/127808906/235382737-7daa8bf7-ba0c-412b-932c-e3852eb71dbc.png)

- 'Python 3.9.13'
- 'R 4.2.3'
- 'Git 2.40.0.windows.1'
- 'R studio 2023.03.0+386'
- 'ERwin Data Modeler r7'
- 'MariaDB 10.11.1'
- 'Numpy 1.21.5'
- 'Pandas 1.4.4'
- 'matplotlib 3.5.2'
- 'seaborn 0.11.2'
- 'Scipy 1.9.1'
- 'Selenium 4.8.3'
- 'Scikit-learn 1.2.2'
- 'folium 0.14.0'
- 'plotly 5.9.0'

### 1. 배경 및 목적
- 부동산 시장은 경제에 큰 영향을 미치는 중요한 시장입니다.

- 최근 몇 년간 부동산 가격은 상승세를 보이며, 부동산에 대한 관심도 높아졌습니다.

- 아파트 가격 변동요인을 분석하는 것은 부동산 시장을 이해하고 관심을 가진 사람들에게 중요한 정보입니다.

### 2. 가설 설정

- **환경적 요인** (ex) 인프라, 범죄건수, ... 등등) 과 **경제적 요인** (ex) 기준금리, GDP, 물가지수, 유동성비율, ...등등)이

     **아파트 가격**에 영향을 미친다

### 3-1 데이터 수집 & 전처리

- 데이터 수집을 하고 **파이썬**을 사용하여 데이터를 알아보기 좋게, 분석하기 좋게 가공한다.

![데이터전처리1](https://user-images.githubusercontent.com/127808906/235382251-1de5ded9-b543-4fef-a986-cc31fb3cf39c.png)

![데이터전처리2](https://user-images.githubusercontent.com/127808906/235382263-915a3ca8-bba1-46a0-b944-936fd212c0d9.png)

![버스](https://user-images.githubusercontent.com/129472378/235400841-ae961d76-c6d8-47fc-bb23-d38be7d810c5.PNG)


### 3-2 데이터베이스 생성
#### 환경적 요인과 경제적요인을 정제해서 데이터베이스에 저장한다.
![제목 없음](https://user-images.githubusercontent.com/127808906/235381834-59ce0bfa-e78b-4c6c-bd78-e0b45b3a267e.png)


### 4. 데이터 시각화
- *경제적 요인*을 **시계열 그래프**로 알아보고 *환경적 요인*은 **산점도**로 알아봄으로써 서로 어떤 영향을 주는지 시각적으로 알아본다.
![그림1](https://user-images.githubusercontent.com/127808906/235381781-f1819a41-70c3-4081-8474-11c3ce04e250.png)

![시각화](https://user-images.githubusercontent.com/127808906/235383471-19c0b45f-da6b-498c-817a-d7974835ab27.png)

### 5. 데이터 분석
#### 5-1. 상관관계
- 데이터 분석 전에 상관관계를 함으로써 가볍게 변수들간의 관계를 파악한다.
![상관관계](https://user-images.githubusercontent.com/127808906/235382640-4c0b5953-45e3-491e-9763-b3adf14065f5.png)

#### 5-2. 경제적요인, 환경적요인 회귀분석

- 회귀분석 전에 4가지 조건 선형성, 독립성, 정규성, 등분산성을 확인하고 정확한 회귀분석을 실시했다. 설명력을 통해 우리의 가설의 정확성을 판단하였다.

![제목 없음2](https://user-images.githubusercontent.com/127808906/235382027-3c72f087-52f8-474f-ba5b-66601a9f7d86.png)

![제목 없음1](https://user-images.githubusercontent.com/127808906/235382034-f29c69e2-3cec-4096-b1b1-95e5919acffd.png)

![제목 없음3](https://user-images.githubusercontent.com/127808906/235382134-67c98bd4-7225-43f4-a109-bb92b3e6b1fe.png)






