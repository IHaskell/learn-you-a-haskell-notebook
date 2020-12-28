# 하스켈 프로그래밍 배우기

"하스켈 프로프로그래밍 배우기"는 '**프로그래밍, 수리논리, 수학은 하나다**' 라는 주제하에 진행되는 
강좌 시리즈의 첫번째 강좌이다. 
하스켈 프로그래밍을 먼저 소개하는 이유는 다음과 같다.

* 프로그래밍 기초 학습
* 유형이론(type theory) 기초 활용
* 수학과 프로그래밍의 연관성 소개

다루는 내용은 
[Creative Commons Attribution-Noncommercial-Share Alike 3.0 Unported License](https://creativecommons.org/licenses/by-nc-sa/3.0/deed.ko) 조건으로
무료로 공개된 Miran Lipovača의
[Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)을
기본으로 하면서 아래 자료를 참조한 내용을 담고 있다.

* [A Gentle Introduction To Haskell](https://www.haskell.org/tutorial/)
    * 저자: Paul Hudak, John Peterson, and Joseph Fasel
* [Programming in Haskell](https://www.cambridge.org/core/books/programming-in-haskell/8FED82E807EF12D390DE0D16FDE217E4)
    * 저자: Graham Hutton
* [The Haskell Road to Logic, Maths and Programming](https://staff.fnwi.uva.nl/d.j.n.vaneijck2/HR/)
    * 저자: Kees Doets and Jan van Eijck

제공되는 주피터 노트북은 James Brock의 [learn-you-a-haskell-notebook](https://github.com/jamesdbrock/learn-you-a-haskell-notebook)을 
포크(fork)한 것을 기본틀로 사용한다. 

## 목차

* 머리말
* ...


## 프로그래밍 환경

* (__추천__)여기 리포지토리를 복제 한 후 도커([Docker](https://www.docker.com/products/docker-desktop)) 활용

    * 도커 이미지 설치는 터미널을 이용하여 아래와 같이 진행하면 됨.

        ```bash
        git clone https://github.com/prologma/learn-you-a-hakell.git
        cd learn-you-a-haskell
        docker run --rm -p 8888:8888 -v $PWD:/home/jovyan/work --name learn-you-a-haskell crosscompass/ihaskell-notebook:latest jupyter lab --LabApp.token=''
        ```

    * 위 과정을 마친 후 크롬 브라우저 등을 이용하여 다음 주소 `http://localhost:8888` 를 방문하면 됨.
    * 주피터 노트북 등을 수정한 후 깃(git)으로 커밋하면 수정된 사항을 보존할 수 있음. 
    * 아래에서 소개하는 binder 를 활용하고자 할 경우 깃허브에 수정된 사항을 푸쉬(push)하면 됨.

* _mybinder.org_ 의 주피터 노트북 서버 활용
    * [![하스켈 프로그래밍 배우기](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/prologma/learn-you-a-haskell/master) 를 클릭하여 이동한 후 `learn_you_a_haskell` 디렉토리로 이동하면 바로 주피터 노트북 사용 가능함. 
    * 주의사항: binder 에서 제공하는 주피터 노트북에서 하스켈 명령을 실행할 수는 있지만 실행 결과나 변경 사항이 저장되지는 않음.
        주피터 노트북을 수정한 후 저장하려면 앞서 추천한 도커 이미지 활용법을 사용해야 함.

제공되는 mybinder의 서버와 도커 이미지는 James Brock이 
[learn-you-a-haskell-notebook](https://github.com/jamesdbrock/learn-you-a-haskell-notebook)에서 
제공한 것을 활용함.
주피터 노트북 서버에 사용되는 하스켈 해석기는 
[GHCi](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html)를 
Jupyter Project에 적용한 Andrew Gibiansky의 
[IHaskell](https://github.com/gibiansky/IHaskell)임. 

## 도커와 주피터 노트북 소개, 설치 및 사용 안내

도커와 도커 이미지 활용에 대한 간단한 사용법은 아래 동영상을 참조:

* [도커 소개](https://www.youtube.com/watch?v=ufLmReluPww)
* [윈도우 10에서 도커 + VS Code 사용법](./개발환경설정.md)

주피터 노트북의 간단한 사용법은 아래 동영상을 참조:

* [Jupyter Notebook 사용법](https://www.youtube.com/watch?v=4_-IIfbdR5M)
