## 소개

"하스켈 프로그래밍 배우기"는 '**프로그래밍, 수리논리, 수학은 하나다**' 라는 주제하에 진행되는 
강좌 시리즈의 첫번째 강좌이며, 하스켈 프로그래밍을 먼저 소개하는 이유는 다음과 같습니다.

* 프로그래밍 기초 학습
* 유형이론(type theory) 기초 활용
* 수학과 프로그래밍의 연관성 소개

<p>다루는 내용은 
    <a href="https://creativecommons.org/licenses/by-nc-sa/3.0/deed.ko">Creative Commons Attribution-Noncommercial-Share Alike 3.0 Unported License</a>
    조건으로 무료 공개된 Miran Lipova&ccaron;a의
    <a href="http://learnyouahaskell.com/">Learn You a Haskell for Great Good!</a>을
    기본으로 하면서 다음 자료를 참조한 내용을 담고 있습니다.</p>

* [A Gentle Introduction To Haskell](https://www.haskell.org/tutorial/), 
    Paul Hudak, John Peterson, and Joseph Fasel
* [Programming in Haskell](https://www.cambridge.org/core/books/programming-in-haskell/8FED82E807EF12D390DE0D16FDE217E4), 
    Graham Hutton
* [The Haskell Road to Logic, Maths and Programming](https://staff.fnwi.uva.nl/d.j.n.vaneijck2/HR/), 
    Kees Doets and Jan van Eijck

함께 제공되는 주피터 노트북은 James Brock의 
[learn-you-a-haskell-notebook](https://github.com/jamesdbrock/learn-you-a-haskell-notebook)을 
포크(fork)한 것을 기본틀로 사용합니다.

## 목차

1. 소개
1. ...

## 학습 및 프로그래밍 환경

### 추천 환경

* 주의사항: 아래 추천 환경은 비초보자용입니다. 
    초보자가 아니더라도 도커, Visual Studio Code 등을 처음 접하는 경우 많이 생소하고, 또 설치 및 설정할 때 어려울 수 있습니다.
    하지만 프로그래밍에 관심이 많다면 상식적으로 알고 있어야 하는 내용입니다. 
    조금 어렵더라도 검색을 통해 확인하면서 시도해보세요.  

[깃허브 원격저장소(repository)](https://github.com/prologma/learn-you-a-haskell)를 PC에 복제하여 도커(Docker) 이미지와 함께 활용할 것을 추천합니다.

* 깃허브 원격저장소 복제하기

    ```bash
    git clone https://github.com/prologma/learn-you-a-hakell.git
    cd learn-you-a-haskell
    ```

* 도커 이미지 설치

    ```bash
    docker run --rm -p 8888:8888 -v $PWD/notebooks:/home/jovyan/work --name learn-you-a-haskell crosscompass/ihaskell-notebook:latest jupyter lab --LabApp.token=''
    ```

* 위 과정을 마친 후 크롬 브라우저 등을 이용하여 다음 주소 `http://localhost:8888` 방문한 후 `work` 디렉토리를 열면 주피터 노트북을 사용할 수 있음.
    또한 Visual Studio Code를 도커 이미지에 연결하여 주피터 노트북 내용을 따라 학습할 수 있음.
    구체적인 활용법은 [참고자료](./개발환경설정.md)에서 설명됨.
    
* 아래에서 소개하는 binder 서버에 변경사항을 적용하고자 할 경우 깃허브에 수정된 사항 푸시(push)할 것.

도커 이미지 활용이 어렵거나 제공되는 주피터 노트북을 단순히 실행하고자 한다면
<a href="https://mybinder.org/">mybinder.org</a>에서 제공하는 아래 Binder 서버를 이용할 수 있습니다.

* [![launch Learn You a Haskell](https://img.shields.io/badge/launch-하스켈%20프로그래밍%20배우기-579ACA.svg?logo=data:image/png;base64,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)](https://mybinder.org/v2/gh/prologma/learn-you-a-haskell/master?urlpath=lab/tree/learn_you_a_haskell/00-preface.ipynb) 를 
    클릭하여 이동하면 강의에 사용되는 주피터 노트북 바로 실행 가능

* 주의사항: Binder 서버에서 제공하는 주피터 노트북을 이용하여 하스켈 코드를 실행할 수는 있지만 
    실행 결과나 변경 사항이 저장되지는 않음.
    주피터 노트북을 수정한 후 저장하려면 앞서 추천한 도커 이미지 활용법을 사용해야 함.

### 초보자용 환경

프로그래밍 초보자는 [Repl.it](https://repl.it/) 사이트와 
위 바인더 서버가 제공하는 주피터 노트북을 함께 사용하는 방식을 추천합니다.

* [Repl.it에서 하스켈 프로그래밍하기](https://youtu.be/OZ6q6Rz9f6k?t=261) 동영상 참조

* 주의사항: 주피터 노트북은 지원하지 않음. 대신에 위 Binder 서버의 주피터 노트북에서 설명하는 코드를 따라해볼 수 있음. 
    Visual Studio Code와 도커 이미지를 활용하는 방식에 비해 편집기능이 조금 불편하지만 무리없이 잘 활용할 수 있음.

### 도커, 주피터 노트북, Visual Studio Code 소개, 설치 및 사용 관련 참고자료

* 윈도우 10에서 도커 이미지를 활용한 개발환경 설정법: [윈도우 10에서 도커 + Visual Studio Code 사용법](./개발환경설정.md)
* 도커 소개: [도커가 왜 좋은지 5분안에 설명해줌](https://www.youtube.com/watch?v=ufLmReluPww)
* 주피터 노트북의 간단한 사용법: [Jupyter Notebook 사용법](https://www.youtube.com/watch?v=4_-IIfbdR5M)

## 감사의 말(Acknowledgement)
<p>제공되는 mybinder의 서버와 도커 이미지는 James Brock의 
<a href="https://github.com/jamesdbrock/learn-you-a-haskell-notebook">learn-you-a-haskell-notebook</a>에서 
제공한 것을 활용합니다.
주피터 노트북 서버에 사용되는 하스켈 해석기는 
<a href="https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html">GHCi</a>를 
Jupyter Project에 적용한 Andrew Gibiansky의 
<a href="https://github.com/gibiansky/IHaskell">IHaskell</a>입니다.
<a href="http://learnyouahaskell.com/">Learn You a Haskell for Great Good!</a> 책을 무료로 공개한
Miran Lipova&ccaron;a를 포함하여 세 사람에게 무한한 감사를 드립니다.</p>
