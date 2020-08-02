# 하스켈 프로그래밍 배우기

하스켈 프로그래밍의 기초를 소개한다.
참고 자료는 다음과 같다.

* 기본: [__Learn You a Haskell for Great Good!__](http://learnyouahaskell.com/)
    * 저자: Miran Lipovača
    * 주피터 노트북: [jamesdbrock/learn-you-a-haskell-notebook](https://github.com/jamesdbrock/learn-you-a-haskell-notebook)
* 참고자료
    * [A Gentle Introduction To Haskell](https://www.haskell.org/tutorial/)
        * 저자: Paul Hudak, John Peterson, and Joseph Fasel
    * [__Programming in Haskell__](https://www.cambridge.org/core/books/programming-in-haskell/8FED82E807EF12D390DE0D16FDE217E4)
        * 저자: Graham Hutton
* 프로그래밍 환경
    * [GHCi](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html) + [IHaskell](https://github.com/gibiansky/IHaskell)
    * __mybinder.org__ 의 주피터 노트북 서버: [![하스켈 프로그래밍 배우기](https://img.shields.io/badge/launch-Learn%20You%20A%20Haskell%20For%20Great%20Good!-579ACA.svg?logo=data:image/png;base64,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)](https://mybinder.org/v2/gh/jamesdbrock/learn-you-a-haskell-notebook/master?urlpath=lab/tree/learn_you_a_haskell/00-preface.ipynb)
    * 도커 이미지: Clone this repository, then run this `docker` command in a shell:
      ```bash
      git clone https://github.com/jamesdbrock/learn-you-a-haskell-notebook.git
      cd learn-you-a-haskell-notebook
      docker run --rm \
            -p 8888:8888 \
            -v $PWD/notebook:/home/jovyan/work \
            --env JUPYTER_ENABLE_LAB=yes \
            --env JUPYTER_TOKEN=x \
            --name learn-you-a-haskell \
            crosscompass/ihaskell-notebook:latest
      ```

      Then open this link in your browser: [http://localhost:8888?token=x](http://localhost:8888?token=x)

---

![Screenshot](README.screenshot.png)

# 강좌 내용

* 강좌 소개
* ...

Each chapter of the book is one `.ipynb` Jupyter notebook file. See the list of chapter files on the left sidebar in JupyterLab.

Each notebook cell depends on cells that come before it, so run the notebooks from top to bottom. I have refactored code to make the examples work in Jupyter, and removed instructions for how to use GHCI. Other than that I have tried to be faithful to the original text.
 
Miran Lipovača wrote this book and released it to the world under a [Creative Commons Attribution-Noncommercial-Share Alike 3.0 Unported License](http://creativecommons.org/licenses/by-nc-sa/3.0/), which means that the book is free-as-in-speech, and allows us to enjoy the book in notebook format. But that does not mean that the book is free-as-in-beer; it means that you, dear reader, pay for the book under the honor system. If you are honorable and you want to keep living in a world in which honorable artists license their art as Creative Commons, then [buy the book](http://learnyouahaskell.com/).

Thanks also to Paul Vorbach for <https://github.com/pvorb/learn-you-a-haskell>.

# 주피터 노트북 사용법 

아래 사이트를 참조한다. 

* [Jupyter Notebook 사용법](https://greeksharifa.github.io/references/2019/01/26/Jupyter-usage/#jupyter%EC%9D%98-%EA%B8%B0%EB%B3%B8-%EC%82%AC%EC%9A%A9%EB%B2%95) 
