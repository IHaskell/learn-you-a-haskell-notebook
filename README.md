# Learn You a Haskell for Great Good! *Jupyter notebook Edition*

This is a Jupyter notebook adaptation of [__*Learn you a Haskell for Great Good!*__](http://learnyouahaskell.com/) by Miran Lipovača.

I learned Haskell from this book in 2014 by following along in [GHCI](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html), as the book suggested.

In 2019, the Jupyter notebook format would be a nice way read this book. This is one of the best cases for Theodore Gray's idea of the [computational essay](https://www.theatlantic.com/science/archive/2018/04/the-scientific-paper-is-obsolete/556676/), and Andrew Gibiansky has made it possible with [IHaskell](https://github.com/gibiansky/IHaskell).

Each notebook cell depends on cells that come before it, so run the notebooks from top to bottom.

I have refactored code to eliminate errors and removed instructions for how to use GHCI. Other than that I have tried to be faithful to the original text.
 
Miran Lipovača wrote this book and gave it to the world under a [Creative Commons Attribution-Noncommercial-Share Alike 3.0 Unported License](http://creativecommons.org/licenses/by-nc-sa/3.0/), which means that the book is free-as-in-speech, and allows us to enjoy the book in notebook format. But that does not mean that the book is free-as-in-beer; it means that you, dear reader, pay for the book under the honor system. If you are honorable and you want to keep living in a world in which honorable artists license their art as Creative Commons, then [buy the book](http://learnyouahaskell.com/).

Thanks also to Paul Vorbach for <https://github.com/pvorb/learn-you-a-haskell>.

# How to Read

```bash
git clone https://github.com/jamesdbrock/learn-you-a-haskell-notebook.git
cd learn-you-a-haskell-notebook
docker run --rm \
      -p 8888:8888 \
      -v $PWD/notebook:/home/jovyan/work \
      --env JUPYTER_ENABLE_LAB=yes \
      --env JUPYTER_TOKEN=x \
      --name ihaskell_notebook \
      crosscompass/ihaskell-notebook:latest
```

[http://localhost:8888?token=x](http://localhost:8888?token=x)
