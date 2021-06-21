# Purescript Update Monad

[![CI](https://github.com/jmatsushita/purecript-update-monad/actions/workflows/ci.yml/badge.svg)](https://github.com/jmatsushita/purecript-update-monad/actions/workflows/ci.yml)

Purescript implementation of the update monad (in src/Control/Monad) 
  - Control.Monad.Update / Control.Monad.UpdateT : from these haskell blogposts https://chrispenner.ca/posts/update-monad https://hashanp.xyz/posts/update.html 
  - Control.Monad.Update.State / StateT.purs : following Chris suggestion to implement the update monad with an underlying state monad for performance.
  - StateFreeCofree.purs : A state monad implemented as a Free monad, with a Cofree interpreter (using the pairing instance / zap in haskell )
  - [unfinished] I wanted to try to implement the update monad as a free monad.