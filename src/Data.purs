module Data where

import Data.String (joinWith)

import Data.Maybe (Maybe(..))
import Data.Array
import Types

initialState :: AppState
initialState = AppState {
    actionsCount: 0
  , currentPath: ["about"]
}


theSite = Node {
    path: ""
  , title: "Eugene Naumenko"
  , dataSource: MemorySource "The root"
  , children: [
        Node { title: "About"
             , path: "about"
             , children: []
             , dataSource: MemorySource (joinWith "\n"[
                    "Hi, I'm Eugene."
                  , ""
                  , "Software {engineer, architect} with 10 years of experience in networking, distributed and high load systems, web development, software design and project management."
                  , ""
                  , "Worked on online education systems, spam protection systems, online GIS systems, accounting and trading software, rich web applications, IT/network infrastructure systems."
                  , ""
                  , "Created own small language for web development and a few open source utilities and applications."
                  , ""
                  , ""
                  , "> ![Prom.ua](http://static-cache.ua.uaprom.net/image/i18n-domain/UA/logo_main-trans.png?r=e8abc69e68fadede022fc4e4a50e327b)"
                  , "> ![Cisco](https://upload.wikimedia.org/wikipedia/commons/thumb/6/64/Cisco_logo.svg/225px-Cisco_logo.svg.png)"
                  , "> ![Pearson](data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAJcAAAAsCAMAAABfTNr9AAAAdVBMVEXtawbtagPuchjsYADtZwD8+/L79er7+O7sYwDsXQD8/vXsWgD42MH66tr2zLj1v6D549HwkVv43Mj2z7f78eTvgULudSnxnXD0tZPuezX0so71wab0upzufTvvh0/rVADzrIfypn/2ya70u5fxlmPyoXjwh0myEfQ+AAAEh0lEQVRYhe2W25KqOhCGiUmnAwRlOCgqOsDovP8j7j8H0DVVuzZzt6o2XV4IhPClD393svs7LRF/pyWbbbbZZpv9T03IYEaFa4W/zqRcmoPA5atTGGne14cF7ob4eUMtd5R5Lcdma7hMUVVlWVbTQ7pX1d3We2e1PcZtRbuf6uUbZqrxzNZdmy63jn1V2v4sZ6r03Nmy3D9VJFDjfv+h5vf7+u1I/2qyYc6yjIiKFuvVR+aucUkzl+kyzuZtE2mJyS+wSVhgnhRsCmvUro439BCcboaM9S6sTgvSMvlvk4Xm0/1+AZ57Vd1J03DprtPir7xkzdO8l6yZp3GosKxKPcZ3hsfjQZP17lFnbNUcxgEHyGofXfUkzVUIdFpysYqrZL4ZZVSj6Wo8F3++55c4k9a6mWcRs2d6SpOPpLO7iqBfuZFH7Q8idpqpRhKa/K411Y5dnbAF9f4YacXlKq6KucWG5kTuBXGDc/L3BaYjW3H2mPPjynTC/89C02iCA8hFWSV+hbTMVR7C14LdrVUPdzR6+OWWq99wqW/ScLA4/+SSRfYciG36Jxf2p3H2V6nwz7Ooe6bpHM8gD6yL3CUt9sa+zqGppd9xwV8WXEcOcZyrWd0y3iVIoZi35uB5RN7orPXJ88DD4hbDbia4Kx7Bp4BbBC5GUBEP4bjqX3ApZLfzuecax/Fy6E3kwPkQHBrma3AZk37R/H15ABj1QaFSlFG3CBTg3XvgojN8Rlf5K65vmX8jS1yoxI61drqRlf6rwjTAQEFxmb+4bn0FXYkeTFInA5AZhyNB8lz0CUnOfeBqcRJkm3Fca4QVXNR2DJDJhcJxMXTUVkEZECVGbUkdoxa4nhk3w6shyHOJxPYa94MLxd4FrqMApOZzXq32F7VXLqZWhjKHvz59owk711x0w9AhPFe5cCG5+f52aiUPruKgJY7ka3mSBsrAJRKkWPH5C6777mhkOKTjatLlqXefk3fQNr4XOa5L3gPj+N4Q00vQBJTrSwd83h9F5PJlSde1+YXEur86rMv7Ny4DhSgra6sS+uPkwXN1xqlxGQOpZh2niwsZXHmO28mefVpCv3xXS3EcnHMlFwTv1Uh/cOUFlz6qn32sfycEvXF+pGvoQw9fiXksWUdsg64qLPJ67Lgcq0+xtVzFn1zQ+8Lrl/Rimzkn+Pvww00ELvQrc8p09vQVWBRt6tpQ6EsKIk+H3CgljwXTFPsQtbFJgWudrjYxQMGc7DfD5fJ1nUAhJxSFin5lPpgg7+ByBQFQ5QuWq8uhgWz6E7iGRuV4/+iRmTb07TFyJcaFeR2X5mx847pnce7B6REtmpu/6YkZX8GcQ5NxXadhahIhzphq2P1mPTMfxTwKHcIMYS5Z6PE4zpW4WcOl7FQ/37g+yrr2Y2HZCnWy9hKLXrRVbaFQmAutFwL1sLWFOIn02FVFYcdlYFXmZDHrlP1xrozBVnGAE6au7Zq5MJGzRMRNpR+kpZ9z1CxjSRi4fUHO99Qscpia01S+ajoRCjfSt8nayOUbbps1WJttttlmm2222T+OK0cIBMLjzQAAAABJRU5ErkJggg==)"
                  , "> ![G4](https://bytebucket.org/technowolf/wolfden_1/raw/6d4ce0e0b6f27612ede53642ca96e33d03218cd4/g4/media/images/g4logo_wolf.png?token=7bb2b0d4916016a8db3c603b6651ddaef5cb5168)"
                  , "> ![SoftServe](http://softserve.ua/html/img/softserve_logo-01.svg)"
               ])
             }
      , Node { title: "Contact"
             , path: "contacts"
             , children: []
             , dataSource: MemorySource (joinWith "\n" [
                  "[mailto:eugene.naumenko <$> gmail.com](eugene.naumenko <$> gmail.com)"
                , "[http://eugenen.github.io/](eugenen.github.io)"
                , "[https://drive.google.com/file/d/0B8speZLVOSYvYTZTMXVIbHVnZWM/view](CV)"
                , "[https://twitter.com/8gene](twitter.com/8gene)"
                , "[https://github.com/EugeneN](github.com/EugeneN)"
                , "[https://plus.google.com/+EugeneNaumenko/](https://plus.google.com/+EugeneNaumenko/)"
               ])
           }
      , Node { title: "Blog"
             , path: "blog"
             , children: []
             , dataSource: MemorySource "Nothing"
             }
      , Node { title: "Apps"
             , path: "apps"
             , children: []
             , dataSource: MemorySource (joinWith "\n" [
                  "[http://eugenen.github.io/C.MD](C.MD gist editor)"
                , "[http://eugenen.github.io/pureGoL](pureGoL)"
                , "[https://github.com/EugeneN/twic](twic)"
              ])
             }
      , Node { title: "Presentations"
             , path: "presentations"
             , children: []
             , dataSource: MemorySource (joinWith "\n" [
                  "[https://docs.google.com/presentation/d/1IOM9A3Otxufs5xzvYb3yPrT7JDVPhkJVkdaWvVl8R_E/pub?start=false&loop=false&delayms=3000](Purescript)"
                , "[https://docs.google.com/presentation/d/1lfbKvDcXfBdvdu76anAyTglo6O3vP68oYJUa0K-d7zo/pub?start=false&loop=false&delayms=3000](DNA)"
                , "[https://docs.google.com/presentation/d/1e5dyOXcSAp3UCCS3sJMBTttKDmQw8dDq3qYobsTvpKA/pub?start=false&loop=false&delayms=3000](Evolution of client side applications)"
              ])
             }
      , Node { title: "G4"
             , path: "g4"
             , children: []
             , dataSource: MemorySource (joinWith "\n" [
                  "![http://eugenen.github.io/g4/g4massive.png](Many objects)"
                , "![http://eugenen.github.io/g4/g4manywpts.jpg](Many waypoints)"
                , "![http://eugenen.github.io/g4/g4greenbtn1.png](Login screen)"
                , "![http://eugenen.github.io/g4/g4chartsnew.png](Track charts)"
                , "![http://eugenen.github.io/g4/g4evolving.png](More charts)"
                , "![http://eugenen.github.io/g4/g4r.jpg](Tracks comparison)"
              ])
             }
      ]
}
