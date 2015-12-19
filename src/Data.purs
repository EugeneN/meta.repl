module Data where

import Data.String (joinWith)

import Data.Maybe (Maybe(..))
import Data.Array
import Types
import Utils (unlines)

initialState :: AppState
initialState = AppState {
    actionsCount: 0
  , currentPath: ["about"]
  , currentContent: Nothing
}


appDNA = Node {
    path: ""
  , title: "Eugene Naumenko"
  , dataSource: ChildSource "about"
  , processor: TextProcessor
  , children: [
        Node { title: "About"
             , path: "about"
             , processor: TextProcessor
             , children: [ ]
             , dataSource: StringSource (unlines[
                    "Hi, I'm Eugene Naumenko."
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
      , Node { title: "Contacts"
             , path: "contacts"
             , processor: TextProcessor
             , children: [ Node { title: "Email"
                                , path: "email"
                                , processor: TextProcessor
                                , children: []
                                , dataSource: StringSource "[eugene.naumenko^at^gmail.com](mailto:eugene.naumenko^gmail.com)"
                                }
                         , Node { title: "CV"
                                , path: "cv"
                                , processor: GistProcessor
                                , children: []
                                , dataSource: StringSource "e1a6a166728f7d403fc4"
                                }
                         , Node { title: "Social"
                                , path: "social"
                                , processor: TextProcessor
                                , children: []
                                , dataSource: StringSource (unlines [
                                       "[eugenen.github.io](http://eugenen.github.io)"
                                     , ""
                                     , ""
                                     , "[github.com/EugeneN](https://github.com/EugeneN)"
                                     , ""
                                     , ""
                                     , "[twitter.com/8gene](https://twitter.com/8gene)"
                                     , ""
                                     , ""
                                     , "[plus.google.com/+EugeneNaumenko](https://plus.google.com/+EugeneNaumenko/)"
                                    ])
                                }
                         ]
             , dataSource: ChildSource "email"
           }
      , Node { title: "Blog"
             , path: "blog"
             , processor: TextProcessor
             , children: [ Node { title: "DNA Architecture"
                                , path: "dna-arch"
                                , processor: GistProcessor
                                , children: [ ]
                                , dataSource: StringSource "cdc388425dacf87cba71"
                                }
                         , Node { title: "Cats"
                                , path: "cats"
                                , processor: GistProcessor
                                , children: []
                                , dataSource: StringSource "6c6a56e4abb7ff7fa7fa"
                                }
                         , Node { title: "Photos"
                                , path: "photos"
                                , processor: ImgListProcessor
                                , children: []
                                , dataSource: ArraySource [ "https://lh3.googleusercontent.com/-bD8uEjQ5QC8/Um_5YVL6Q4I/AAAAAAAAbbo/9HJcdtzoLy8/w1084-h610-n-rw-no/2012-02-25%2B16.46.32.jpg"
                                                          , "https://lh3.googleusercontent.com/-5Ir-YW-nLfGu2H1bh4MUoQ3gnAhp1MvaO0Eb2OAUDuf_gb8RV7m5R2LAT1JvcVBRcONl-QV5cOLcIPJsIxxJet-J3H5ECns1giKvmmFJLtrgNbERfjqAWsO6m5l-foSMpzaWuHCMfyKYBDWwJKXqi-rwTSaq8i7TLprs4zNSSQj2dhtWycDSTExqoTKgr2vKsvZO1RSssDcftssbf64mYmVyzxi-sNwPd2o2HtWV_XfSY9Dq4hWn5EL-KavoJb580jb5ebrOCZheTG8WcS9pZVfdfHblSHTdbdDlS7LaTKPVMMDRDyob5ebEHfx86Un8aIDu5b46uTbKe9nQpJdyDfALtl-recFdzNs-KDmYg5U6Brf8G0Ffcy-2QXWZP0wHJKD0ylCgyspd1F4Ykca4pSF8aTGf4r8e1FzR0aRVsDvYpB18UocPOxlH2MJN4T2ZyJN5B0eA997541o_66r1tRUVkVNT8GJWCKRsmgGDbqCy4ytX7hYPgDUSPqyouZ3fRIIUChRfkYXV1jSCbgfZp38hJLT4c16yg8F24NvCbA9DDfkqLT7UJvGw2iTJW_c9a7vGQ=w1474-h1105-no"
                                                          , "https://lh3.googleusercontent.com/fQFjxLCfIXqPivs53G8e1-t7HCfKtNhF5MLRUX9wIXW1CusE9H8ur-ifvJ5Qloe8AUd9gxzkYywC0n0HuSfVkDjas1lg0j_oyByc3qmyyTEf6U1rE7wGfJI9_1ySaw9bl8szG8IHr9wpMBkKGzTgd8KRL3_cal6lghiOwa-U_SUd4CGKFx7NluyB3hEg_hnbuGMyPk8fTZj9TgOhEoeYRwyzWIGOMLuuCQK5s43NLv-qu61ZyFA5JHf4i6jHKvok7KaDN-qIPFGJon3z0emjBCf2jIZRJVCvgjuNHkF4rOeZYEIKBYYY30VJULgMmp8H3SaemIXxeja8XpOdidrPTXjfigP4iEf1KkN5IIBWfSmAss8PzyfYqbgcZMcUb4Nb8ti2dbhpeQbmhFIcp7ZAoIm6tl9v-IBPhfmev0gk34RRCgTBkMSgWV7PZkmY-oTZcJ_duFwzrXtIKfNzYvfDbr7zTViDKrOxOJ1plzuUT7ycl8QTbsGsCN_LJrY5rhlhUXeRPegjL-ltAqughqeMbrxbE6ctB24c136FfFjOsYRm-0OWfXKwp2rwW_Mi6PHKVFXvRg=w1280-h853-no"
                                                          , "https://lh3.googleusercontent.com/waE6y-kWoGdJ_MuZWIx-8bH2ZVCno06xyu0ahwWELVqf2SS2Rcpd30-3sKH33oMNgwrQjCToRFcEQznAvl0TLl28OnW-HPJPPe_XHhLh00nZoXUjekl4ACAYiBT63DVyKDmQGaUlw7xqztbNl3cth8cOUIqM80f-fNhxyct2HeZWWB0da9UrPuTwjDW0fohPam-3BLRpOhc0zwRxVi4RYLbyivh1MLmPsxAu0OxChtX4ecRbHmYjzj7cm7M1ccPKvL2LfI9wqfWu23Z_mbSVy04D25ah0lVAD6XtOoMqRB8vAwAg8L4y3YWQMmdB6PA4gcG7sAxxDiCwahSDzsbA3cuwo2l_ENEm3mqzQCbwXNn0Qu5Rpa7DhicEtgQW67mdN5wQHjb5YIls5MnEusDTjCnSBSgf7EtSfKsu6uHMSJQD-qFmymAsqr4oRxPqw4PCwL4TrbnAQViNc96tveLFu41AI9Pfbb3SA6OVLaA671Y9U5FVGaFWf9UWB8_cCTuW-Dz_XYK6Mye3QJlaoZhzGKo0c2uq3Zhq6Sxpd13wnEk=w800-h600-no"
                                                          , "https://lh3.googleusercontent.com/C-kMUvnWjLxt1xrMPHPSK9n6BAbjznxA_1wD3xaItuiGf0-t2p1sjNGzDA3nih1mMTlZJBk1lROS7pCzT-0i-vTU2SUTGHHOF3X0HY-0dJasZJ_DYcM8mHU2_dXnFsVtkMWg2pi14aCfBW8OoodUmA3Yr0AirbxhEqEqSPaOXap6XN5jqQploGBzgiaiB1wtKDjD1XiVn7hqfs_wDQSarIhr8K51l4Xd1dnI1eyrrRCYTqxshME9vphC5Hk39903hCIhRKaOkz7KxpL2u4QeGWTNJcjrAsrgVhyR3BfvmHpMfAR9-4WxnHBqyUxGMrUfikdMg2FCWn9Itr8zk-xpjBVL2ryA9kcHfXp1hxu7Zx9nbp41vR0u2-R_6SPVbhcPsmY1DIPTRRv7xvNCIIY8jxcdvlkUIKdRR3aZrAsZfTo3GCRLLln-JgvgUJuYXTSnwJ2lqnMfOuDZGi4zm4Ix3CKH3Gn2wpWh6BLPv5dtMQ2YqjUZd21ybnBfqe29ItNdL9JxhGBzaPcqVTvL4Q8toqJIGvyjO89FQD_9YngNekg=w600-h800-no"
                                                          , "https://lh3.googleusercontent.com/Emg8T6PyLUioQrB2SbIJ2V58eLQ71PrsQDBPgpdvtgJsM4kQ3TWI5Gpb425FFV5rwu6rjkBXH2emutL-e0N5C3y-aQ7zfjvm2Nof9SiKEYSAGzb9qAqPILBVOoZbcEGbVemKGBqSnlpH-sgT1Yn1Qewhn3wFZoTFR9aBKROu2GLXNiqdUkwhmMsa6cB04sgE-5rxWV6YGJvPGnsZESqo0sjsTXWcG3H-irmNw1NQOeiTIrJfUjW8Er6BHbp20-kMh-adw_AFB9dAnKIWsznXszOTE6K-bLlBRJR2E45naH4DlRlVFdVCfbCFejKreZzi95PkEzteY__8m44L920IP_A46MOyWGxLsglmWGoQvxgzBr5PiEAYnKEdFaxy7PELhOR5vPubP-TNRgqTJZ0J25XbJvOIske-Na5VLXWHLShgrPCv1V5tsMQXHm6LnklEbMwpQlc7M1Gn2Y8wvMGNBjujZOnRMIinFO10zVdakPc2qRby4d2OOo7fOSV7qWRUCk6W33C-AAxdBCDIWLJl2WBLLb4yj7fxL3ILh3lquBtcJURmoA-garnKaeOj0_b-6jap=w600-h800-no"
                                                          , "https://lh3.googleusercontent.com/-yxRMkF93Hh21waoSUO5iMpgj4CRSV0Z8s_ht8gsPYBYobWbZ3BTttR6vnUwF5-tF9MqSg_ToyC7zMsjH_uAi3avFT5MjalPyeWxrXLKTzzqKQtyCWHzavafVUWvwy60p2mVrR63zvS9VRE7jxI6zoSlgJWayrZ-0T-4y52mhwXdN37-DjKKMbp7p6OQ_vbp_8LdN8QzEM6S2TkSko_PLKko4inzyLO45qoC6p5hgm4ZJaNWDxAZH5zxhf7RkKKYcd-5Le-CoGOLUfNG2dX7t5MBSfkhYFvO-nTg9Vlkhn5W4l5x-ZIn3nrukUyjQUsjuDfLz5iY_Hn6X0WiOwCkuqt44-lyj2WWUMPv6tvLzkqxmGK3tq4yZrMA80mxZfONXUh_ErgzBf_szFatRYmHX6N_sDzCMvElhJf0EjSU5Eqv95eJ4FJOLAlIid8X5rmI_Q-tyFw40h6_5CcKUrv7Td-4eAaUOWhPCu6-C9FrJb-q4s40allLKd9EIT3clNGWE4tlq5KWXcOCwDDLE7o8vtDsMnHqSBW9pacyCdZiH6zdFU07XrrGn8jZxRDw56c_YeFd=w540-h720-no"
                                                          , "https://lh3.googleusercontent.com/b92cOfZh56TqNKtBoTaQZeVRq_qE1NRDKZz6G_jvtF4HfRdPeLsXazrR8nhlMtzUeiSyHdrzl09k75MPl7GaW4Th5PyAYkB8N97q1PCydhiSn79r3EWe4er3DDwDL7g0PENL8J7bEfFpFD3EVG5CiiZi0gN6Vog9mhZLrb4TSDXxLHraepmneJ2XftY-DI-J_qrsjSv7tFH6XLMDDU5TNFeF-IlQOfjVO52u3TlZKCWUXn8s_CzmSLB3xeRJgh8iq71swiozi2sRFD1wCLRgcBRgUhbsJnLdk_aVSA1YyLy28jP9QhQhPVfLSwYDk8pRL8o4LrT2SHUKK7N6wU6ErOtyGjTRbQPn-Yq8ha27SYXa4moD0WSeNMFR4GSBYjlaVb7mYc348Sf9oeKgUuJMcmV6CS1t8AyzpuWBXTpA3uxmcrWylWyhURqpMXUg5qh_TTkvfrULyK0eLjd0zj23tnFuRa0mKjVD2KOYi2h-cqFTHc9gfGa7yWpYwtn44Uq_4oqluKnTBbtpeeAZEq2DI5c-epIGGIIBEO9CS_5gg6k=w800-h534-no"
                                                          , "https://lh3.googleusercontent.com/xr1xGfYfaljVuvZxrF0QaVrWQUQGFzRDmlISK6BC0qdS6r1TNW0GdAvQPV087YdsGP8aemGTOEbdzxy3dGzsUgkINdEg2iGlVvZRVCwwB8rDGB11noTzvzqNUj5uM8YAClWlTTMvCcvTSLe5EuUuWIbyOVLBywTm-TyVe7d2n_CMc6_LeGAcpSHvnHzmx106gaHQT7q_avdVcsXdPMp8GrXGV0EQP7YQLHdkDPAXdSe7xI-eqMclSJalGng7bntjBK3umPjxcfEowWwBS8z4I7o1k7UeaPFdXAu1IZ6L_Vp9bbG2JkRe60iRcq5de1CA5TBRG-dJu-Q8_2rOIzGR-2NxEbdPElokPiepb6r7jVOIkmjz9-IrmParyaFIQH-r5eVP3939SX_2RycZLcMc3Rw8UUJnCjAeIDGNP4l4UkKsXW2xGepmS4RuDNye9FA3qJLrxNsqRWMNKp4VpupvLWCuDsEjeTvKoz65Zhrv6hPrUHrdcMRqsdcVA06Wh5uWEDqet9yaWvFq4umThIuADHOH4y4Ft26zX_erbtRRpDs=w658-h441-no"
                                                          , "https://lh3.googleusercontent.com/h_j3F5mdoJR4NJmhmMHrWYJ30z7at9KbLAtrzouypKDRqEh_UXzei_m7EkR3pdYIbTZyC1BBOlV51hPzPNTbeh0hxiPrR35tDFgDZ_mlfhgfPH0TBq9QtW9StDP3W81wOEhI9ClycyxnuuZhpbnxtxSW6aCb7a3KhcpICfVZ7klIUo8cwjCyj_hHHey70fiPDBOFTmsi0ZzWXbUK8OcnashPoQoi34pHkcOGs_kpUsBDUjkXlsat7vtIB3fPTdkXrpyLkc_F6eRg6BCxwFyo88Fx4im1ZdtUrK3-rZ8FuvKjWE38R-oG4un7N3CkD-TKCYpRJyMXU4yWdeNAXiTQgq1MrvdhgFLfdMsEOHMqx5SkDOtGBJa6_SU9KbjEbMUvgxwFBCHUfBmXfZUNrc4HkDnJIt9zlkjlZDCO6OR-2WUi3qAs2_yfLMcjOKpS3BwipDYR95hd158K4_DOpsKYLKpfEwVBnk7IzwGN6Gpb4ZV9YYwOHTwFeqSHyxfni4tTCp_taoh4dANKTmjEwOqUtYIeel11DB4CvK6HUDd7GXQ=w441-h658-no"
                                                          ]
                                }
                         ]
             , dataSource: ChildSource "dna-arch"
             }
      , Node { title: "Apps"
             , path: "apps"
             , processor: TextProcessor
             , children: [ Node { title: "C.MD"
                                , path: "cmd"
                                , processor: TextProcessor
                                , children: [ ]
                                , dataSource: StringSource (unlines [
                                     "This is a rich markdown editor for Github Gists, a 100% client side application written in ClojureScript to explore offline mode and multiprocessing for web applications."
                                   , ""
                                   , "[C.MD](http://eugenen.github.io/C.MD)"
                                   , "# ![cmd](cmd.png)"
                                   , ""
                                 ])
                                }
                         , Node { title: "pureGoL"
                                , path: "pureGoL"
                                , processor: TextProcessor
                                , children: [ ]
                                , dataSource: StringSource (unlines [
                                     "“Game of Life” game written in Purescript as a research on"
                                   , "decoupling and encapsulating state between user interface, application core and input effects in reactive web applications. "
                                   , "Features a stateful core and few distinct interchangeable stateful user interfaces."
                                   , ""
                                   , "[pureGoL](http://eugenen.github.io/pureGoL)"
                                   , "# ![gol](gol.png)"
                                   , ""
                                   ])
                                }
                         , Node { title: "twic"
                                , path: "twic"
                                , processor: TextProcessor
                                , children: [ ]
                                , dataSource: StringSource (unlines [
                                     "Experimental Twitter client written in Haskell and Purescript with clean UI and simple UX. "
                                   , "Written to explore component models for web applications and usage of immutable cloud database for eventual consistency."
                                   , ""
                                   , "[twic](https://github.com/EugeneN/twic)"
                                   , "# ![twic](twic.png)"
                                   , ""
                                   ])
                                }
                         , Node { title: "meta.repl"
                                , path: "meta.repl"
                                , processor: TextProcessor
                                , children: [ ]
                                , dataSource: StringSource (unlines [
                                     "This application. Experiments with better ways to build apllications."
                                   , ""
                                   , "[meta.repl](https://github.com/EugeneN/meta.repl) (draft)"
                                   , "# ![meta.repl](meta.repl.png)"
                                   ])
                                }
                         ]
             , dataSource: ChildSource "cmd"
             }
      , Node { title: "Presentations"
             , path: "presentations"
             , processor: TextProcessor
             , children: []
             , dataSource: StringSource (unlines [
                  "[Purescript](https://docs.google.com/presentation/d/1IOM9A3Otxufs5xzvYb3yPrT7JDVPhkJVkdaWvVl8R_E/pub?start=false&loop=false&delayms=3000)"
                , ""
                , ""
                , "[DNA](https://docs.google.com/presentation/d/1lfbKvDcXfBdvdu76anAyTglo6O3vP68oYJUa0K-d7zo/pub?start=false&loop=false&delayms=3000)"
                , ""
                , ""
                , "[Evolution of client side applications](https://docs.google.com/presentation/d/1e5dyOXcSAp3UCCS3sJMBTttKDmQw8dDq3qYobsTvpKA/pub?start=false&loop=false&delayms=3000)"
              ])
             }
      , Node { title: "G4"
             , path: "g4"
             , processor: TextProcessor
             , children: []
             , dataSource: StringSource (unlines [
                  "G4 is a rich web application for managing geospatial data – GPS tracks, waypoints, geotagged items, custom maps etc. "
                , "The application features complex geographical/geometric calculations, automatic categorization and clustering of data, "
                , "real-time push based communications, external RESTful API, spatial search, uses advanced HTML5 "
                , "features like drag-and-drop multi-file upload, sound effects, interactive svg charting etc."
                , ""
                , ""
                , "The application is currently offline, but you can see some screenshots here:"
                , ""
                , "# ![Many objects](http://eugenen.github.io/g4/g4massive.png)"
                , "# ![Many waypoints](http://eugenen.github.io/g4/g4manywpts.jpg)"
                , "# ![Login screen](http://eugenen.github.io/g4/g4greenbtn1.png)"
                , "# ![Track charts](http://eugenen.github.io/g4/g4chartsnew.png)"
                , "# ![More charts](http://eugenen.github.io/g4/g4evolving.png)"
                , "# ![Tracks comparison](http://eugenen.github.io/g4/g4r.jpg)"
              ])
             }
      , Node { title: "Help"
             , path: "help"
             , processor: TextProcessor
             , children: []
             , dataSource: StringSource (unlines [
                  "This «web site» is a concept **application** aimed to explore ways to reach *The Holy Grail* of software engineering -"
                , "*composability* and *reusability*. These ways look like following SOLID, GRASP, "
                , "and reinventing «OOP in a large» using [purely functional language with powerful type system](http://www.purescript.org/), "
                , "high level abstractions, immutable data, messaging, some Category Theory and engineering approach for the win."
                , "Currently it is at a very early stage. Github link: [https://github.com/EugeneN/meta.repl](https://github.com/EugeneN/meta.repl) "
                , ""
                , ""
                , "The application can be run either in browsers or under node.js - 100% *isomorphic* application :-)"
                , ""
                , ""
                , "To run it in a browser, just open [eugenen.github.io](http://eugenen.github.io/) and then follow instructions. "
                , "There are 2 distinct user interfaces for browsers: "
                , "- [conventional HTML-based](?ui=html#about), rendered using virtual dom;"
                , "- [REPL-based](?ui=console#about) using Javascript console. After switching to this mode one has to open Javascript console and "
                , "  use functions provided to interact with the application."
                , ""
                , ""
                , "Another option is to run the application without a browser. Just save the very same [app.js](app.js) file, which is used "
                , "in browsers, to your filesystem, then run it with `node.js` and connect using `telnet`:"
                , ""
                , "```"
                , "$ wget http://eugenen.github.io/app.js"
                , "$ node app.js"
                , "```"
                , ""
                , "and in another terminal:"
                , ""
                , "```"
                , "$ telnet localhost 8888"
                , "```"
                , ""
                , "Then follow prompts and input commands to interact with the application :-)"
                , ""
                , ""
                , "_"
                , ""
                , ""
                , "Here be dragons."
                ])
             }
      ]
}
