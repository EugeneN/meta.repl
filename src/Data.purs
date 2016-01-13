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
  , menuPath: ["about"]
  , currentNode: Nothing
  , currentContent: Nothing
  , keyboardInput: Nothing
}

appDNA = Node {
    path: ""
  , title: "Eugene Naumenko"
  , dataSource: ChildSource "about"
  , processor: TextProcessor
  , pathProcessor: GlobalPP
  , children: [
        Node { title: "About"
             , path: "about"
             , processor: MdProcessor
             , pathProcessor: GlobalPP
             , children: [ ]
             , dataSource: aboutDS
             }
      , Node { title: "Photos"
             , path: "photos"
             , processor: ImgListProcessor
             , pathProcessor: ChildPP
            --  , inputProcessor: InputDriver
             , children: []
             , dataSource: photosDS
             }
      , Node { title: "CV"
             , path: "cv"
             , processor: MdProcessor
             , pathProcessor: GlobalPP
             , children: []
             , dataSource: GistSource "e1a6a166728f7d403fc4"
             }
      , Node { title: "Blog"
             , path: "blog"
             , processor: BlogProcessor
             , pathProcessor: ChildPP
             , children: [ ]
             , dataSource: GistSource "ff3d182ce385cebb1774"
             }
      , Node { title: "Apps"
             , path: "apps"
             , processor: TextProcessor
             , pathProcessor: GlobalPP
             , children: [ Node { title: "C.MD"
                                , path: "cmd"
                                , processor: MdProcessor
                                , pathProcessor: GlobalPP
                                , children: [ ]
                                , dataSource: cmdDS
                                }
                         , Node { title: "pureGoL"
                                , path: "pureGoL"
                                , processor: MdProcessor
                                , pathProcessor: GlobalPP
                                , children: [ ]
                                , dataSource: golDS
                                }
                         , Node { title: "twic"
                                , path: "twic"
                                , processor: MdProcessor
                                , pathProcessor: GlobalPP
                                , children: [ ]
                                , dataSource: twicDS
                                }
                         , Node { title: "meta.repl"
                                , path: "meta.repl"
                                , processor: MdProcessor
                                , pathProcessor: GlobalPP
                                , children: [ ]
                                , dataSource: metareplDS
                                }
                         , Node { title: "G4"
                                , path: "g4"
                                , processor: MdProcessor
                                , pathProcessor: GlobalPP
                                , children: []
                                , dataSource: g4DS
                                }
                         ]
             , dataSource: ChildSource "cmd"
             }
      , Node { title: "Talks"
             , path: "talks"
             , processor: MdProcessor
             , pathProcessor: GlobalPP
             , children: []
             , dataSource: talksDS
             }

      , Node { title: "Help"
             , path: "help"
             , processor: MdProcessor
             , pathProcessor: GlobalPP
             , children: []
             , dataSource: helpDS
             }
      ]
}

aboutDS = StringSource (unlines[
       "Hi, I'm Eugene Naumenko."
     , ""
     , "Software {engineer, architect} with 10 years of experience in networking, distributed and high load systems, web development, software design and project management."
     , ""
     , "Worked on online education systems, spam protection systems, online GIS systems, accounting and trading software, rich web applications, IT/network infrastructure systems."
     , "Created own small language for web development and a few open source utilities and applications."
     , ""
     , "Contacts: [e-mail](mailto:eugene.naumenko@gmail.com), [github](https://github.com/EugeneN), [twitter](https://twitter.com/8gene)."
     , ""
     , ""
     , "*"
     , ""
     , ""
     , "> ![Prom.ua](promua.png)"
     , "> ![Cisco](cisco.png)"
     , "> ![Pearson](pearson.png)"
     , "> ![G4](g4.png)"
     , "> ![SoftServe](softserve.png)"
  ])


photosDS = ArraySource [ "https://lh3.googleusercontent.com/eWwR1tPCh42kvp3g5cd2ajogkUhGp9ol0_N08gsJf8DvIAK8AI_0zRW0WaWUxnQ0eGhUlerXo_psMw=w1084-h610-no"
                        , "https://lh3.googleusercontent.com/-kkUBuYKmHSBTj5ad8yHfC5uRgH1V2edL9nnSgBY2GqFaDegNV02fCEQogrogXf60J0wmOW1PjrffQ=w1474-h1105-no"
                        , "https://lh3.googleusercontent.com/mR9FX06r0JlFByYSku73j9XBAAg-z8YC9jv3ack1kgRchM9XM0pdgNbTPHQhGsc4JGSGB_soVRJwLw=w1280-h853-no"
                        , "https://lh3.googleusercontent.com/waE6y-kWoGdJ_MuZWIx-8bH2ZVCno06xyu0ahwWELVqf2SS2Rcpd30-3sKH33oMNgwrQjCToRFcEQznAvl0TLl28OnW-HPJPPe_XHhLh00nZoXUjekl4ACAYiBT63DVyKDmQGaUlw7xqztbNl3cth8cOUIqM80f-fNhxyct2HeZWWB0da9UrPuTwjDW0fohPam-3BLRpOhc0zwRxVi4RYLbyivh1MLmPsxAu0OxChtX4ecRbHmYjzj7cm7M1ccPKvL2LfI9wqfWu23Z_mbSVy04D25ah0lVAD6XtOoMqRB8vAwAg8L4y3YWQMmdB6PA4gcG7sAxxDiCwahSDzsbA3cuwo2l_ENEm3mqzQCbwXNn0Qu5Rpa7DhicEtgQW67mdN5wQHjb5YIls5MnEusDTjCnSBSgf7EtSfKsu6uHMSJQD-qFmymAsqr4oRxPqw4PCwL4TrbnAQViNc96tveLFu41AI9Pfbb3SA6OVLaA671Y9U5FVGaFWf9UWB8_cCTuW-Dz_XYK6Mye3QJlaoZhzGKo0c2uq3Zhq6Sxpd13wnEk=w800-h600-no"
                        , "https://lh3.googleusercontent.com/C-kMUvnWjLxt1xrMPHPSK9n6BAbjznxA_1wD3xaItuiGf0-t2p1sjNGzDA3nih1mMTlZJBk1lROS7pCzT-0i-vTU2SUTGHHOF3X0HY-0dJasZJ_DYcM8mHU2_dXnFsVtkMWg2pi14aCfBW8OoodUmA3Yr0AirbxhEqEqSPaOXap6XN5jqQploGBzgiaiB1wtKDjD1XiVn7hqfs_wDQSarIhr8K51l4Xd1dnI1eyrrRCYTqxshME9vphC5Hk39903hCIhRKaOkz7KxpL2u4QeGWTNJcjrAsrgVhyR3BfvmHpMfAR9-4WxnHBqyUxGMrUfikdMg2FCWn9Itr8zk-xpjBVL2ryA9kcHfXp1hxu7Zx9nbp41vR0u2-R_6SPVbhcPsmY1DIPTRRv7xvNCIIY8jxcdvlkUIKdRR3aZrAsZfTo3GCRLLln-JgvgUJuYXTSnwJ2lqnMfOuDZGi4zm4Ix3CKH3Gn2wpWh6BLPv5dtMQ2YqjUZd21ybnBfqe29ItNdL9JxhGBzaPcqVTvL4Q8toqJIGvyjO89FQD_9YngNekg=w600-h800-no"
                        , "https://lh3.googleusercontent.com/Emg8T6PyLUioQrB2SbIJ2V58eLQ71PrsQDBPgpdvtgJsM4kQ3TWI5Gpb425FFV5rwu6rjkBXH2emutL-e0N5C3y-aQ7zfjvm2Nof9SiKEYSAGzb9qAqPILBVOoZbcEGbVemKGBqSnlpH-sgT1Yn1Qewhn3wFZoTFR9aBKROu2GLXNiqdUkwhmMsa6cB04sgE-5rxWV6YGJvPGnsZESqo0sjsTXWcG3H-irmNw1NQOeiTIrJfUjW8Er6BHbp20-kMh-adw_AFB9dAnKIWsznXszOTE6K-bLlBRJR2E45naH4DlRlVFdVCfbCFejKreZzi95PkEzteY__8m44L920IP_A46MOyWGxLsglmWGoQvxgzBr5PiEAYnKEdFaxy7PELhOR5vPubP-TNRgqTJZ0J25XbJvOIske-Na5VLXWHLShgrPCv1V5tsMQXHm6LnklEbMwpQlc7M1Gn2Y8wvMGNBjujZOnRMIinFO10zVdakPc2qRby4d2OOo7fOSV7qWRUCk6W33C-AAxdBCDIWLJl2WBLLb4yj7fxL3ILh3lquBtcJURmoA-garnKaeOj0_b-6jap=w600-h800-no"
                        , "https://lh3.googleusercontent.com/-yxRMkF93Hh21waoSUO5iMpgj4CRSV0Z8s_ht8gsPYBYobWbZ3BTttR6vnUwF5-tF9MqSg_ToyC7zMsjH_uAi3avFT5MjalPyeWxrXLKTzzqKQtyCWHzavafVUWvwy60p2mVrR63zvS9VRE7jxI6zoSlgJWayrZ-0T-4y52mhwXdN37-DjKKMbp7p6OQ_vbp_8LdN8QzEM6S2TkSko_PLKko4inzyLO45qoC6p5hgm4ZJaNWDxAZH5zxhf7RkKKYcd-5Le-CoGOLUfNG2dX7t5MBSfkhYFvO-nTg9Vlkhn5W4l5x-ZIn3nrukUyjQUsjuDfLz5iY_Hn6X0WiOwCkuqt44-lyj2WWUMPv6tvLzkqxmGK3tq4yZrMA80mxZfONXUh_ErgzBf_szFatRYmHX6N_sDzCMvElhJf0EjSU5Eqv95eJ4FJOLAlIid8X5rmI_Q-tyFw40h6_5CcKUrv7Td-4eAaUOWhPCu6-C9FrJb-q4s40allLKd9EIT3clNGWE4tlq5KWXcOCwDDLE7o8vtDsMnHqSBW9pacyCdZiH6zdFU07XrrGn8jZxRDw56c_YeFd=w540-h720-no"
                        , "https://lh3.googleusercontent.com/b92cOfZh56TqNKtBoTaQZeVRq_qE1NRDKZz6G_jvtF4HfRdPeLsXazrR8nhlMtzUeiSyHdrzl09k75MPl7GaW4Th5PyAYkB8N97q1PCydhiSn79r3EWe4er3DDwDL7g0PENL8J7bEfFpFD3EVG5CiiZi0gN6Vog9mhZLrb4TSDXxLHraepmneJ2XftY-DI-J_qrsjSv7tFH6XLMDDU5TNFeF-IlQOfjVO52u3TlZKCWUXn8s_CzmSLB3xeRJgh8iq71swiozi2sRFD1wCLRgcBRgUhbsJnLdk_aVSA1YyLy28jP9QhQhPVfLSwYDk8pRL8o4LrT2SHUKK7N6wU6ErOtyGjTRbQPn-Yq8ha27SYXa4moD0WSeNMFR4GSBYjlaVb7mYc348Sf9oeKgUuJMcmV6CS1t8AyzpuWBXTpA3uxmcrWylWyhURqpMXUg5qh_TTkvfrULyK0eLjd0zj23tnFuRa0mKjVD2KOYi2h-cqFTHc9gfGa7yWpYwtn44Uq_4oqluKnTBbtpeeAZEq2DI5c-epIGGIIBEO9CS_5gg6k=w800-h534-no"
                        , "https://lh3.googleusercontent.com/xr1xGfYfaljVuvZxrF0QaVrWQUQGFzRDmlISK6BC0qdS6r1TNW0GdAvQPV087YdsGP8aemGTOEbdzxy3dGzsUgkINdEg2iGlVvZRVCwwB8rDGB11noTzvzqNUj5uM8YAClWlTTMvCcvTSLe5EuUuWIbyOVLBywTm-TyVe7d2n_CMc6_LeGAcpSHvnHzmx106gaHQT7q_avdVcsXdPMp8GrXGV0EQP7YQLHdkDPAXdSe7xI-eqMclSJalGng7bntjBK3umPjxcfEowWwBS8z4I7o1k7UeaPFdXAu1IZ6L_Vp9bbG2JkRe60iRcq5de1CA5TBRG-dJu-Q8_2rOIzGR-2NxEbdPElokPiepb6r7jVOIkmjz9-IrmParyaFIQH-r5eVP3939SX_2RycZLcMc3Rw8UUJnCjAeIDGNP4l4UkKsXW2xGepmS4RuDNye9FA3qJLrxNsqRWMNKp4VpupvLWCuDsEjeTvKoz65Zhrv6hPrUHrdcMRqsdcVA06Wh5uWEDqet9yaWvFq4umThIuADHOH4y4Ft26zX_erbtRRpDs=w658-h441-no"
                        , "https://lh3.googleusercontent.com/h_j3F5mdoJR4NJmhmMHrWYJ30z7at9KbLAtrzouypKDRqEh_UXzei_m7EkR3pdYIbTZyC1BBOlV51hPzPNTbeh0hxiPrR35tDFgDZ_mlfhgfPH0TBq9QtW9StDP3W81wOEhI9ClycyxnuuZhpbnxtxSW6aCb7a3KhcpICfVZ7klIUo8cwjCyj_hHHey70fiPDBOFTmsi0ZzWXbUK8OcnashPoQoi34pHkcOGs_kpUsBDUjkXlsat7vtIB3fPTdkXrpyLkc_F6eRg6BCxwFyo88Fx4im1ZdtUrK3-rZ8FuvKjWE38R-oG4un7N3CkD-TKCYpRJyMXU4yWdeNAXiTQgq1MrvdhgFLfdMsEOHMqx5SkDOtGBJa6_SU9KbjEbMUvgxwFBCHUfBmXfZUNrc4HkDnJIt9zlkjlZDCO6OR-2WUi3qAs2_yfLMcjOKpS3BwipDYR95hd158K4_DOpsKYLKpfEwVBnk7IzwGN6Gpb4ZV9YYwOHTwFeqSHyxfni4tTCp_taoh4dANKTmjEwOqUtYIeel11DB4CvK6HUDd7GXQ=w441-h658-no"
                        ]

helpDS = StringSource (unlines [
     "This «web site» is a *concept application* aimed to explore ways to reach *The Holy Grail* of software engineering -"
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
   , "- [conventional HTML-based](?ui=html#!about), rendered using virtual dom;"
   , "- [REPL-based](?ui=console#!about) using Javascript console. After switching to this mode one has to open Javascript console and "
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

talksDS = StringSource (unlines [
     "[Purescript](https://docs.google.com/presentation/d/1IOM9A3Otxufs5xzvYb3yPrT7JDVPhkJVkdaWvVl8R_E/pub?start=false&loop=false&delayms=3000)"
   , ""
   , ""
   , "[DNA](https://docs.google.com/presentation/d/1lfbKvDcXfBdvdu76anAyTglo6O3vP68oYJUa0K-d7zo/pub?start=false&loop=false&delayms=3000)"
   , ""
   , ""
   , "[Evolution of client side applications](https://docs.google.com/presentation/d/1e5dyOXcSAp3UCCS3sJMBTttKDmQw8dDq3qYobsTvpKA/pub?start=false&loop=false&delayms=3000)"
 ])

g4DS = StringSource (unlines [
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

metareplDS = StringSource (unlines [
     "This application. Experiments with better ways to build apllications."
   , ""
   , "[meta.repl](https://github.com/EugeneN/meta.repl) (draft)"
   , "# ![meta.repl](meta.repl.png)"
   ])

twicDS = StringSource (unlines [
     "Experimental Twitter client written in Haskell and Purescript with clean UI and simple UX. "
   , "Written to explore component models for web applications and usage of immutable cloud database for eventual consistency."
   , ""
   , "[twic](https://github.com/EugeneN/twic)"
   , "# ![twic](twic.png)"
   , ""
   ])

golDS = StringSource (unlines [
     "“Game of Life” game written in Purescript as a research on"
   , "decoupling and encapsulating state between user interface, application core and input effects in reactive web applications. "
   , "Features a stateful core and few distinct interchangeable stateful user interfaces."
   , ""
   , "[pureGoL](http://eugenen.github.io/pureGoL)"
   , "# ![gol](gol.png)"
   , ""
   ])

cmdDS = StringSource (unlines [
     "This is a rich markdown editor for Github Gists, a 100% client side application written in ClojureScript to explore offline mode and multiprocessing for web applications."
   , ""
   , "[C.MD](http://eugenen.github.io/C.MD)"
   , "# ![cmd](cmd.png)"
   , ""
 ])
