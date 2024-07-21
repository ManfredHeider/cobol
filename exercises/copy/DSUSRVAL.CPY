CCC008*01 DS-User-Validation-Linkage.
          03 DS-UV-Field-No           PIC 9(4) COMP-X.
          03 DS-UV-Return-Status      PIC 9(4) COMP-X.
             78 DS-UV-Field-Valid        VALUE 0.
             78 DS-UV-Field-Invalid      VALUE 1.
             78 DS-UV-Use-Error-Msg-No   VALUE 2.
             78 DS-UV-Use-Error-Msg      VALUE 4.
          03 DS-UV-Error-Msg-No       PIC 9(2) COMP-X.
          03 DS-UV-Error-Msg          PIC X(251).
