      *****************************************************************
      *    COPY-MEMBER: DRKI3973                                      * 
      *                                                               *
      *                                                               *
      *    Aktualisierung der Tabelle GEIGERVERTRZUSATZ/TCKI0559      *
      *    von G79 (Geiger)                                           *
      *                                                               *
      *                                                               *
      *    SATZLÄNGE 100B                                             *
      *                                                               *
      *                                                               *
      *                                                               *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      *DBO10080    !09.09.22!neu                           !u131774   *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
       01  'XX'-SATZ.
           05  'XX'-GES                     PIC  9(002).
           05  'XX'-VNR                     PIC  9(015).
        
           05  'XX'-KUNDENIDEXT             PIC  9(008).
           05  'XX'-AEND-KUNDENIDEXT-KZN    PIC  X(001).
      *        J WENN KUNDENIDEXT GEAENDERT

           05  'XX'-FLATRATE                PIC  X(001).
           05  'XX'-AEND-FLATRATE-KZN       PIC  X(001).
      *        J WENN FLATRATE GEAENDERT

           05  'XX'-KNDRABATT               PIC  9(003)V99.
           05  'XX'-AEND-KNDRABATT-KZN      PIC  X(001).
      *        J WENN KNDRABATT GEAENDERT

           05  'XX'-KUMAUSLIEFGB            PIC  9(005).
           05  'XX'-AEND-KUMAUSLIEFGB-KZN   PIC  X(001).
      *        J WENN KUMAUSLIEFGB GEAENDERT

           05  'XX'-KUMAUSLIEFGM            PIC  9(005).
           05  'XX'-AEND-KUMAUSLIEFGM-KZN   PIC  X(001).
      *        J WENN KUMAUSLIEFGM GEAENDERT

           05  'XX'-KUMAUSLIEFSBB           PIC  9(005).
           05  'XX'-AEND-KUMAUSLIEFSBB-KZN  PIC  X(001).
      *        J WENN KUMAUSLIEFSBB GEAENDERT

           05  'XX'-KUMAUSLIEFSBZ           PIC  9(005).
           05  'XX'-AEND-KUMAUSLIEFSBZ-KZN  PIC  X(001).
      *        J WENN KUMAUSLIEFSBZ GEAENDERT

           05  'XX'-KUMAUSLIEFSM            PIC  9(005).
           05  'XX'-AEND-KUMAUSLIEFSM-KZN   PIC  X(001).
      *        J WENN KUMAUSLIEFSM GEAENDERT

           05  FILLER                      PIC  X(035).

           05  'XX'-AKTUAL-KZN              PIC  X(001).
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      
      *****************************************************************
      *---------------------- ENDE DRKI3973 --------------------------* 
      *****************************************************************
