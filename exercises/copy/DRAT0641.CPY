      *****************************************************************
      *    COPY-MEMBER: DRAT0641
      *
      *    KONSTANTEN FÜR TBAT0038/KTY 8
      *                            ZAHLWEISE
      *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------*
      * DBO-10293  !29.08.22! NEU                          ! T13AC20  
      * -----------!--------!------------------------------!----------
      *            !        !                              !
      *            !        !                              !
      *
      * ENDE-LOGBUCH
      *****************************************************************
       01 FILLER.
          05 K-KNR-ZAHLWEISE-UNBEKANNT   PIC S9(004) BINARY VALUE -2.
          05 K-KNR-ZAHLWEISE-LEER        PIC S9(004) BINARY VALUE -1.
          05 K-KNR-JAEHRLICH             PIC S9(004) BINARY VALUE 1.
          05 K-KNR-HALBJAEHRLICH         PIC S9(004) BINARY VALUE 2.
          05 K-KNR-ALLE-3-JAHRE          PIC S9(004) BINARY VALUE 3.
          05 K-KNR-VIERTELJAEHRLICH      PIC S9(004) BINARY VALUE 4.
          05 K-KNR-ZWEIMONATLICH         PIC S9(004) BINARY VALUE 5.
          05 K-KNR-MONATLICH             PIC S9(004) BINARY VALUE 6.
          05 K-KNR-EINMALZAHLUNG         PIC S9(004) BINARY VALUE 7.
          05 K-KNR-EINMALPRAEMIE         PIC S9(004) BINARY VALUE 8.
          05 K-KNR-KURZFRISTIG-UNTER-12-MON
                                         PIC S9(004) BINARY VALUE 10.
          05 K-KNR-JAEHRLICH-O-SKONTO    PIC S9(004) BINARY VALUE 11.
      *****************************************************************
      *---------------------- ENDE DRAT0641 --------------------------*
      *****************************************************************