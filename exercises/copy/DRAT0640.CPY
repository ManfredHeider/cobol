      *****************************************************************
      *    COPY-MEMBER: DRAT0640
      *
      *    KONSTANTEN FÜR TBAT0038/KTY 9
      *                            VERTRAGSZUSTAND
      *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------*
      * DBO-10293  !23.08.22! NEU                          ! T13AC20  
      * -----------!--------!------------------------------!----------
      *            !        !                              !
      *            !        !                              !
      *
      * ENDE-LOGBUCH
      *****************************************************************
       01 FILLER.
          05 K-KNR-UNBEKANNT             PIC S9(004) BINARY VALUE -2.
          05 K-KNR-LEER                  PIC S9(004) BINARY VALUE -1.
          05 K-KNR-AKTIV                 PIC S9(004) BINARY VALUE 0.
          05 K-KNR-VERTRAG-RUHT          PIC S9(004) BINARY VALUE 1.
          05 K-KNR-WIEDERINKRAFTSETZUNG  PIC S9(004) BINARY VALUE 2.
          05 K-KNR-KURZFRISTIG           PIC S9(004) BINARY VALUE 3.
          05 K-KNR-WAGNISWEGFALL         PIC S9(004) BINARY VALUE 4.
          05 K-KNR-STORNO-VORGEMERKT     PIC S9(004) BINARY VALUE 5.
          05 K-KNR-STORNIERT-ABGELAUFEN  PIC S9(004) BINARY VALUE 6.
          05 K-KNR-GESTORBEN             PIC S9(004) BINARY VALUE 7.
          05 K-KNR-ABL-VOLLSTUNDUNG-VORG PIC S9(004) BINARY VALUE 8.
          05 K-KNR-ABL-TEILSTUNDUNG-VORG PIC S9(004) BINARY VALUE 9.
          05 K-KNR-DEPOT-AUFGELOEST      PIC S9(004) BINARY VALUE 10.
          05 K-KNR-DEPOT-GELOESCHT       PIC S9(004) BINARY VALUE 11.
          05 K-KNR-INFOSPERRE            PIC S9(004) BINARY VALUE 12.
          05 K-KNR-AKTIV-BEITRAGSFREI    PIC S9(004) BINARY VALUE 13.
          05 K-KNR-KORRESPONDENZMAKLER   PIC S9(004) BINARY VALUE 14.
          05 K-KNR-HAUSTARIF             PIC S9(004) BINARY VALUE 15.
      *****************************************************************
      *---------------------- ENDE DRAT0640 --------------------------*
      *****************************************************************