      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0363                                     *
      *                                                              *
      *    KONSTANTEN FÜR DIE AUSPRÄGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    VERTRAGSZUSTAND - TCKI0363                                *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!30.06.98!01.01! NEUERSTELLUNG            !V.D.GOLTZ *
      * --------!--------!-----!--------------------------!----------*
      * AKI00001! 1.09.98!01.02! VZUST-UNBEKANNT mit Value!K-H Müller*
      *         !        !     ! '-2' (lt. Prot. v.3.8.98)!          *
      *         !        !     ! versehen                 !          *
      * --------!--------!-----!--------------------------!----------*
      * AKI00001!11.09.98!01.03! VZUST-LEER mit Value     !v.d. Goltz*
      *         !        !     ! '-1' (lt. Prot. v.3.8.98)!          *
      *         !        !     ! versehen                 !          *
      *---------!--------!-----!--------------------------!----------*
      * AKI01462!01.09.16!01.04! Neue Ausprägungen 14, 15 ! HSE      *
      *         !        !     ! aufgenommen. Neuer Vertr-!          *
      *         !        !     ! zustand bei Maklerbetreu.!          *
      *         !        !     ! und Haustarif            !          *
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------*
      * DBO-10293  !26.08.22! NEUE AUSPRÄGUNG 13           ! T13AC20  
      * -----------!--------!------------------------------!----------
      *            !        !                              !
      *            !        !                              !
      *
      * ENDE-LOGBUCH
      ****************************************************************
       01  FILLER.
           05 K-AKTIV                    PIC S9(004) BINARY VALUE 0.
           05 K-VERTRAG-RUHT             PIC S9(004) BINARY VALUE 1.
           05 K-WIEDERINKRAFTSETZUNG     PIC S9(004) BINARY VALUE 2.
           05 K-KURZFRISTIG              PIC S9(004) BINARY VALUE 3.
           05 K-WAGNISWEGFALL            PIC S9(004) BINARY VALUE 4.
           05 K-STORNO-VORGEMERKT        PIC S9(004) BINARY VALUE 5.
           05 K-STORNIERT-ABGELAUFEN     PIC S9(004) BINARY VALUE 6.
           05 K-GESTORBEN                PIC S9(004) BINARY VALUE 7.
           05 K-ABL-VOLLSTUNDUNG-VORG    PIC S9(004) BINARY VALUE 8.
           05 K-ABL-TEILSTUNDUNG-VORG    PIC S9(004) BINARY VALUE 9.
           05 K-DEPOT-AUFGELOEST         PIC S9(004) BINARY VALUE 10.
           05 K-DEPOT-GELOESCHT          PIC S9(004) BINARY VALUE 11.
           05 K-INFOSPERRE               PIC S9(004) BINARY VALUE 12.
           05 K-AKTIV-BEITRAGSFREI       PIC S9(004) BINARY VALUE 13.
           05 K-KORRESPONDENZMAKLER      PIC S9(004) BINARY VALUE 14.
           05 K-HAUSTARIF                PIC S9(004) BINARY VALUE 15.
           05 K-VZUST-UNBEKANNT          PIC S9(004) BINARY VALUE -2.
           05 K-VZUST-LEER               PIC S9(004) BINARY VALUE -1.
      *
      ********  ENDE COPY-MEMBER DSKI0363  ***********
