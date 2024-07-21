      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0368                                     *
      *                                                              *
      *    KONSTANTEN FÜR DIE AUSPRÄGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    ZAHLUNGSART - TCKI0368                                    *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!30.06.98!01.01! NEUERSTELLUNG            !V.D.GOLTZ *
      * --------!--------!-----!--------------------------!----------*
      * AKI00001! 1.09.98!01.02! ZAHLUNGSART-UNBEKANNT mit!K-H Müller*
      *         !        !     ! Value '-2' (lt. Protokoll!          *
      *         !        !     ! v. 3.8.98) versehen      !          *
      * --------!--------!-----!--------------------------!----------*
      * AKI00001!11.09.98!01.03! 'nicht geliefert' = 13   !v.d. Goltz*
      *         !        !     ! entfällt.                !          *
      *         !        !     !                          !          *
      *         !10.11.98!01.04! sonstige anforderung     !          *
      *         !        !     ! 11 in 19 geändert        !P.Hübner  *
071217* --------!--------!-----!--------------------------!----------*
071217* AKI01534!07.12.17!2.1  ! LENA: NEUE AUSPRÄGUNGEN  ! SWO      *
071217*         !        !     !  - NICHT ABGESTIMMT      !          *
071217*         !        !     !  - UNBEKANNT             !          *
      ****************************************************************
      * AENDERUNGEN:                                                  
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-9961   !21.04.22! NEUE AUSPRÄGUNG              ! T13AC20  
      *            !        !    - DEPOTABRUF              !          
      * -----------!--------!------------------------------!----------
      *            !        !                              !          
      *            !        !                              !          
      *                                                               
      * ENDE-LOGBUCH                                                  
      ****************************************************************
       01  FILLER.
           05 K-GESCHAEFTST-INKASSO         PIC S9(004) BINARY VALUE 0.
           05 K-MASCH-ZAHLUNGSAUF           PIC S9(004) BINARY VALUE 1.
           05 K-ZENTRALINKASSO              PIC S9(004) BINARY VALUE 2.
           05 K-DAUERAUFTRAG                PIC S9(004) BINARY VALUE 3.
           05 K-SAMMELINKASSO               PIC S9(004) BINARY VALUE 4.
           05 K-LASTSCHRIFT                 PIC S9(004) BINARY VALUE 5.
           05 K-LASTSCHRIFT-MON-NACHTR      PIC S9(004) BINARY VALUE 6.
           05 K-MANUELLE-AUFFORDERUNG       PIC S9(004) BINARY VALUE 7.
           05 K-OHNE-AUFFORDERUNG           PIC S9(004) BINARY VALUE 8.
           05 K-UMBUCHUNG                   PIC S9(004) BINARY VALUE 9.
           05 K-BEITRAGSDEPOT               PIC S9(004) BINARY VALUE 10.
           05 K-DEPOTABRUF                  PIC S9(004) BINARY VALUE 11.
           05 K-SONSTIGE-ANFORDERUNG        PIC S9(004) BINARY VALUE 19.
           05 K-ZAHLUNGSART-UNBEKANNT       PIC S9(004) BINARY VALUE -2.
071217     05 K-ZAHLART-UNBEKANNT           PIC S9(004) BINARY VALUE -1.
071217     05 K-ZAHLART-NICHT-ABGESTIMMT    PIC S9(004) BINARY VALUE -2.
      *
      ********  ENDE COPY-MEMBER DSKI0368  ***********