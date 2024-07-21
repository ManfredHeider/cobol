      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM PMKI0125              *
      *        - EINGABE DTA VERTRAGSDATENAENDERUNGEN TEILSCHICHT AML -*
      *                                                                *
      *    SATZLÄNGE 150                                               *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *04.08.97!NEUERSTELLUNG                           !A.CECCONI! 01 *
      *KI-NEU  !                                        !         !    *
      *03.10.98!Änderung Vorsatz-text in Text Änderungs-!S.Jost   ! 02 *
      *KI-NEU  !bestand und Text Komplettbestand        !         !    *
      *--------+----------------------------------------+---------+----*
150905*13.05.05! AKI00483 neues Feld                    !P. Pohl  !    *
      *--------!----------------------------------------!--------!-----*
010310*01.03.10! Neues Feld TTYP-BEITR und Feldnamen    ! HSE     ! 04 *
      *AKI00920! RATEN-BEITRAG in SCHICHT-BEITR geae.   !         !    *
      *21.03.10! LRECL auf 150 angepasst                !         !    *
      *--------!----------------------------------------!---------!----*
070610*07.06.10! Neues Feld: DEPZINSUEBANT              !HSE      !  5 *
      *AKI00969! Filler in HS und NS eingefuegt         !         !    *
      *--------!----------------------------------------!---------!----*
020614*02.06.14! NEUE FELDER: RENTENSCHLUESSEL UND      !HSE      ! 06 *
      *AKI01296! RENTENSCHLUESSELDATUM                  !         !    *
      *--------!----------------------------------------!---------!----*
180731*31.07.18! NEUES FELD: STAND-VERTR                !DETMAR   ! 10 *
      *AKI01618! RENTENSCHLUESSELDATUM                  !         ! 13 *
      *--------!----------------------------------------!---------!----*
251018*25.10.18! Korrektur in AML-Satz                  !DETMAR   ! 14 *
251018*        !                                        !         ! 15 *
      *--------!----------------------------------------!---------!----*
      *13.02.20! AKI01757 ÄNDERUNG AML-RT 03/2020       !T13AC20  !2.16*
      *        !   NEUES FELD: BTGRUEKZNPFL             !         !    *
      *--------!----------------------------------------!---------!----*
      ****************************************************************
      * AENDERUNGEN:                                                  
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-9444   !16.11.21! NEUE FELDER:           :     ! T13AC20  
      *            !        !    KLB-ERH-AUSSCHL           !          
      * -----------!--------!------------------------------!----------
      * DBO-9994   !03.05.22! Ausbau Feld STAND-VERTR      ! U131381  
      *            !        !                              !          
      * -----------!--------!------------------------------!----------
      * DBO-11133  !20.04.23! NEUE FELDER:                 ! T13AC20  
      *            !        !    RABSTUFE                  !          
      * -----------!--------!------------------------------!----------
      *            !        !                              !          
      *            !        !                              !          
      *                                                               
      * ENDE-LOGBUCH                                                  
      ****************************************************************
       01  DRKI0015.
      *
      ***********************************************************
      *                                                         *
      *         HAUPTSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  AML-SATZ.
      *
               20  AML-SA                        PIC  X(001).
      *            SATZART        = 'C'
               20  AML-VNR                       PIC  9(010).
      *            VERTRAGSNR. (ERSTE 5 STELLEN = 0)
               20  AML-TEILTYP                   PIC  9(002).
      *            LV-TEILTYP-NUMMER
               20  AML-TEILSCHICHT               PIC  9(003).
      *            SCHICHTNUMMER
               20  AML-SCHICHTDATEN.
                   30  AML-VBEGINN               PIC  X(008).
      *                TECHNISCHER BEGINN VERTRAG/VERTRAGSTEIL
                   30  AML-VABLAUF               PIC  X(008).
      *                TECHNISCHER ABLAUF VERTRAG/VERTRAGSTEIL
                   30  AML-BZUST                 PIC  9(002).
      *                BEITRAGSZUSTAND/BEITRAGSSCHLÜSSEL
                   30  AML-VZUST                 PIC  9(004).
      *                SCHLÜSSEL VERTRAGSZUSTAND SCHICHT/VERTRAG
                   30  AML-VZUST-DAT             PIC  X(008).
      *                DATUM VERTRAGSZUSTAND SCHICHT/VERTRAG
                   30  AML-DEZIMAL1.
                       40  AML-VERS-SUMME        PIC  9(011)V99.
      *                    VERSICHERUNGSSUMME/JAHRESRENTE
                   30  AML-BZAHL-DAUER           PIC  X(008).
      *                ABLAUFDATUM BEITRAGSZAHLUNGSDAUER
                   30  AML-DEZIMAL2.
010310                 40  AML-SCHICHT-BEITR     PIC  9(011)V99.
      *                    SCHICHTBEITRAG
                   30  AML-TARIF-BEZ             PIC  X(015).
      *                TARIFBEZEICHNUNG
150905             30  AML-SCHICHTZUGART         PIC  X(001).
               20  AML-SCHICHT-TT-DATEN.
                   30  AML-DEZIMAL3.
010310                 40  AML-TTYP-BEITR        PIC  9(011)V99.
      *                    Raten-BEITRAG pro Teiltyp
070610         20  AML-DEPZINSUEBANT             PIC  9(003)V99.
070610*            Depot-Zinssatz für das einjährige Parkdepot
070610*            nach Tarif PDP
020614         20  AML-RNTSCHL                   PIC  9(001).
020614         20  AML-RNTSCHLDAT                PIC  9(008).
020614         20  AML-AKTUAL-KZ                 PIC  X(001).
      *            ÄNDERUNGS-KZ
               20  AML-BTGRUEKZNPFL              PIC  X(001).
      *            Kennzeichen Beitragsrückgewähr bei Tod
               20  AML-KLB-ERH-AUSSCHL           PIC  X(001).
      *            KLB-Erhöhungsausschluss
               20  AML-RABSTUFE                  PIC  X(003).
      *            Rabattstufe
               20  AML-RESERVE                   PIC  X(021).
      *            RESERVE
      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************
      *
           10  AML-VORSATZ     REDEFINES      AML-SATZ.
      *
               20  AML-V-SA                      PIC X(001).
      *            SATZART = 'A'
               20  AML-V-ABSENDER                PIC X(003).
      *            ABSENDER = 'AML'
               20  AML-V-DATUM                   PIC 9(008).
      *            ERFASSUNGS-/AENDERUNGSDATUM = 'JJJJMMTT'
               20  AML-V-EMPFAENGER              PIC X(004).
      *            EMPFAENGER = 'DVAG'
031098         20  AML-V-TEXT                    PIC X(020).
      *            ÄNDERUNGSBESTAND = 'VERTRAG SCHICHT'
      *            KOMPLETTBESTAND  = 'VERTRAG SCHICHT INIT'
220503         20  FILLER                        PIC X(114).
      *            RESERVE
      ***********************************************************
      *                                                         *
      *          NACHSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  AML-NACHSATZ    REDEFINES      AML-SATZ.
      *
               20  AML-N-SA                      PIC X(001).
      *            SATZART             'E'
               20  AML-N-ANZ                     PIC 9(009).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
010310         20  FILLER                        PIC X(140).
      *            RESERVE