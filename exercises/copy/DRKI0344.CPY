      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZUR FORMALEN AUFBEREITUNG LAUFENDE AKTUALISIER- *
      *    UNG  AML                                                    *
      *        - EINGABE DTA VERTRAGSDATENAENDERUNGEN TEILSCHICHT AML -*
      *    KSDS-DATEI                                                  *
      *    SATZLÄNGE 150                                               *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *17.08.98!NEUERSTELLUNG                           !S.JOST   ! 01 *
      *AKI00001!                                        !         !    *
      *        !                                        !         !    *
200905*20.09.05!Erweiterung für Schichtzugangsart       !P. Pohl  ! 02 *
      *AKI00483!                                        !         !    *
      *--------!----------------------------------------!---------!----*
010310*01.03.10! Neues Feld TTYP-BEITR und Feldnamen    ! HSE     ! 02 *
      *AKI00920! RATEN-BEITRAG in SCHICHT-BEITR geae.   !         !    *
      *--------!----------------------------------------!---------!----*
260810*26.08.10! Neues Feld DEPZINSUEBANT               ! HSE     ! 03 *
      *AKI00969! LRECL von 111 auf 150 erhoeht          !         !    *
      *--------!----------------------------------------!---------!----*
020614*02.06.14! Neue Felder: RENTENSCHLUESSEL und      !HSE      ! 04 *
      *AKI01296! RENTENSCHLUESSELDATUM                  !         !    *
      *--------!----------------------------------------!---------!----*
      *25.06.14! Verschieben Aktual-KZ entspr. DTA      !HSe      ! 05 *
      *--------!----------------------------------------!---------!----*
      *13.02.20! AKI01757 ÄNDERUNG AML-RT 03/2020       !T13AC20  !2.8 *
      *        !   NEUES FELD: BTGRUEKZNPFL             !         !    *
      *--------!----------------------------------------!---------!----*
      ****************************************************************
      * AENDERUNGEN:                                                  
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-9444   !16.11.21! NEUE FELDER:                 ! T13AC20  
      *            !        !    KLB-ERH-AUSSCHL           !          
      * -----------!--------!------------------------------!----------
      * DBO-11133  !20.04.23! NEUE FELDER:                 ! T13AC20  
      *            !        !    RABSTUFE                  !          
      * -----------!--------!------------------------------!----------
      *            !        !                              !          
      *            !        !                              !          
      *                                                               
      * ENDE-LOGBUCH                                                  
      ****************************************************************
      *
       01  AML-KOMPLETT-SATZ.
               05  KOMPL-AML-SA                  PIC  X(001).
      *            SATZART        = 'C'

               05  KOMPL-AML-KEY.
                 10  KOMPL-AML-VNR               PIC  9(010).
      *              VERTRAGSNR. (ERSTE 5 STELLEN = 0)
                 10  KOMPL-AML-TEILTYP           PIC  9(002).
      *              LV-TEILTYP-NUMMER
                 10  KOMPL-AML-TEILSCHICHT       PIC  9(003).
      *              SCHICHTNUMMER

               05  KOMPL-AML-SCHICHTDATEN.
                   10  KOMPL-AML-VBEGINN         PIC  X(008).
      *                TECHNISCHER BEGINN VERTRAG/VERTRAGSTEIL
                   10  KOMPL-AML-VABLAUF         PIC  X(008).
      *                TECHNISCHER ABLAUF VERTRAG/VERTRAGSTEIL
                   10  KOMPL-AML-BZUST           PIC  9(002).
      *                BEITRAGSZUSTAND/BEITRAGSSCHLÜSSEL
                   10  KOMPL-AML-VZUST           PIC  9(004).
      *                SCHLÜSSEL VERTRAGSZUSTAND SCHICHT/VERTRAG
                   10  KOMPL-AML-VZUST-DAT       PIC  X(008).
      *                DATUM VERTRAGSZUSTAND SCHICHT/VERTRAG
                   10  KOMPL-AML-DEZIMAL1.
                       40  KOMPL-AML-VERS-SUMME  PIC  9(011)V99.
      *                    VERSICHERUNGSSUMME/JAHRESRENTE
                   10  KOMPL-AML-BZAHL-DAUER     PIC  X(008).
      *                ABLAUFDATUM BEITRAGSZAHLUNGSDAUER
                   10  KOMPL-AML-DEZIMAL2.
010310                 40  KOMPL-AML-SCHICHT-BEITR 
                                                 PIC  9(011)V99.
      *                    SCHICHTBEITRAG
                   10  KOMPL-AML-TARIF-BEZ       PIC  X(015).
      *                TARIFBEZEICHNUNG
200905             10  KOMPL-SCHICHTZUGART       PIC  X(001).
200905*                Schichtzugangsart
010310         05  KOMPL-AML-TTYPDATEN.
010310             10  KOMPL-AML-DEZIMAL2.
010310                 40  KOMPL-AML-TTYP-BEITR  PIC  9(011)V99.
      *                    Raten-BEITRAG pro Teiltyp
260810         05  KOMPL-AML-SCHICHTDATEN2.
260810             10  KOMPL-AML-DEPZINSUEBANT   PIC  9(003)V99.
      *                Zinssatz fuer Parkdepot
020614         05  KOMPL-AML-RENTENSCHLUESSEL.
                   10  KOMPL-AML-RNTSCHL         PIC  9(001).
                   10  KOMPL-AML-RNTSCHLDAT      PIC  9(008).
      *                Rentenschluessel und -Datum
250614         05  KOMPL-AML-AKTUAL-KZ           PIC  X(001).
      *            ÄNDERUNGSKENNZEICHEN
               05  KOMPL-AML-BTGRUEKZNPFL        PIC  X(001).
      *            Kennzeichen Beitragsrückgewähr bei Tod
               05  KOMPL-AML-KLB-ERH-AUSSCHL     PIC  X(001).
      *            KLB-Erhöhungsausschluss
               05  KOMPL-AML-RABSTUFE            PIC  X(003).
      *            Rabattstufe
               05  KOMPL-AML-RESERVE             PIC  X(021).
      *            RESERVE