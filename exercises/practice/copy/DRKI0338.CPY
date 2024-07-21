      ******************************************************************
      *                                                                 
      *    COPY-MEMBER ZUR FORMALEN AUFBEREITUNG LAUFENDE AKTUALISIER-  
      *    UNG AML                                                      
      *        - EINGABE DTA ALLGEMEINE VERTRAGSDATEN AML -             
      *    KSDS-DATEI
      *
      *    SATZLÄNGE 349 Byte geändert
      *
      *    Achtung!!!
      *    Die Satzlänge der KSDS ist 9 Byte größer als die Satzlänge
      *    des DTAs.
      *    Grund: CoC-Attribute am Schluss
      *    Diese Vergrößerung muss bei allen zukünftigen Änderungen
      *    mit beachtet werden.
      *                                                                 
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
      *17.08.98!NEUERSTELLUNG                           !S.JOST   ! 01 *
      *AKI00001!                                        !         !    *
      *30.08.99!ÄNDERUNG                                !S.JOST   ! 02 *
      *AKI00044!EURO UMSTELLUNG  WÄHRUNGSKENNZEICHEN    !         !    *
      *        !PIC X(003) EINGEFÜGT                    !         !    *
      *        !                                        !         !    *
      *11.04.03! AKI00337 neue Felder BAV               !P. Hübner!    *
      *        !                                        !         !    *
130505*13.05.05! AKI00482 neues Feld BTGVERF            !P. Pohl  !    *
      *20.02.09! AKI00798 Erweiterung auf 10 Fonds      !I.Frenzel!2.7 *
      * AKI00860!19.08.09!2.8! Erweiterung um Abtretungs-   !I. Frenzel*
      *         !        !    ! und Rentenbezugs-KZ         !          *
      * AKI00918!17.02.10!2.9 ! Erweiterung um              !I. Frenzel*
      *         !        !    ! BU-Bezugs-KZ                !          *
      * AKI00909!02.03.11!2.11! Erweiterung um Rechnungs-   !I. Frenzel*
      *         !        !    ! zins                        !          *
      * AKI01065!10.08.11!2.12! Rechnungszins entfernt      !I. Frenzel*
      *         !        !    ! Bonus-KZ und Anteile der    !          *
      *         !        !    ! konventionellen Anlage      !          *
      *         !        !    ! eingefügt                   !          *
      *         !03.01.13!2.13! SEPA Bankverbindung(alt)    !I. Frenzel*
      *         !        !    ! entfernt                    !          *
      *---------!--------!----!-----------------------------!----------*
250716* AKI01455!25.07.16!2.13! Produktbezeichnung fuer     ! HSE      *
      *         !        !    ! PORTOS aufgenommen          !          *
      *         !        !    ! Satzlaenge von 150 auf 200  !          *
      *---------!--------!----!-----------------------------!----------*
300816* AKI01467!30.08.16!2.14! CoC-Felder                  ! SWZ      *
      *         !        !    !                             !          *
      *         !        !    !                             !          *
      *---------!--------!----!-----------------------------!----------*
010916* AKI01462!01.09.16!2.15! Korrespondnz-Makler-KZ      ! HSE      *
      *         !        !    ! aufgenommen. Ist nur bei    !          *
      *         !        !    ! DELETE-Satz gesetzt.        !          *
      *---------!--------!----!-----------------------------!----------*
140217* AKI01492!14.02.17!2.16! Neue Attribute zu RT 03/17  ! IFR      *
      *---------!--------!----!-----------------------------!----------*
090817* AKI01538!09.08.17!2.17! Neue Attribute zu RT 09/17  ! MBS      *
      *---------!--------!----!-----------------------------!----------*
      * AKI01706!10.09.19!2.19! neue Felder aufgenommen:    ! SWO      *
      *         !        !    !    NEUABSCHLKZN             !          *
      *         !        !    !    BASISZINS-BETR           !          *
      *         !        !    !    ZINSBOOST-SATZ           !          *
      *         !        !    !    ZINSBOOST-BETR           !          *
      *---------!--------!----!-----------------------------!----------*
      * AKI01782!14.05.20!2.20! neue Felder aufgenommen:    ! T13AC20  *
      *         !        !    !    EU-KLAUSEL-KZN           !          *
      *---------!--------!----!-----------------------------!----------*
      * AKI01825!03.02.21!2.21! neue Felder aufgenommen:    ! T13AC20  *
      *         !        !    !    SCHUELER-BU              !          *
      *         !        !    ! Feld durch FILLER erstezt:  !          *
      *         !        !    !    BERUFSGRP-KZN            !          *
      *---------!--------!----!-----------------------------!----------*
      ****************************************************************
      * AENDERUNGEN:                                                  
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-9070   !16.08.21! neue Felder aufgenommen:     ! T13AC20  
      *            !        !    BTG-AG                    !          
      *            !        !    BTG-AN                    !          
      * -----------!--------!------------------------------!----------
      * DBO-9444   !25.11.21! Ausprägung ZINSBOOST-SATZ    ! T13AC20  
      *            !        ! geändert                     !          
      * -----------!--------!------------------------------!----------
      * DBO-10780  !23.01.23! neue Felder aufgenommen:     ! T13AC20  
      *            !        !    WERTSICHOPTKZN            !          
      *            !        !    FILLER                    !          
      *            !        ! Felder vergrößert:           !          
      *            !        !    BTG-AG                    !          
      *            !        !    BTG-AN                    !          
      * -----------!--------!------------------------------!----------
      *            !        !                              !          
      *            !        !                              !          
      *                                                               
      * ENDE-LOGBUCH                                                  
      ****************************************************************
      *
 
      *
       01  AML-KOMPLETT.
140217     02  AML-KOMPLETT-SATZ.
               05  KOMPL-AML-SA                  PIC X(001).
      *            SATZART        = 'C'
               05  KOMPL-AML-KEY.
                 10 KOMPL-AML-VNR                PIC 9(010).
      *             VERTRAGSNR. (ERSTE 5 STELLEN = 0)
 
               05  KOMPL-AML-RUMS                PIC X(003).
      *            KUNDENBETREUUNGSGRUPPE
               05  KOMPL-AML-VBNR                PIC X(007).
      *            ABSCHLUSSVERMITTLER
               05  KOMPL-AML-ZAHLWEISE           PIC 9(002).
      *            ZAHLUNGSWEISE
               05  KOMPL-AML-ZAHLART             PIC 9(002).
      *            INKASSO ANFORDERUNGSART
               05  KOMPL-AML-LV-ART              PIC 9(002).
      *            ART DER LEBENSVERSICHERUNG
               05  KOMPL-AML-DIREKT-VERS-KZ      PIC 9(002).
      *            KENNZEICHEN DIREKTVERSICHERUNG
110403         05  KOMPL-AML-BAV-FINART          PIC 9(002).
110403*            KZ TEXT DIREKTVERSICHERUNG
110403         05  KOMPL-AML-BAV-DURCHFWEG       PIC 9(002).
110403*            KZ 2 TEXT DIREKTVERSICHERUNG
               05  KOMPL-AML-RENTE-ZAHLW         PIC 9(002).
      *            LEISTUNGSZAHLUNGSWEISE FÜR TEILTYP 01/ SCHICHTNR. 00
               05  KOMPL-AML-RENTE-GARANT        PIC X(008).
      *            ABLAUF DER RENTENGARANTIEZEIT
      *            FÜR TEILTYP 01/ SCHICHTNR. 00
               05  KOMPL-AML-KAP-RECHT-KZ        PIC X(001).
      *            ABWEICHUNG KAPITALABFINDUNG
      *            J = KAPITALRECHT AUSGESCHLOSSEN
               05  KOMPL-AML-AKTUAL-KZ           PIC X(001).
      *            ÄNDERUNGSKENNZEICHEN
300899         05  KOMPL-AML-WAEHRUNG-KZ         PIC X(003).
300899*            WÄHRUNGSKENNZ. MOMENTAN GÜLTIG DEM(DEUTSCHE MARK)
      *            UND EUR(EURO)
130505         05  KOMPL-AML-BTGVERF             PIC X(001).
130505*            Beitragsverfahren
100408*        05  KOMPL-AML-FLVFO   OCCURS 4 TIMES   PICTURE X(02).
200209         05  KOMPL-AML-FLVFO               OCCURS 10 TIMES
                                                 PIC  X(002).
>>    *            Fondbezeichnung(Kürzel)
               05  KOMPL-AML-FONDSPALETTE        PIC  X(008).
      *            Fondspalette
               05  KOMPL-AML-FONDSAKTKZN         PIC  X(001).
      *            Fondswechsel-Kennzeichen
               05  KOMPL-AML-DEPBETR             PIC  9(011)V99.
      *            Depotbetrag
               05  KOMPL-AML-ZSBISWTGJ           PIC  9(011)V99.
      *            aufgelaufene Zinsen
               05  KOMPL-AML-DEPZSSATZ           PIC  9(003)V99.
      *            Depot-Zinssatz
               05  KOMPL-AML-DEPWT               PIC  X(008).
      *            Depot-Wertstellungstermin
    <<         05  KOMPL-AML-DAUZULANTRKZN       PIC  X(003).
100408*            Dauerzulagenantragskennzeichen für Riester
190809         05  KOMPL-AML-ABTRETUNGSKZN       PIC  X(001).
190809*            Abtretungskennzeichen
190809         05  KOMPL-AML-RENTENBEZUGSKZN     PIC  9(001).
190809*            Rentenbezugskennzeichen
170210         05  KOMPL-AML-BUKZN               PIC  X(001).
170210*            BU-Bezugskennzeichen
020311*        05  KOMPL-AML-RZINS               PIC  9(003)V99.
020311*            Rechnungszins
100811         05  KOMPL-AML-BONUSKZN            PIC  X(001).
100811*            Bonuskennzeichen
100811         05  KOMPL-AML-KONVANLPROZ         PIC  9(003).
100811*            Anteil der konventionellen Anlage in Prozent
250716         05  KOMPL-AML-PRODBEZEICH-SCHL    PIC  9(004).
>>    *            Numerischer Schluessel für Produktbezeichnung
    <<         05  KOMPL-AML-PRODBEZEICH-TEXT    PIC  X(040).
250716*            Produktbezeichnung in Textform
010916         05  KOMPL-AML-KORR-MAKLER-KZ      PIC  X(001).
010916*            J, wenn Abgang aufgrund von Korr.-Makler-Vollmacht
140217         05  KOMPL-AML-URSPRPOLDAT         PIC  X(008).
>>    *            Ursprüngliches Policierungsdatum
               05  KOMPL-AML-LVURSPRBEG          PIC  X(008).
      *            Ursprünglicher Vertragsbeginn
               05  KOMPL-AML-FOMGNTART           PIC  X(001).
      *            Kennzeichen für Fondsmanagement
               05  KOMPL-AML-FOMGNTKWAB          PIC  X(008).
      *            Fondsmanagement Kundenwunsch ab
               05  KOMPL-AML-FOMGNTKWBIS         PIC  X(008).
      *            Fondsmanagement Kundenwunsch bis
090817         05  KOMPL-AML-BU-KT-KZN           PIC  X(001).
>>    *            Kennzeichen BU kombiniert mit Krankentage.
               05  KOMPL-AML-VITALITY-KZN        PIC  9(003).
      *            Kennzeichen BU kombiniert mit Vitality
      *        05  KOMPL-AML-BERUFSGRP-KZN       PIC  9(002).
               05  FILLER                        PIC  X(002).
    <<*            Berufsgruppenkennzeichen
               05  KOMPL-AML-NEUABSCHLKZN        PIC  X(001).
      *            Kennzeichen für Neuabschluss
               05  KOMPL-AML-BASISZINS-BETR-ALPHA
                                                 PIC  X(014).
               05  KOMPL-AML-BASISZINS-BETR REDEFINES
                   KOMPL-AML-BASISZINS-BETR-ALPHA
                                                 PIC  9(012)V99.
      *            Betrag zum Basiszins
               05  KOMPL-AML-ZINSBOOST-SATZ-ALPHA
                                                 PIC  X(008).
               05  KOMPL-AML-ZINSBOOST-SATZ REDEFINES
                   KOMPL-AML-ZINSBOOST-SATZ-ALPHA
                                                 PIC  9(005)V999.
      *            Zinssatz des Boosters
               05  KOMPL-AML-ZINSBOOST-BETR-ALPHA
                                                 PIC  X(014).
               05  KOMPL-AML-ZINSBOOST-BETR REDEFINES
                   KOMPL-AML-ZINSBOOST-BETR-ALPHA
                                                 PIC  9(012)V99.
      *            Betrag zum Boosterzins
               05  KOMPL-AML-EU-KLAUSEL-KZN      PIC  X(001).
      *            Kennzeichen für EU-Klausel
               05  KOMPL-AML-SCHUELER-BU         PIC  X(001).
      *            Kennzeichen für Schüler-BU
               05  KOMPL-AML-BTG-AG              PIC  9(011)V99.
      *            Tarifbeitrag Arbeitgeber-Anteil
               05  KOMPL-AML-BTG-AN              PIC  9(011)V99.
      *            Tarifbeitrag Arbeitnehmer-Anteil
               05  KOMPL-AML-WERTSICHOPTKZN      PIC  X(001).
      *            Wertsicherungsoptionskennzeichen
               05  KOMPL-AML-RESERVE             PIC  X(063).
      *            RESERVE
140214 02  AML-KOMPLETT-COC.
300816         05  KOMPL-AML-COC-SPERRDATUM      PIC  X(008).
300816*            CoC Sperrdatum JHJJMMTT oder Space
300816         05  KOMPL-AML-COC-SPERRAKTION     PIC  X(001).
300816*            Sperraktion S (Sperrung) E (Entsperrung) L (Löschung)