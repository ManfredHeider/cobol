      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZUM DTA ALLGEMEINE VERTRAGSDATEN AML            *
      *                                                                *
      *    SATZLÄNGE 340                                               *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *04.08.97!NEUERSTELLUNG                           !A.CECCONI! 01 *
      *KI-NEU  !                                        !         !    *
      *03.10.98!ÄNDERUNG VORSATZ-TEXT IN TEXT ÄNDERUNGS-!S.JOST   ! 02 *
      *KI-NEU  !BESTAND UND TEXT KOMPLETTBESTAND        !         !    *
      *        !                                        !         !    *
      *17.08.99!HINZUFÜGEN WÄHRUNGSKENNZEICHEN  PIC X(3)!S.Jost   ! 03 *
      *KI-NEU  !ZUR ZEIT GÜLTIG (EUR=EURO DEM = DEUTSCHE          !    *
      *        !MARK)                                   !         !    *
      *11.04.03! AKI00337 neue Felder BAV               !P. Hübner!    *
      *--------+----------------------------------------+---------+----*
130505*13.05.05! AKI00483 neues Feld BTGVERF            !P. Pohl  !    *
130505*        !          (Beitragsverfahren)           !         !    *
      *        !                                        !         !    *
      *--------+----------------------------------------+---------+----*
      *10.04.08! AKI00711 neue Felder                   !I.Frenzel!2.5 *
      *--------+----------------------------------------+---------+----*
      *20.02.09! Erweiterung auf 10 Fonds               !I.Frenzel!2.6 *
      * AKI00860!19.08.09!2.7 ! Erweiterung um Abtretungs- I. Frenzel*
      *         !        !    ! und Rentenbezugs-KZ       !          *
      * AKI00918!17.02.10!2.8 ! Erweiterung um            !I. Frenzel*
      *         !        !    ! BU-Bezugs-KZ              !          *
      * AKI00909!02.03.11!2.10! Erweiterung um Rechnungs- !I. Frenzel*
      *         !        !    ! zins                      !          *
      * AKI01065!10.08.11!2.11! Rechnungszins entfernt    !I. Frenzel*
      *         !        !    ! Bonus-KZ und Anteile der  !          *
      *         !        !    ! konventionellen Anlage    !          *
      *         !        !    ! eingefügt                 !          *
      *         !03.01.13!2.12! SEPA Bankverbindung(alt)  !I. Frenzel*
      *         !        !    ! entfernt                  !          *
      *---------!--------!----!---------------------------!----------*
250716* AKI01455!25.07.16!2.13! Produktbezeichnung fuer   ! HSE      *
      *         !        !    ! PORTOS aufgenommen        !          *
      *         !        !    ! FILLER in HAUPTSATZ eingef!          *
      *---------!--------!----!---------------------------!----------*
010916* AKI01462!01.09.16!2.14! Korrespondnz-Makler-KZ    ! HSE      *
      *         !        !    ! aufgenommen. Ist nur bei  !          *
      *         !        !    ! DELETE-Satz gesetzt.      !          *
      *         !        !    ! CoC-Sperrdaten in Struktur!          *
      *         !        !    ! aufgenommen wg. KSDS      !          *
      *---------!--------!----!---------------------------!----------*
150217* AKI01492!15.02.17!2.15! neue Felder aufgenommen:  ! SWO      *
      *         !        !    !    URSPRPOLDAT            !          *
      *         !        !    !    LVURSPRBEG             !          *
      *         !        !    !    FOMGNTART              !          *
      *         !        !    !    FOMGNTKWAB             !          *
      *         !        !    !    FOMGNTKWBIS            !          *
      *         !        !    ! CoC-Sperrdaten wieder     !          *
      *         !        !    ! rausgenommen              !          *
      *         !        !    ! Hauptsatz vor Vorsatz     !          *
      *         !        !    ! gestellt                  !          *
      *---------!--------!----!---------------------------!----------*
170803* AKI01492!15.02.17!2.16! NEUE FELDER AUFGENOMMEN:  ! MBS      *
      *         !        !    !    BU-KT-KZN              !          *
      *         !        !    !    VITALITY-KZN           !          *
      *         !        !    !    BUZBERUFGRP            !          *
      *---------!--------!----!---------------------------!----------*
180731* AKI01618!31.07.18!2.17! NEUES FELD AUFGENOMMEN:   ! DETMAR   *
      *         !        !2.23!    STAND-VERTR            !          *
      *---------!--------!----!---------------------------!----------*
270818* AKI01618!27.08.18!2.24! ÄNDERN DES FELD AML-V-TEXT! DETMAR   *
      *         !        !2.25! SATZLÄNGE SEIT ANBEGINN   !          *
      *         !        !    ! 23, STATT 20              !          *
      *---------!--------!----!---------------------------!----------*
      * AKI01706!10.09.19!2.26! neue Felder aufgenommen:  ! SWO      *
      *         !        !    !    NEUABSCHLKZN           !          *
      *         !        !    !    BASISZINS-BETR         !          *
      *         !        !    !    ZINSBOOST-SATZ         !          *
      *---------!--------!----!---------------------------!----------*
      * AKI01782!14.05.20!2.27! neue Felder aufgenommen:  ! T13AC20  *
      *         !        !    !    EU-KLAUSEL-KZN         !          *
      *---------!--------!----!---------------------------!----------*
      * AKI01825!03.02.21!2.28! neue Felder aufgenommen:  ! T13AC20  *
      *         !        !    !    SCHUELER-BU            !          *
      *         !        !    ! Feld durch FILLER erstezt:!          *
      *         !        !    !    BERUFSGRP-KZN          !          *
      ****************************************************************
      * AENDERUNGEN:                                                  
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-9070   !16.08.21! neue Felder aufgenommen:     ! T13AC20  
      *            !        !    BTG-AG                    !          
      *            !        !    BTG-AN                    !          
      * -----------!--------!------------------------------!----------
      * DBO-9444   !24.11.21! Ausprägung ZINSBOOST-SATZ    ! T13AC20  
      *            !        ! geändert                     !          
      * -----------!--------!------------------------------!----------
      * DBO-9994   !03.05.22! Ausbau Feld STAND-VERT       ! U131381  
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
      *SUB-THEMA  LFD. AKT.                                          *
      ****************************************************************
       01  DRKI0013.
 
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
030113*        20  AML-BLZ                       PIC  9(008).
030113*            BANKLEITZAHL
030113*        20  AML-KTO-NR                    PIC  9(010).
030113*            KONTONUMMER
               20  AML-RUMS                      PIC  X(003).
      *            KUNDENBETREUUNGSGRUPPE
               20  AML-VBNR                      PIC  X(007).
      *            ABSCHLUSSVERMITTLER
               20  AML-ZAHLWEISE                 PIC  9(002).
      *            ZAHLUNGSWEISE
               20  AML-ZAHLART                   PIC  9(002).
      *            INKASSO ANFORDERUNGSART
               20  AML-LV-ART                    PIC  9(002).
      *            ART DER LEBENSVERSICHERUNG
               20  AML-DIREKT-VERS-KZ            PIC  9(002).
      *            KENNZEICHEN DIREKTVERSICHERUNG
110403         20  AML-BAV-FINART                PIC  9(002).
110403*            KZ TEXT DIREKTVERSICHERUNG
110403         20  AML-BAV-DURCHFWEG             PIC  9(002).
110403*            KZ 2 TEXT DIREKTVERSICHERUNG
               20  AML-RENTE-ZAHLW               PIC  9(002).
      *            LEISTUNGSZAHLUNGSWEISE FÜR TEILTYP 01/ SCHICHTNR. 00
               20  AML-RENTE-GARANT              PIC  X(008).
      *            ABLAUF DER RENTENGARANTIEZEIT
      *            FÜR TEILTYP 01/ SCHICHTNR. 00
               20  AML-KAP-RECHT-KZ              PIC  X(001).
      *            ABWEICHUNG KAPITALABFINDUNG
      *            J = KAPITALRECHT AUSGESCHLOSSEN
               20  AML-AKTUAL-KZ                 PIC  X(001).
      *            ÄNDERUNGSKENNZEICHEN
170899         20  AML-WAEHRUNG-KZ               PIC  X(003).
170899*            WÄHRUNGSKENNZEICHEN
130505         20  AML-BTGVERF                   PIC  X(001).
130505*            Kennzeichen Riesterzulagenart
100408*        20  AML-FLVFO   OCCURS 4 TIMES   PICTURE X(02).
200209         20  AML-FLVFO                     OCCURS 10 TIMES
                                                 PIC  X(002).
>>    *            Fondbezeichnung(Kürzel)
               20  AML-FONDSPALETTE              PIC  X(008).
      *            Fondspalette
               20  AML-FONDSAKTKZN               PIC  X(001).
      *            Fondswechsel-Kennzeichen
               20  AML-DEPBETR                   PIC  9(011)V99.
      *            Depotbetrag
               20  AML-ZSBISWTGJ                 PIC  9(011)V99.
      *            aufgelaufene Zinsen
               20  AML-DEPZSSATZ                 PIC  9(003)V99.
      *            Depot-Zinssatz
               20  AML-DEPWT                     PIC  9(008).
      *            Depot-Wertstellungstermin
    <<         20  AML-DAUZULANTRKZN             PIC  X(003).
100408*            Dauerzulagenantragskennzeichen für Riester
190809         20  AML-ABTRETUNGSKZN             PIC  X(001).
190809*            Abtretungskennzeichen
190809         20  AML-RENTENBEZUGSKZN           PIC  9(001).
190809*            Rentenbezugskennzeichen
170210         20  AML-BUKZN                     PIC  X(001).
170210*            BU-Bezugskennzeichen
100811*        20  AML-RZINS                     PIC  9(003)V99.
020311*            Rechnungszins
100811         20  AML-BONUSKZN                  PIC  X(001).
100811*            Bonuskennzeichen
100811         20  AML-KONVANLPROZ               PIC  9(003).
100811*            Anteil der konventionellen Anlage in Prozent
250716         20  AML-PRODBEZEICH-SCHL          PIC  9(004).
>>    *            Numerischer Schluessel für Produktbezeichnung
    <<         20  AML-PRODBEZEICH-TEXT          PIC  X(040).
250716*            Produktbezeichnung in Textform
010916         20  AML-KORR-MAKLER-KZ            PIC  X(001).
010916*            J, wenn DELETE aufgrund Korr.-Makler-Vollmacht
150217         20  AML-URSPRPOLDAT               PIC  X(008).
>>    *            Ursprüngliches Policendatum
               20  AML-LVURSPRBEG                PIC  X(008).
      *            Ursprünglicher Vertragsbeginn
               20  AML-FOMGNTART                 PIC  X(001).
      *            Kennzeichen für Fondsmanagement
               20  AML-FOMGNTKWAB                PIC  X(008).
      *            Fondsmanagement Kundenwunsch ab
    <<         20  AML-FOMGNTKWBIS               PIC  X(008).
150217*            Fondsmanagement Kundenwunsch bis
               20  AML-BU-KT-KZN                 PIC  X(001).
      *            Kennzeichen Berufunfäh. kombiniert mit Krankentage.
               20  AML-VITALITY-KZN              PIC  9(003).
      *            Kennzeichen Berufunfäh. kombiniert mit Vitality
      *        20  AML-BERUFSGRP-KZN             PIC  9(002).
               20  FILLER                        PIC  X(002).
      *            Kennzeichen Berufsgruppe
               20  AML-NEUABSCHLKZN              PIC  X(001).
      *            Kennzeichen für Neuabschluss
               20  AML-BASISZINS-BETR-ALPHA      PIC  X(014).
               20  AML-BASISZINS-BETR REDEFINES AML-BASISZINS-BETR-ALPHA
                                                 PIC  9(012)V99.
      *            Betrag zum Basiszins
               20  AML-ZINSBOOST-SATZ-ALPHA      PIC  X(008).
               20  AML-ZINSBOOST-SATZ REDEFINES AML-ZINSBOOST-SATZ-ALPHA
                                                 PIC  9(005)V999.
      *            Zinssatz des Boosters
               20  AML-ZINSBOOST-BETR-ALPHA      PIC  X(014).
               20  AML-ZINSBOOST-BETR REDEFINES AML-ZINSBOOST-BETR-ALPHA
                                                 PIC  9(012)V99.
      *            Betrag zum Boosterzins
               20  AML-EU-KLAUSEL-KZN            PIC  X(001).
      *            Kennzeichen für EU-Klausel
               20  AML-SCHUELER-BU               PIC  X(001).
      *            Kennzeichen für Schüler-BU
               20  AML-BTG-AG                    PIC  9(011)V99.
      *            Tarifbeitrag Arbeitgeber-Anteil
               20  AML-BTG-AN                    PIC  9(011)V99.
      *            Tarifbeitrag Arbeitnehmer-Anteil
               20  AML-WERTSICHOPTKZN            PIC  X(001).
      *            Wertsicherungsoptionskennzeichen
               20  FILLER                        PIC  X(063).
      *            Reserve
      *
      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************
      *
           10  AML-VORSATZ  REDEFINES      AML-SATZ.
      *
               20  AML-V-SA                      PIC  X(001).
      *            SATZART = 'A'
               20  AML-V-ABSENDER                PIC  X(003).
      *            ABSENDER = 'AML'
               20  AML-V-DATUM                   PIC  9(008).
      *            ERFASSUNGS-/AENDERUNGSDATUM = 'JJJJMMTT'
               20  AML-V-EMPFAENGER              PIC  X(004).
      *            EMPFAENGER = 'DVAG'
               20  AML-V-TEXT                    PIC  X(020).
      *            ÄNDERUNGSBESTAND = 'VERTRAG ALLGEMEIN'
      *            KOMPLETTBESTAND  = 'VERTRAG ALLG.   INIT'.
               20  FILLER                        PIC  X(304).
      *            RESERVE
      *
      ***********************************************************
      *                                                         *
      *          NACHSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  AML-NACHSATZ    REDEFINES     AML-SATZ.
      *
               20  AML-N-SA                      PIC  X(001).
      *            SATZART             'E'
               20  AML-N-ANZ                     PIC  9(009).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               20  FILLER                        PIC  X(330).
      *            RESERVE