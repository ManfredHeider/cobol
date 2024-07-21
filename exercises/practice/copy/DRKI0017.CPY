      * T13EX33   Stand: 19/02/99    09:52:54
      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0017.CPY  $
      *    $Revision:   2.20  $
      *
      *    $Log:   L:/mf/CPY/DRKI0017.CPv  $
      *
      *   Rev 2.20   Feb 27 2020 14:10:08   t13ac20
      *AKI01757: VDL-Leben: HOST Verarbeitung und Speicherung der D
      *aten für die neuen Pflegetarife (pflegeSmart und pflegePro) 
      *echenwerte
      *
      *   Rev 2.19   Aug 14 2018 09:34:20   t13ac17
      *AKI01605: G20 EVWS - neue Produkte zum RT 09/2018 Sourcen
      *
      *   Rev 2.18   Mar 13 2017 08:30:38   t13ac17
      *AKI01492: AML-Änderungen zum RT 03/2017-Rechenwerte(PORTOS)
      *
      *   Rev 2.17   Jan 17 2017 10:57:34   t13ex27
      *AKI01488: AM-Leben Konsortialverträge
      *
      *   Rev 2.16   Jun 24 2016 09:03:28   t13ex48
      *AKI01434: AML-Neue Produkte zum RT 06/16: Rechenwerte
      *
      *   Rev 2.15   Mar 19 2015 07:34:00   t13ae31
      *AKI01349: AMLeben: VEP-Bestandsmasken
      *
      *   Rev 2.14   Jun 26 2014 14:03:04   t13ex48
      *AKI01296: VERMÖGENS- & EINKOMMENSPLAN - VEP: Cobol RW
      *
      *   Rev 2.13   Mar 28 2014 01:17:38   t13ex48
      *AKI01153: Neues Verfahren: Aktualisierung AML-Rechenwerte
      *
      *   Rev 2.12   Aug 31 2011 12:39:56   t13ex48
      *AKI01065: AMLeben - Einführung Impulsprodukt - VASP: Source RW
      *
      *   Rev 2.11   Jan 04 2011 13:52:48   t13ex27
      *AKI00961: Anzeige "Frühester Rentenbeginn" in KI-Vertragsdat
      *en-Aktualisierung
      *
      *   Rev 2.10   Dec 10 2010 10:23:36   t13ex48
      *AKI00969: AML-DTA: neue Felder zum RT 06/2010: RW
      *
      *   Rev 2.9   Sep 07 2009 16:14:56   t13ex48
      *AKI00860: CR 22378 - AML-Wunschpolice;LEBENRECHENWERTE
      *
      *   Rev 2.8   Feb 24 2009 14:05:38   t13ex48
      *AKI00798: AML - Erweiterung  auf 10 Fonds: PMKI0127
      *
      *   Rev 2.7   04 Jun 2008 16:03:02   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben Datei-Copys
      *
      *   Rev 2.6   19 Oct 2006 09:05:58   t13ae31
      *AKI00615: Einführung Pflegerentenversicherung
      *
      *   Rev 2.5   Jun 17 2004 14:13:12   t13ex27
      *Integration Kinderprodukt in der Vermögensplanung - Rechenwerte
      *
      *   Rev 2.4   Jul 24 2003 10:24:04   t13ae31
      *FKI00762 GSMTVSR + ZAHLBTG vertauscht
      *
      *   Rev 2.3   Apr 28 2003 14:32:18   t13ae31
      *AKI00348 neues Feld Zahlbeitrag
      *
      *   Rev 2.2   Nov 19 2002 07:10:12   t13ae31
      *AKI313 Lebenfonderw. 3 auf 4
      *
      *   Rev 2.1   14 Jun 2000 11:29:34   T13AE31
      *AKI0105
      *
      *   Rev 2.0   Mar 23 1999 15:53:28   T13EX33
      *aki00019 Produktionsuebergabe
      *
      *   Rev 1.0   29 Sep 1997 11:41:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *        - EINGABE DTA VERTRAGSDATENAENDERUNGEN RECHENWERTE AML -*
      *                                                                *
070610*    SATZLANGE 1000                                              *
260811*    SATZLANGE 2000                                              *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
      *24.07.98!NEUERSTELLUNG                           !HUEBNER  ! 01 *
      *KI-NEU  !                                        !         !    *
      *        !                                        !         !    *
      *03.02.98!FELD FLVFO VON CHAR 1 AUF CHAR 40 ERH÷HT!S.JOST   !1.1 *
      *        !                                        !         !    *
      *06.06.00!FELD TAZRENTE TAZRENTE-INFO EINGEBAUT   !HUEBNER  !    *
      *        !                                        !         !    *
      *15.11.02 ERWEITERUNG LEBENFONDDATEN AKI313       !HUEBNER  !    *
      *        !                                        !         !    *
      *16.04.03!NEUES FELD ZAHLBTG AKI00348             !HUEBNER  !    *
      *        !                                        !         !    *
      *24.07.03!GSMTVSR und ZAHLBTG vertauscht FKI00762 !HUEBNER  !    *
      *        !                                        !         !    *
      *07.06.04!Neues Feld LRWGKIRENTE                  !Frenzel  !2.5 *
      *        !                                        !         !    *
      *30.09.06!Neue Felder LRWGPFRENTE / GPFRENTANW    !Hübner   !    *
      *        ! AKI615                                 !         !    *
      *10.04.08!Neue Felder AKI00711                    !Frenzel  !2.7 *
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *23.02.09!Erweiterung (OCCURS) von 4 auf 10 Fonds !HSE      !2.8 *
      *AKI00798!Satzlaenge von 563 auf 755 Stellen erh. !         !    *
      *--------!----------------------------------------!---------!----*
280809*28.08.09!Neues Feld: DECKUNGSKAPITAL             !HSE      !2.9 *
      *AKI00860!Satzlaenge von 755 auf 763 Stellen erh. !         !    *
      *--------!----------------------------------------!---------!----*
070610*07.06.10!Neue Felder: FMBTFRTT, FMBTFRTT-INFO,   !HSE      !2.10*
      *AKI00969!             BTFRISTWERT, BTFRMINDWERT. !         !    *
      *        !Satzlaenge von 763 auf 1000 Stellen erh.!         !    *
      *--------!----------------------------------------!---------!----*
      *13.12.10!Neue Felder zum AML-RT 06/2010          !IFR      !2.11*
      *--------!----------------------------------------!---------!----*
260811*26.08.11!Neue Felder zum AML-RT 09/2011          !HSE      !2.12*
      *        !LRECL von 1000 auf 2000 erweitert       !         !    *
      *--------!----------------------------------------!---------!----*
010812*01.08.12!Neue Felder zum AML-RT 07/2012          !HSE      !2.13*
      *--------!----------------------------------------!---------!----*
011013*01.10.13!Neue Felder zum AML-RT 12/2013          !HSE      !2.14*
      *--------!----------------------------------------!---------!----*
020614*02.06.14!Neue Felder zum AML-RT 06/2014          !HSE      !2.15*
      *--------!----------------------------------------!---------!----*
100315*10.03.15!Neue Felder zum AML-RT 03/2015          !HHU      !2.16*
      *--------!----------------------------------------!---------!----*
010616*01.06.16!Neue Felder zum AML-RT 06/2016          !HSE      !2.17*
      *AKI01434!Achtung: Felder sind nicht am Ende der  !         !    *
      *        !Satzstruktur, sond. thematisch einsort. !         !    *
      *--------!----------------------------------------!---------!----*
130117*13.01.17! AKI01488 Neu: Konsortial-KZ            !IFR      !2.16*
      *--------!----------------------------------------!---------!----*
200217*20.02.17! AKI01492 Neu: GARABLRNTPRL, AKTVTRWERT,!GWA      !2.18*
      *        ! GARABLRNTPRL-INFO, AKTVTRWERT-INFO     !         !    *
      *        ! Konsortialkennz. jetzt hinter WIRKDAT  !         !    *
      *--------!----------------------------------------!---------!----*
040718*04.07.18! AKI01605 Neue Felder AML-RT 09/2018    !GWA      !2.19*
      *        ! GBURENTANW                             !         !    *
      *--------!----------------------------------------!---------!----*
      *30.01.20! AKI01757 ÄNDERUNG AML-RT 03/2020       !T13AC20  !2.20*
      *        !   NEUE FELDER:                         !         !    *
      *        !     PFSTAFFELKZN                       !         !    *
      *        !     GARPFRENTE1-5                      !         !    *
      *        !     GARPFRENTE-INFO                    !         !    *
      *        !     GESAMTPFRENTE1-5                   !         !    *
      *        !     GESAMTPFRENTE-INFO                 !         !    *
      *        !     GARPFRENTANW                       !         !    *
      *        !     GARPFRENTANW-INFO                  !         !    *
      *        !     GESAMTPFRENTANW                    !         !    *
      *        !     GESAMTPFRENTANW-INFO               !         !    *
      *        !     LSTPROZPFL1-5                      !         !    *
      *        !   ENTFALLENE FELDER:                   !         !    *
      *        !     GPFRENTE                           !         !    *
      *        !     GPFRENTE-INFO                      !         !    *
      *        !     GPFRENTANW                         !         !    *
      *        !     GPFRENTANW-INFO                    !         !    *
      *--------!----------------------------------------!---------!----*
      *
       01  DRKI0017.
      *
      ***********************************************************
      *                                                         *
      *         HAUPTSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  AML-SATZ.
      *
      ***DIE DATENGRUPPE MUSS IN DER FORM BESTEHEN WEGEN AML-KZ # !!
             15  AML-RECHENWERTE.
      *
               20 AML-VNR                   PICTURE 9(10).
               20 AML-WIRKDAT               PICTURE 9(8).
200217*     Konsortialkennzeichen
200217         20 AML-KONSORTIAL-KZ         PIC X(0001).
 
               20 AML-GRTBTG                PICTURE S9(11)V99 COMP-3.
               20 AML-GRTBTG-INFO           PICTURE X.
 
160403         20 AML-ZAHLBTG               PICTURE S9(11)V99 COMP-3.
160403         20 AML-ZAHLBTG-INFO          PICTURE X.
 
               20 AML-GSMTVSR               PICTURE S9(11)V99 COMP-3.
               20 AML-GSMTVSR-INFO          PICTURE X.
 
               20 AML-GBURENTE              PICTURE S9(7)V99 COMP-3.
               20 AML-GBURENTE-INFO         PICTURE X.
 
               20 AML-GBUZVRENTE            PICTURE S9(7)V99 COMP-3.
               20 AML-GBUZVRENTE-INFO       PICTURE X.
 
               20 AML-GUZVSUMME             PICTURE S9(11)V99 COMP-3.
               20 AML-GUZVSUMME-INFO        PICTURE X.
               20 AML-SUMRKW                PICTURE S9(11)V99
                                                           COMP-3.
               20 AML-SUMRKW-INFO           PICTURE X.
               20 AML-BTFRSUM               PICTURE S9(11)V99
                                                 COMP-3.
               20 AML-BTFRSUM-INFO          PICTURE X.
               20 AML-GSMTBUZBONRNT         PICTURE S9(7)V99
                                                           COMP-3.
               20 AML-GSMTBUZBONRNT-INFO    PICTURE X.
               20 AML-VZWERT                PICTURE S9(11)V99
                                                           COMP-3.
               20 AML-VZWERT-INFO           PICTURE X.
               20 AML-BELEIHWERT            PICTURE S9(11)V99
                                                           COMP-3.
               20 AML-BELEIHWERT-INFO       PICTURE X.
               20 AML-VSRTOD                PICTURE S9(11)V99
                                                           COMP-3.
               20 AML-VSRTOD-INFO           PICTURE X.
               20 AML-VSRTODUEB             PICTURE S9(11)V99
                                                           COMP-3.
               20 AML-VSRTODUEB-INFO        PICTURE X.
010616         20 AML-VSRSK                 PICTURE S9(11)V99 COMP-3.
010616         20 AML-VSRSK-INFO            PICTURE X.
010616         20 AML-VSRSKUEB              PICTURE S9(11)V99 COMP-3.
010616         20 AML-VSRSKUEB-INFO         PICTURE X.
               20 AML-ERMABL                PICTURE 9(8).
               20 AML-ERMABL-INFO           PICTURE X.
               20 AML-GARLSTABL             PICTURE S9(11)V99
                                                           COMP-3.
               20 AML-GARLSTABL-INFO        PICTURE X.
               20 AML-VORLSTABL             PICTURE S9(11)V99
                                                           COMP-3.
               20 AML-VORLSTABL-INFO        PICTURE X.
               20 AML-GARKAPABF             PICTURE S9(11)V99
                                                           COMP-3.
               20 AML-GARKAPABF-INFO        PICTURE X.
               20 AML-VORKAPABF             PICTURE S9(11)V99
                                                           COMP-3.
               20 AML-VORKAPABF-INFO        PICTURE X.
               20 AML-TAZTERM               OCCURS 8 TIMES
                                            PICTURE 9(8).
               20 AML-TAZTERM-INFO          OCCURS 8 TIMES
                                            PICTURE X.
               20 AML-TAZBETR               OCCURS 8 TIMES
                                            PICTURE S9(11)V99
                                                           COMP-3.
               20 AML-TAZBETR-INFO          OCCURS 8 TIMES
                                            PICTURE X.
000606         20 AML-TAZRENTE              OCCURS 8 TIMES
000606                                      PICTURE S9(11)V99
000606                                                     COMP-3.
000606         20 AML-TAZRENTE-INFO         OCCURS 8 TIMES
000606                                      PICTURE X.
               20 AML-VORLSTABLSIM          PICTURE S9(11)V99
                                                           COMP-3.
               20 AML-VORLSTABLSIM-INFO     PICTURE X.
               20 AML-VORKAPABFSIM          PICTURE S9(11)V99
                                                           COMP-3.
               20 AML-VORKAPABFSIM-INFO     PICTURE X.
021115***      20 AML-FLVAE                 OCCURS 3 TIMES
021115*        20 AML-FLVAE     OCCURS 4 TIMES
230209         20 AML-FLVAE     OCCURS 10 TIMES
100408*                       PICTURE S9(8)V9(3) COMP-3.
100408                        PICTURE S9(8)V9(6) COMP-3.
02115****      20 AML-FLVAE-INFO            OCCURS 3 TIMES
021115*        20 AML-FLVAE-INFO  OCCURS 4 TIMES     PICTURE X.
230209         20 AML-FLVAE-INFO  OCCURS 10 TIMES     PICTURE X.
030299*****    20 AML-FLVFO                 OCCURS 3 TIMES
030299*****                                 PICTURE X.
021115***      20 AML-FLVFO                 OCCURS 3 TIMES
021115*        20 AML-FLVFO   OCCURS 4 TIMES   PICTURE X(40).
100408*        20 AML-FLVFO   OCCURS 4 TIMES   PICTURE X(02).
230209         20 AML-FLVFO   OCCURS 10 TIMES   PICTURE X(02).
021115***      20 AML-FLVFO-INFO            OCCURS 3 TIMES
021115*        20 AML-FLVFO-INFO   OCCURS 4 TIMES  PICTURE X.
230209         20 AML-FLVFO-INFO   OCCURS 10 TIMES  PICTURE X.
021115***      20 AML-BWFRUECK              OCCURS 3 TIMES
021115*        20 AML-BWFRUECK   OCCURS 4 TIMES
230209         20 AML-BWFRUECK   OCCURS 10 TIMES
100408*                          PICTURE S9(5)V99 COMP-3.
100408                           PICTURE S9(11)V99 COMP-3.
021115***      20 AML-BWFRUECK-INFO         OCCURS 3 TIMES
021115*        20 AML-BWFRUECK-INFO   OCCURS 4 TIMES PICTURE X.
230209         20 AML-BWFRUECK-INFO   OCCURS 10 TIMES PICTURE X.
021115***      20 AML-FODK                  OCCURS 4 TIMES
021115*        20 AML-FODK   OCCURS 5 TIMES
230209         20 AML-FODK   OCCURS 11 TIMES
                                PICTURE S9(11)V99 COMP-3.
021115***      20 AML-FODK-INFO             OCCURS 4 TIMES
021115*        20 AML-FODK-INFO   OCCURS 5 TIMES  PICTURE X.
230209         20 AML-FODK-INFO   OCCURS 11 TIMES  PICTURE X.
021115***      20 AML-FOPROZ                OCCURS 3 TIMES
021115*        20 AML-FOPROZ   OCCURS 4 TIMES
230209         20 AML-FOPROZ   OCCURS 10 TIMES
                                PICTURE S999V99 COMP-3.
021115***      20 AML-FOPROZ-INFO           OCCURS 3 TIMES
021115*        20 AML-FOPROZ-INFO   OCCURS 4 TIMES  PICTURE X.
230209         20 AML-FOPROZ-INFO   OCCURS 10 TIMES  PICTURE X.
               20 AML-FORENDITE             PICTURE S999V99
                                                           COMP-3.
               20 AML-FORENDITE-INFO        PICTURE X.
 
070604         20 AML-LRWGKIRENTE             PIC S9(7)V9(2) COMP-3.
070604         20 AML-LRWGKIRENTE-INFO        PICTURE X.
 
               20 AML-PFSTAFFELKZN       PIC  X(001).
               20 AML-GARPFRENTE1        PIC S9(011)V99 PACKED-DECIMAL.
               20 AML-GARPFRENTE2        PIC S9(011)V99 PACKED-DECIMAL.
               20 AML-GARPFRENTE3        PIC S9(011)V99 PACKED-DECIMAL.
               20 AML-GARPFRENTE4        PIC S9(011)V99 PACKED-DECIMAL.
               20 AML-GARPFRENTE5        PIC S9(011)V99 PACKED-DECIMAL.
               20 AML-GARPFRENTE-INFO    PIC  X(001).
               20 AML-GESAMTPFRENTE1     PIC S9(011)V99 PACKED-DECIMAL.
               20 AML-GESAMTPFRENTE2     PIC S9(011)V99 PACKED-DECIMAL.
               20 AML-GESAMTPFRENTE3     PIC S9(011)V99 PACKED-DECIMAL.
               20 AML-GESAMTPFRENTE4     PIC S9(011)V99 PACKED-DECIMAL.
               20 AML-GESAMTPFRENTE5     PIC S9(011)V99 PACKED-DECIMAL.
               20 AML-GESAMTPFRENTE-INFO PIC  X(001).
               20 AML-GARPFRENTANW       PIC S9(011)V99 PACKED-DECIMAL.
               20 AML-GARPFRENTANW-INFO  PIC  X(001).
               20 AML-GESAMTPFRENTANW    PIC S9(011)V99 PACKED-DECIMAL.
               20 AML-GESAMTPFRENTANW-INFO
                                         PIC  X(001).
               20 AML-LSTPROZPFL1        PIC S9(003)    PACKED-DECIMAL.
               20 AML-LSTPROZPFL2        PIC S9(003)    PACKED-DECIMAL.
               20 AML-LSTPROZPFL3        PIC S9(003)    PACKED-DECIMAL.
               20 AML-LSTPROZPFL4        PIC S9(003)    PACKED-DECIMAL.
               20 AML-LSTPROZPFL5        PIC S9(003)    PACKED-DECIMAL.
 
100408         20 AML-SUMBETBWRRK             PIC S9(11)V9(2) COMP-3.
100408         20 AML-SUMBETBWRRK-INFO        PICTURE X.
 
100408         20 AML-BEGABRUFPHASE           PIC X(008).
100408*           Format JJJJMMTT
 
100408         20 AML-VORVORGLSTABLSIM        PIC S9(11)V9(2) COMP-3.
100408         20 AML-VORVORGLSTABLSIM-INFO   PICTURE X.
 
100408         20 AML-VORVORGKAPABFSIM        PIC S9(11)V9(2) COMP-3.
100408         20 AML-VORVORGKAPABFSIM-INFO   PICTURE X.
280809* Bei RVs wird statt des Rueckkaufswerts das Deckungskapital gel.
280809         20 AML-SUMDK                   PIC S9(11)V9(2) COMP-3.
280809         20 AML-SUMDK-INFO              PICTURE X.
070610         20 AML-FMBTFRTT                PIC 9(08).
>>    * frühestmöglicher Beitragsfreistellungstermin (JJJJMMTT)
               20 AML-FMBTFRTT-INFO           PICTURE X.
               20 AML-BTFRISTWERT             PIC S9(11)V9(2) COMP-3.
      * erreichter beitragsfreier Wert
    <<         20 AML-BTFRMINDWERT            PIC S9(11)V9(2) COMP-3.
070610* beitragsfreier Mindestwert
131210         20 AML-FRUEHBEGRNTZAHLG        PIC 9(008).
>>    *           FORMAT JJJJMMTT
      * frühestmögl. Beginn der Rentenzahlung
               20 AML-GEURENTE                PIC S9(7)V9(2) COMP-3.
      * Gesamt- EU-Rente
               20 AML-GEURENTE-INFO           PICTURE X.
      * Berechnungs-KZ zu Gesamt- EU-Rente
               20 AML-GEUZVRENTE              PIC S9(7)V9(2) COMP-3.
      * Gesamt- EUZV-Rente
               20 AML-GEUZVRENTE-INFO         PICTURE X.
      * Berechnungs-KZ zu Gesamt- EUZV-Rente
               20 AML-GSMTEUZBONRNT           PIC S9(7)V9(2) COMP-3.
      * Gesamt- EUZV-Bonus-Rente
    <<         20 AML-GSMTEUZBONRNT-INFO      PICTURE X.
131210* Berechnungs-KZ zu Gesamt-EUZV-Bonus-Rente
260811* Neue Felder zum Produkt VASP(Vermögensaufbau und Sicherungsplan)
>>             20 AML-RKWHV                   PIC S9(11)V9(2) COMP-3.
      *  Gesamt-Rückkaufswert des Teiltypen 01 (Hauptversicherung)
      *  zum TT(Berechnungstermin), wenn VA existiert
               20 AML-RKWHV-INFO              PICTURE X.
      * Berechnungs-KZ zu RKW HV
               20 AML-RKWBUZV                 PIC S9(11)V9(2) COMP-3.
      * Gesamt-RKW TT 11 und 12 (BUZV-Befreiung und BUZV-Rente)
      * zum TT, wenn SP existiert
               20 AML-RKWBUZV-INFO            PICTURE X.
      * Berechnungs-KZ zu RKWBUZV
               20 AML-RKWTZV                  PIC S9(11)V9(2) COMP-3.
      * Gesamt-RKW des Teiltypen 22 (TZV) zum TT, wenn SP existiert
    <<         20 AML-RKWTZV-INFO             PICTURE X.
260811* Berechnungs-KZ zu RKWTZV
010616         20 AML-RKWSK                   PIC S9(11)V9(2) COMP-3.
010616* Gesamt-RKW des Teiltypen 27 (SK) zum TT, wenn SP existiert
010616         20 AML-RKWSK-INFO              PICTURE X.
010616* Berechnungs-KZ zu RKWSK
260811         20 AML-RKWPZV                  PIC S9(11)V9(2) COMP-3.
>>    * Gesamt-RKW TT 23 oder 24 (POZV oder PZV) zum TT, wenn SP exist.
               20 AML-RKWPZV-INFO             PICTURE X.
      * Berechnungs-KZ zu RKWPZV
               20 AML-RKWEUZV                 PIC S9(11)V9(2) COMP-3.
      * Gesamt-RKW TT 25 und 26 (EUZV-Befreiung und -Rente] zum TT,
      * wenn SP existiert
               20 AML-RKWEUZV-INFO            PICTURE X.
    <<* Berechnungs-KZ zu RKWEUZV
260811*        20 FILLER                      PIC X(1148).
010812* Weitere neue Felder
      *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
               20 AML-BTFRSUMBUZV             PIC S9(11)V9(2) COMP-3.
      * beitragsfreie Jahresrente der Teiltypen 11 und 12
      * [BUZV-Befreiung und BUZV-Rente] zum TT, wenn SP existiert
               20 AML-BTFRSUMBUZV-INFO        PIC X(001).
               20 AML-BTFRSUMTZV              PIC S9(11)V9(2) COMP-3.
      * beitragsfreie Versicherungssumme des Teiltypen 22 [TZV] zum TT,
      * wenn SP existiert
               20 AML-BTFRSUMTZV-INFO         PIC X(001).
               20 AML-BTFRSUMPZV              PIC S9(11)V9(2) COMP-3.
      * beitragsfreie Jahresrente der Teiltypen 23 oder 24
      * [POZV oder PZV] zum TT, wenn SP existiert
               20 AML-BTFRSUMPZV-INFO         PIC X(001).
               20 AML-BTFRSUMEUZV             PIC S9(11)V9(2) COMP-3.
      * beitragsfreie Jahresrente der Teiltypen 25 und 26
      * [EUZV-Befreiung und EUZV-Rente] zum TT, wenn SP existiert
               20 AML-BTFRSUMEUZV-INFO        PIC X(001).
010616         20 AML-BTFRSUMSK               PIC S9(11)V9(2) COMP-3.
010616* beitragsfreie Vers-Summe des Teiltypen 27 (SK)
010616         20 AML-BTFRSUMSK-INFO          PIC X(001).
               20 AML-FMBTFRTTBUZV            PIC 9(008).
      * frühestmöglicher Beitragsfreistellungstermin der Teiltypen 11
      * und 12 [BUZV-Befreiung und BUZV-Rente] zum TT, wenn SP existiert
               20 AML-FMBTFRTTBUZV-INFO        PIC X(001).
               20 AML-FMBTFRTTTZV             PIC 9(008).
      * frühestmöglicher Beitragsfreistellungstermin des Teiltypen 22
      * [TZV] zum TT, wenn SP existiert
               20 AML-FMBTFRTTTZV-INFO         PIC X(001).
               20 AML-FMBTFRTTPZV              PIC 9(008).
      * frühestmöglicher Beitragsfreistellungstermin der Teiltypen 23
      * oder 24 [POZV oder PZV] zum TT, wenn SP existiert
               20 AML-FMBTFRTTPZV-INFO         PIC X(001).
               20 AML-FMBTFRTTEUZV             PIC 9(008).
      * frühestmöglicher Beitragsfreistellungstermin der Teiltypen 25
      * und 26  [EUZV-Befreiung und EUZV-Rente] zum TT, wenn SP exist.
               20 AML-FMBTFRTTEUZV-INFO        PIC X(001).
010616         20 AML-FMBTFRTTSK               PIC 9(008).
010616* frühestmöglicher Beitragsfreistellungstermin des Teiltypen 27
010616         20 AML-FMBTFRTTSK-INFO          PIC X(001).
               20 AML-BTFRISTWERTBUZV          PIC S9(11)V9(2) COMP-3.
      * erreichter beitragsfreier Wert der Teiltypen 11 und 12
      * [BUZV-Befreiung und BUZV-Rente] zum TT, wenn SP existiert
               20 AML-BTFRISTWERTTZV          PIC S9(11)V9(2) COMP-3.
      *  ERREICHTER BEITRAGSFREIER WERT DES TEILTYPEN 22 [TZV] ZUM TT,
      *  WENN SP EXISTIERT
               20 AML-BTFRISTWERTPZV          PIC S9(11)V9(2) COMP-3.
      *  ERREICHTER BEITRAGSFREIER WERT DER TEILTYPEN 23 ODER 24 [POZV
      *  ODER PZV] ZUM TT, WENN SP EXISTIERT
               20 AML-BTFRISTWERTEUZV         PIC S9(11)V9(2) COMP-3.
      *  ERREICHTER BEITRAGSFREIER WERT DER TEILTYPEN 25 UND 26
010616         20 AML-BTFRISTWERTSK           PIC S9(11)V9(2) COMP-3.
010616*  ERREICHTER BEITRAGSFREIER WERT DEs TEILTYPEN 27 (SK)
               20 AML-BTFRMINDWERTBUZV        PIC S9(11)V9(2) COMP-3.
      *  BEITRAGSFREIER MINDESTWERT DER TEILTYPEN 11 UND 12
      *  [BUZV-BEFREIUNG UND BUZV-RENTE] ZUM TT, WENN SP EXISTIERT
               20 AML-BTFRMINDWERTTZV         PIC S9(11)V9(2) COMP-3.
      *  BEITRAGSFREIER MINDESTWERT DES TEILTYPEN 22 [TZV] ZUM TT, WENN
      *  SP EXISTIERT
               20 AML-BTFRMINDWERTPZV         PIC S9(11)V9(2) COMP-3.
      *  BEITRAGSFREIER MINDESTWERT DER TEILTYPEN 23 ODER 24 [POZV ODER
      *  PZV] ZUM TT, WENN SP EXISTIERT
               20 AML-BTFRMINDWERTEUZV        PIC S9(11)V9(2) COMP-3.
      *  BEITRAGSFREIER MINDESTWERT DER TEILTYPEN 25 UND 26
      *  [EUZV-BEFREIUNG UND EUZV-RENTE] ZUM TT, WENN SP EXISTIERT
010616         20 AML-BTFRMINDWERTSK          PIC S9(11)V9(2) COMP-3.
010616*  BEITRAGSFREIER MINDESTWERT DEs TEILTYPEN 27 (SK)
               20 AML-BETBWRRKHV              PIC S9(11)V9(2) COMP-3.
      *  BETEILIGUNG AN BEWERTUNGSRESERVE RK DES TEILTYPEN 01
      *  [HAUPTVERSICHERUNG] ZUM TT, WENN VA EXISTIERT
               20 AML-BETBWRRKHV-INFO         PIC X(001).
               20 AML-BETBWRRKBUZV            PIC S9(11)V9(2) COMP-3.
      *  BETEILIGUNG AN BEWERTUNGSRESERVE RK DER TEILTYPEN 11 UND 12
               20 AML-BETBWRRKBUZV-INFO       PIC X(001).
               20 AML-BETBWRRKTZV             PIC S9(11)V9(2) COMP-3.
      *  BETEILIGUNG AN BEWERTUNGSRESERVE RK DES TEILTYPEN 22 [TZV]
      *  ZUM TT, WENN SP EXISTIERT
               20 AML-BETBWRRKTZV-INFO        PIC X(001).
               20 AML-BETBWRRKPZV             PIC S9(11)V9(2) COMP-3.
      *  BETEILIGUNG AN BEWERTUNGSRESERVE RK DER TEILTYPEN 23 ODER 24
      *  [POZV ODER PZV] ZUM TT, WENN SP EXISTIERT
               20 AML-BETBWRRKPZV-INFO        PIC X(001).
               20 AML-BETBWRRKEUZV            PIC S9(11)V9(2) COMP-3.
      *  BETEILIGUNG AN BEWERTUNGSRESERVE RK DER TEILTYPEN 25 UND 26
      *  [EUZV-BEFREIUNG UND EUZV-RENTE] ZUM TT, WENN SP EXISTIERT
               20 AML-BETBWRRKEUZV-INFO       PIC X(001).
               20 AML-KONVGH                  PIC S9(11)V9(2) COMP-3.
      *  KONVENTIONELLES GUTHABEN ZUM TT, WENN VA, 5RG ODER BRGV
010812*  EXISTIERT
    <<         20 AML-KONVGH-INFO             PIC X(001).
011013         20 AML-GARVORGLSTABL           PIC S9(11)V9(2) COMP-3.
>>    *  GARANTIERTE LEISTUNG ZU BEGINN DER VERFÜGUNGSPHASE
               20 AML-GARVORGLSTABL-INFO      PIC X(001).
               20 AML-VORVORGLSTABL           PIC S9(11)V9(2) COMP-3.
      *  VORAUSSICHTLICHE LEISTUNG ZU BEGINN DER VERFÜGUNGSPHASE
      *  (HOCHGERECHNETE ÜBERSCHÜSSE UND KURSE)
               20 AML-VORVORGLSTABL-INFO      PIC X(001).
               20 AML-GARVORGKAPABF           PIC S9(11)V9(2) COMP-3.
      *  garantierte Kapitalabfindung zu Beginn der Verfügungsphase
               20 AML-GARVORGKAPABF-INFO      PIC X(001).
               20 AML-VORVORGKAPABF           PIC S9(11)V9(2) COMP-3.
      *  VORAUSSICHTLICHE KAPITALABFINDUNG ZU BEGINN DER
      *  VERFÜGUNGSPHASE (HOCHGERECHNETE ÜBERSCHÜSSE UND KURSE)
               20 AML-VORVORGKAPABF-INFO      PIC X(001).
               20 AML-ABZUG                   PIC S9(11)V9(2) COMP-3.
      *  GESAMT-STORNOABZUG BEI UNISEX-TARIFEN
               20 AML-ABZUG-INFO              PIC X(001).
               20 AML-AUSZBETR                PIC S9(11)V9(2) COMP-3.
      *  AUSZAHLUNGSBETRAG
               20 AML-AUSZBETR-INFO           PIC X(001).
               20 AML-SUMRKWPROLTAZ           PIC S9(11)V9(2) COMP-3.
      *  Gesamt-RKW der prolongierten TAZen zum TT
               20 AML-SUMRKWPROLTAZ-INFO      PIC X(001).
               20 AML-VORLSTABLPROLTAZ        PIC S9(11)V9(2) COMP-3.
      *   voraussichtliche Leistung bei Ablauf der prolongierten TAZen
               20 AML-VORLSTABLPROLTAZ-INFO   PIC X(001).
               20 AML-VORPROLTAZKAPABF        PIC S9(11)V9(2) COMP-3.
      *    voraussichtl. Kapitalabfindung bei Ablauf der prolong. TAZen
               20 AML-VORPROLTAZKAPABF-INFO   PIC X(001).
020614***********************************************************
020614         20 AML-VERMOEGTOPF             PIC S9(11)V9(2) COMP-3.
      *    Vermoegenstopf
               20 AML-VERMOEGTOPF-INFO        PIC X(001).
               20 AML-EINKOMTOPF              PIC S9(11)V9(2) COMP-3.
      *    Einkommenstopf
               20 AML-EINKOMTOPF-INFO         PIC X(001).
               20 AML-BSPBEGRNTZHLG           PIC 9(008).
      *     Beispiel-Beginn der Rentenzahlung (JJJJMMTT)
               20 AML-SUMTEILRENTEN           PIC S9(11)V9(2) COMP-3.
    <<*     Summe der Teilrenten zum TT
020614         20 AML-SUMTEILRNT-INFO         PIC X(001).
010914***********************************************************
               20 AML-GARVORGABLLST           PIC S9(11)V9(2) COMP-3.
      *  garantierte Leistung zwischen Beginn der Verfuegungsphase und
      *  Ablauf bzw. zum Beispiel-Beginn der Rentenzahlung
               20 AML-GARVORGABLLST-INFO      PIC X(001).
               20 AML-GARVORGABLKAPABF        PIC S9(11)V9(2) COMP-3.
      *  garantierte Kapitalabfindung zwischen ... und .. (s.o.)
               20 AML-GARVORGABLKAPABF-INFO   PIC X(001).
      * Wiederholgruppe mit 3 Elementen, von denen nur das Erste ver-
      * wendet wird
               20 AML-VORAUSSICHTL-LSTG  OCCURS 3.
                25 AML-VORVORGABLLST           PIC S9(11)V9(2) COMP-3.
      *  voraussichtl. Leistung zwischen Beg. der Verfuegungsphase und
      *  Ablauf bzw. zum Beispiel-Beginn.. (hochg. Ueberschuesse)
                25 AML-VORVORGABLST-INFO       PIC X(001).
                25 AML-VORVORGABLKAPABF        PIC S9(11)V9(2) COMP-3.
    <<*  voraussichtl. Kapitalabfindung zwischen ... und .. (s.o.)
010914          25 AML-VORVORGABLKAPABF-INFO   PIC X(001).
100315         20 AML-SICHERHEITRES          PIC S9(11)V9(2) COMP-3.
100315*     Sicherheits-Reserve
100315         20 AML-SICHERHEITRES-INFO     PIC X(001).
200217*  Garantierte Rente zum Ablauf incl. nicht in Anspruch genommener
200217*  Teilauszahlungen
               20 AML-GARABLRNTPRL            PIC S9(11)V9(2) COMP-3.
200217         20 AML-GARABLRNTPRL-INFO       PIC X(0001).
200217*  Aktueller Vertragswert
               20 AML-AKTVTRWERT              PIC S9(11)V9(2) COMP-3.
200217         20 AML-AKTVTRWERT-INFO         PIC X(0001).
040718*  Gesamt-BU-Renten-Anwartschaft
               20 AML-GBURENTANW              PIC S9(07)V9(2) COMP-3.
040718         20 AML-GBURENTANW-INFO         PIC X(0001).
100315***********************************************************
    <<*     Summe der Teilrenten zum TT
    <<*        20 FILLER                      PIC X(0928).
011013*        20 FILLER                      PIC X(0904).
020614*        20 FILLER                      PIC X(0872).
100315*        20 FILLER                      PIC X(0799).
010616*        20 FILLER                      PIC X(0745).
120117*        20 FILLER                      PIC X(0744).
200217*        20 FILLER                      PIC X(0728).
040718*        20 FILLER                      PIC X(0722).
               20 FILLER                      PIC X(0635).
 
      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************
 
           10  AML-VORSATZ    REDEFINES     AML-SATZ.
      *
               20  AML-V-SA            PIC X(01).
      *            SATZART = 'A'
               20  AML-V-ABSENDER      PIC X(03).
      *            ABSENDER = 'AML'
               20  AML-V-DATUM         PIC 9(08).
      *            ERFASSUNGS-/AENDERUNGSDATUM = 'JJJJMMTT'
               20  AML-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER = 'DVAG'
               20  AML-V-TEXT          PIC X(20).
      *            INHALTSANGABE = 'RECHENWERTE'
               20  AML-V-SAETZE-ENTHALTEN PIC X(01).
      *            KENNZEICHEN = 'K' ODER LEER WENN DATEN KOMMEN
 
      ***********************************************************
      *                                                         *
      *          NACHSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  AML-NACHSATZ    REDEFINES     AML-SATZ.
      *
               20  AML-N-SA            PIC X(01).
      *            SATZART             'E'
               20  AML-N-ANZ           PIC 9(09).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
 
 


