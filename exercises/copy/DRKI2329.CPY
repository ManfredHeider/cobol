      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2329.CPY  $
      *    $Revision:   2.12  $
      *
      *    $Log:   L:/mf/CPY/DRKI2329.CPv  $
      *
      *   Rev 2.12   May 23 2017 15:40:50   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.11   May 23 2017 15:35:18   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.10   May 23 2017 15:07:28   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.9   Mar 27 2017 09:28:58   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.8   Mar 09 2016 14:44:26   t13ex73
      *AAT00049: Fehlerverfahren mit Prozentkriterien - G48 PGMs
      *
      *   Rev 2.7   Jan 26 2016 12:49:48   t13ex39
      *AKI01411: Namensänderung DWS Altersvorsorgeprodukte
      *
      *   Rev 2.6   Sep 02 2013 12:22:16   t13ex14
      *AKI01204: SEPA - DB-Online - DWS Sparplan
      *
      *   Rev 2.5   Jul 02 2012 10:35:34   t13ex39
      *AKI01149: PIN bei DWS und DeuBa
      *
      *   Rev 2.4   Jun 29 2011 07:55:26   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g Dauerzulagenantrag
      *
      *   Rev 2.3   Oct 27 2009 12:20:16   t13ex50
      *
      *   Rev 2.2   Aug 20 2009 15:24:18   t13ex50
      *AKI00844: DWS-Investmentsparpläne
      *
      *   Rev 2.1   Aug 17 2009 15:03:30   t13ex50
      *AKI00844: DWS-Investmentsparpläne
      *
      *   Rev 2.0   Aug 03 2009 11:44:04   t13ex50
      *Archiverstellung
      *
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für Investmentsparpläne von DWS               *
      *                                                                *
      *    SATZLÄNGE   300                                             *
010813*    SATZLÄNGE   350                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!--------!----!-----------------------------!-----------*
      *AUFTRAG !Datum   !Ver !Text                         !Name       *
      *--------!--------!----!-----------------------------!-----------*
      *AKI00844!03.08.09!2.00! Neuerstellung               !Schleicher *
      *--------!--------!----!-----------------------------!-----------*
      *AKI01040!25.05.11!3.00! Neue Felder Dauerzulagenantr!P. Hübner  *
      *--------!--------!----!-----------------------------!-----------*
100612*AKI01149!11.06.12!5.0 !PIN verarbeiten              !WHO        *
      *----------------------------------------------------------------*
010813*AKI01204!01.08.13! NEUE FELDER EINGEFÜGT f. SEPA    !WMI        *
010813*        !        !   UND DEN FILLER ANGEPASST       !           *
      *----------------------------------------------------------------*
110116*AKI01411!11.01.16!neue Vertragstypen für 'XX'-Alt-  ! WHO      *
      *        !        !vorsorge                          !          *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      *AAT00049!03.03.16! Umbau auf Replacing by XX        !DDY        *
      *        !        ! - ohne Vermerk in AREA A -       !           *
      *----------------------------------------------------------------*
      *FKI01544!24.03.17! FVD entfernt                     !HHU        *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
240417*AKI01512!24.04.17! neu: 'Rechtsstand'               !WHO        *
      *        !        !                                  !           *
      *        !        !                                  !           *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  DRKI2329.
      *
      ***********************************************************
      *                                                         *
      *         HAUPTSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-SATZ.
      *
               15  'XX'-SA                             PIC  X(002).
      *            SATZART        = '16'
               15  'XX'-GES                            PIC  9(002).
      *            GESELLSCHAFT   = '48'
               15  'XX'-KEY.
                   20  'XX'-VNR.
                       25 'XX'-FILIAL-NR               PIC  9(003).
                       25 'XX'-KDNR                    PIC  9(007).
                       25 'XX'-VERT-ORDNUNGSNR         PIC  9(002).
      *        UNTERDEPOTNUMMER
                   20  FILLER REDEFINES 'XX'-VNR.
                       25 'XX'-VNR-1                   PIC  9(010).
                       25 FILLER                       PIC  X(002).
               15  'XX'-PORTFOLIONUMMER                PIC  X(010).
               15  'XX'-DATUM-EROEFFNUNG.
                   20  JAHR-EROEFFNUNG                 PIC  9(004).
                   20  MONAT-EROEFFNUNG                PIC  9(002).
                   20  TAG-EROEFFNUNG                  PIC  9(002).
               15  'XX'-DATUM-AUSZAHLTERMIN.
                   20  JAHR-AUSZAHLTERMIN              PIC  9(004).
                   20  MONAT-AUSZAHLTERMIN             PIC  9(002).
                   20  TAG-AUSZAHLTERMIN               PIC  9(002).
      *            FRÜHESTER AUSZAHLTERMIN, VERTRAGSENDE
      *            KANN AUCH SPÄTER SEIN
               15  'XX'-BANKVERBINDUNG                 PIC  X(020).
      *            Name des Instituts
               15  'XX'-BLZ                            PIC  9(011).
               15  'XX'-KONTONUMMER                    PIC  9(018).
               15  'XX'-ABW-KONTOINHABER               PIC  X(027).
               15  'XX'-SPARRATE                       PIC  9(007)V99.
      *               SPARLEISTUNG FUER SPARPLAENE
               15  'XX'-DYNAM                          PIC  9(002)V99.
      *               DYNAMISIERUNG FUER SPARPLAENE
               15  'XX'-ZAHLWEISE                      PIC  9(001).
      *               ZAHLWEISE.........0 : ZAHLWEISE FEHLT
      *               ZAHLWEISE.........1 : JAEHRLICH
      *               ZAHLWEISE.........2 : HALBJAEHRLICH
      *               ZAHLWEISE.........4 : VIERTELJAEHRLICH
      *               ZAHLWEISE.........5 : 2-MONATLICH
      *               ZAHLWEISE.........6 : MONATLICH
      *               ZAHLWEISE.........8 : EINMALPRAEMIE
               15  'XX'-TAG                            PIC  9(002).
      *               TAG IM MONAT FÜR DIE EINZAHLUNG (RATENTERMIN)
               15  'XX'-VERTRAGSTYP                    PIC  9(002).
      *               51 DWS RIESTERRENTE PREMIUM
      *               52 DWS RIESTERRENTE PREMIUM
      *               54 DWS RIESTERRENTE PREMIUM
      *               56 DWS VERMÖGENSSPARPLAN PREMIUM
      *               58 DWS BASISRENTE
      *               79 KAPITALUEBERTRAGUNG
      *               81 DWS TOPRENTE DYNAMIK
      *               82 DWS TOPRENTE DYNAMIK
      *               83 DWS TOPRENTE BALANCE
      *               84 DWS TOPRENTE BALANCE
      *               85 DWS TOPRENTE PLUS
      *               86 DWS TOPRENTE PLUS
      *               87 DWS TOPRENTE DYNAMIK
      *               88 DWS TOPRENTE BALANCE
      *               89 DWS TOPRENTE PLUS
               15  'XX'-ENDE-RATENAUSSETZUNG           PIC  9(008).
      *               ENDEDATUM DER RATENAUSSETZUNG
               15  'XX'-MAHNSTATUS                     PIC  X(002).
      *                'BLANK BLANK' FELD HAT KEINEN INHALT - OK
      *            00  WIE 'BLANK BLANK' - 00 IST STANDARD
      *            09  ZWISCHENSTUFE - KUNDENBRIEF WIRD GEDRUCKT
      *            10  LASTSCHRIFT WURDE NICHT EINGELÖST
      *            19  ZWISCHENSTUFE - KUNDENBRIEF WIRD GEDRUCKT
      *            20  ZWEITE LASTSCHRIFT IN FOLGE WURDE NICHT EINGEL.
      *            25  3 GROSSE STÖRFÄLLE-BEI 4. WIRD VERTRAG RUHEND
      *            29  ANSTEHENDE TEILRÜCKABWICKLUNG (TEILSTORNO)
      *            39  ANSTEHENDE RESTRÜCKABWICKLUNG (VOLLSTORNO)
      *            40  DRITTE LASTSCHRIFT IN FOLGE WURDE NICHT EINGEL.
               15  'XX'-DAUERZULAGENANTRAG             PIC  X(001).
      *            ' '
      *            'J' DAUERZULAGENANTRAG EINGREICHT
      *            'N' DAUERZULAGENANTRAG NICHT EINGREICHT
               15  'XX'-DAT-VERTR-AUFLOESUNG.
                   20  JAHR-VERTR-AUFLOESUNG           PIC  9(004).
                   20  MONAT-VERTR-AUFLOESUNG          PIC  9(002).
                   20  TAG-VERTR-AUFLOESUNG            PIC  9(002).
      *               DATUM VERTRAGSAUFLÖSUNG  BZW. VORZEITIGE
      *               VERTRAGSKÜNDIGUNG
               15  'XX'-DEL-KZ                         PIC  X(001).
      *               LÖSCHKENNZEICHEN
      *                D = SATZ IST ZU LÖSCHEN
      *                S = Datenschutzsperre aktiviert
250511         15  'XX'-KZ-UNM-ZULAGEBER               PIC X(001).
      *              KZ UNMITTELBARE ZULAGENBERECHTIGUNG
               15  'XX'-KZ-UEBWEG-KAPUEB               PIC X(001).
      *              KZ ÜBERTRAGUNGSWEG KAPITALÜBERTRAGUNG
               15  'XX'-KZ-UEBART                      PIC X(001).
      *              KZ ÜBERTRAGUNGSART
               15  'XX'-DAT-KAPITAL-UEBERTRAG.
                   20  JAHR-KAP-UEBERTAG               PIC 9(004).
                   20  MONAT-KAP-UEBERTRAG             PIC 9(002).
                   20  TAG-KAP-UEBERTRAG               PIC 9(002).
      *              DATUM KAPITAL ÜBERTRAG
               15  'XX'-DAT-GELD-BEWEG.
                   20  JAHR-GELD-BEWEG                 PIC 9(004).
                   20  MONAT-GELD-BEWEG                PIC 9(002).
                   20  TAG-GELD-BEWEG                  PIC 9(002).
      *              DATUM GELD BEWEGUNG
               15  'XX'-ANZAHL-KINDZULAGE              PIC 9(002).
      *               ANZAHL KINDERZULAGEN IM ANTRAG
               15  'XX'-GARANTIESUMME                  PIC 9(11)V99.
      *               GARANTIESUMME
               15  'XX'-DAT-BEGINN-ABLSTAB.
                   20  JAHR-BEGINN-ABLSTAB             PIC 9(004).
                   20  MONAT-BEGINN-ABLSTAB            PIC 9(002).
                   20  TAG-BEGINN-ABLSTAB              PIC 9(002).
      *              BEGINN ABLAUFSTABILISATOR
               15  'XX'-DAT-BEGINN-HOECHSICH.
                   20  JAHR-BEGINN-HOECHSICH           PIC 9(004).
                   20  MONAT-BEGINN-HOECHSICH          PIC 9(002).
                   20  TAG-BEGINN-HOECHSICH            PIC 9(002).
      *              BEGINN HÖCHSTSTAND
               15  'XX'-DAT-LETZTHOECHST.
                   20  JAHR-LETZTHOECHST               PIC 9(004).
                   20  MONAT-LETZTHOECHST              PIC 9(002).
                   20  TAG-LETZTHOECHST                PIC 9(002).
      *              DATUM LETZTER HÖCHSTSTAND
               15  'XX'-BETRHOECHST                    PIC 9(11)V99.
      *              HÖCHSTSTAND
               15  'XX'-DAT-KUENDIGUNG.
                   20  JAHR-KUENDIGUNG                 PIC 9(004).
                   20  MONAT-KUENDIGUNG                PIC 9(002).
250511             20  TAG-KUENDIGUNG                  PIC 9(002).
      *              DATUM KÜNDIGUNGSTERMIN
110612         15  'XX'-PIN                            PIC  9(009).
010813         15  'XX'-IBAN                           PIC X(034).
010813*            IBAN des Belastungskontos zur Lastschrift für
010813*            den Rateneinzug
010813         15  'XX'-BIC                            PIC X(011).
010813*            BIC des Belastungskontos zur Lastschrift für
010813*            den Rateneinzug
010813         15  'XX'-MANDREF                        PIC X(035).
010813*            Mandatsreferenz
110116         15  'XX'-PRODUKT-PROV                   pic S9(003)V99.
 
110116         15  'XX'-TARIF                          pic X(002).
240417         15  'XX'-RECHTSSTAND                    pic 9(002).
 
240417*        15  'XX'-RESERVE                        PIC  X(019).
240417         15  'XX'-RESERVE                        PIC  X(017).
      *            RESERVE
 
      ***********************************************************
      *                                                         *
      *          VORSATZ                                        *
      *                                                         *
      ***********************************************************
 
           10  'XX'-VORSATZ REDEFINES 'XX'-SATZ.
      *
               15  'XX'-V-SA                           PIC X(002).
      *            SATZART = '00'
               15  'XX'-V-ABSENDER                     PIC X(008).
      *            ABSENDER = 'DWS'
               15  'XX'-V-DATUM                        PIC 9(008).
      *            Buchungsdatum = 'JJJJMMTT'
               15  'XX'-V-EMPFAENGER                   PIC X(004).
      *            EMPFAENGER = 'DVAG' 'ALLF'
               15  'XX'-V-TEXT                         PIC X(008).
      *            INHALTSANGABE:
      *            Tägliche Lieferung 'PLANT'
      *            Komplettbestand    'PLANK'
 
               15  'XX'-V-WKZ                          PIC X(003).
      *            WÄHRUNGSKENNZEICHEN = 'EUR'
010813         15  FILLER                              PIC X(317).
      *            RESERVE
      ***********************************************************
      *                                                         *
      *          NACHSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-NACHSATZ REDEFINES 'XX'-SATZ.
      *
               15  'XX'-N-SA                           PIC  X(002).
      *            SATZART             '99'
               15  'XX'-N-ANZ                          PIC  9(007).
      *            ANZAHL SAETZE OHNE VOR- UND NACHSATZ
010813         15  FILLER                              PIC  X(341).
      *            RESERVE
 
      *---------------------------------------------------------------*
 
 




