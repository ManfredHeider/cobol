      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  21.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3383.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI3383.CPv  $
      *
      *   Rev 2.6   Nov 22 2018 12:46:34   t13ae52
      *AKI01615: Erm./Aufb./Akt. SPARPLAN
      *
      *   Rev 2.5   Nov 05 2018 16:30:10   t13ex78
      *AKI01615: Erm./Aufb./Akt. SPARPLAN
      *
      *   Rev 2.4   Nov 05 2018 15:57:42   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTBESTAND
      *
      *   Rev 2.3   Oct 31 2018 17:21:32   t13ex78
      *AKI01615: Erm./Aufb./Akt. SPARPLAN
      *
      *   Rev 2.2   Oct 29 2018 12:02:14   t13ex78
      *AKI01615: Erm./Aufb./Akt. SPARPLAN
      *
      *   Rev 2.1   Oct 25 2018 14:50:50   t13ex78
      *AKI01615: Erm./Aufb./Akt. SPARPLAN
      *
      *   Rev 2.0   Aug 21 2018 16:23:14   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für SPARPLÄNE             VON Generali Invest *
      *                                                                *
      *    SATZLÄNGE   300                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01615!22.08.18! NEU                              ! WHO       *
      *--------!-------------------------------------------!-----------*
      *DBO-8960!30.08.21! Neues Attribut SP-STATUS         ! ATAR      *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
CCC009*    SKIP1
         05  'XX'-DTA-SATZ.
 
      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************
 
           10  'XX'-VORSATZ.
      *
               15  'XX'-V-SA            PIC  X(001).
      *            SATZART = 'A'
               15  'XX'-V-ABSENDER      PIC  X(027).
      *            ABSENDER = 'GEI'
               15  'XX'-V-DAT-STAND     PIC  9(008).
      *            Datum = 'TTMMJJJJ' Stand
               15  'XX'-V-DATUM         PIC  9(008).
      *            Datum = 'TTMMJJJJ' Erstellung
               15  'XX'-V-EMPFAENGER    PIC  X(027).
      *            EMPFAENGER = 'ALAG'
               15  'XX'-V-TEXT          PIC  X(020).
      *            'SPARPLANDATEN'
               15  'XX'-V-ART           PIC  X(001).
      *            K = Komplettbestand, D für Delta
               15  FILLER               PIC  X(208).
      *            RESERVE
      ***********************************************************
      *                                                         *
      *         HAUPTSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-SATZ     REDEFINES      'XX'-VORSATZ.
      *
               15  'XX'-SA                        PIC  X(001).
      *            SATZART        = 'C'
               15  'XX'-GES                       PIC  9(002).
      *            GESELLSCHAFT   = '42'
               15  'XX'-KEY.
                   20  'XX'-VNR                   PIC  9(015).
                   20  'XX'-PLAN-NR               PIC  9(002).
 
               15  'XX'-DATUM-EROEFFNUNG.
                   20  JAHR-EROEFFNUNG            PIC  9(004).
                   20  MONAT-EROEFFNUNG           PIC  9(002).
                   20  TAG-EROEFFNUNG             PIC  9(002).
               15  'XX'-DATUM-SCHLIESSUNG.
                   20  JAHR-SCHLIESSUNG           PIC  9(004).
                   20  MONAT-SCHLIESSUNG          PIC  9(002).
                   20  TAG-SCHLIESSUNG            PIC  9(002).
               15  'XX'-ABW-KONTOINHABER          PIC  X(030).
               15  'XX'-WKN                       PIC  X(006).
      *               WERTPAPIERKENN-NUMMER
               15  'XX'-ISIN                      PIC  X(012).
               15  'XX'-SPARRATE                  PIC  9(007)V99.
      *               SPARLEISTUNG FUER SPARPLAENE
               15  'XX'-DYNAM                     PIC  9(002)V99.
      *               DYNAMISIERUNG FUER SPARPLAENE
               15  'XX'-ZAHLWEISE                 PIC  X(001).
      *               ZAHLWEISE......***0 : ZAHLWEISE FEHLT
      *               ZAHLWEISE........*1 : JAEHRLICH
      *               ZAHLWEISE........*2 : HALBJAEHRLICH
      *               ZAHLWEISE........*4 : VIERTELJAEHRLICH
      *               ZAHLWEISE........*5 : 2-MONATLICH
      *               ZAHLWEISE........*6 : MONATLICH
      *               ZAHLWEISE.........8 : EINMALPRAEMIE
               15  'XX'-TAG                       PIC  9(002).
      *               Tag im Monat für die Einzahlung
               15  'XX'-DATUM-VERTRAGSENDE.
                   20  JAHR-VERTRAGSENDE          PIC  9(004).
                   20  MONAT-VERTRAGSENDE         PIC  9(002).
                   20  TAG-VERTRAGSENDE           PIC  9(002).
      *               Ende der Einzahlungsfrist bei VL-Verträgen
               15  'XX'-VERTRAGSTYP               PIC  X(002).
      *               Vertragstyp......0 : wenn Einmalanlage
      *               Vertragstyp......1 : wenn VL
      *               Vertragstyp......2 : wenn Sparplan
      *               Vertragstyp......3 : Entnahmeplan
      *               Vertragstyp......4 : Lebensphasenmodell
      *               Vertragstyp......5 : VermögensSparplan
               15  'XX'-BEGINN-RATENAUSSETZUNG    PIC  9(008).
      *               Beginndatum der Ratenaussetzung
               15  'XX'-ENDE-RATENAUSSETZUNG      PIC  9(008).
      *               Endedatum der Ratenaussetzung
               15  'XX'-KOSTEN-PROZ               PIC 9(01)V99.
      *               Kosten in %
               15  'XX'-URSPR-GES-SPAR-LEIST      PIC 9(06)V99.
      *               Ursprüngliche Gesamtsparleistung in EUR
               15  'XX'-URSPR-SPAR-RATE           PIC 9(04)V99.
      *               Ursprüngliche Sparrate
      *        15  'XX'-AUSG-AUFSCHL-URSPR        PIC 9(02)V99.
               15  FILLER                         PIC 9(04).
      *               Ausgabeaufschlag des ursprünglichen Fonds
               15  'XX'-AUSG-AUFSCHL-AKTUELL      PIC 9(02)V99.
      *               Ausgabeaufschlag des aktuellen Fonds
               15  'XX'-VERTR-LAUFZEIT            PIC 9(02).
      *               Vertragslaufzeit
               15  'XX'-BEG-DYNAM                 PIC 9(08).
      *               Beginn Dynamisierung
               15  'XX'-ZAEHLER-LASTSCHR-RUECK    PIC 9(02).
      *               Zähler Lastschriftrückgaben
               15  'XX'-KND-MERKMAL               PIC X(01).
      *               Kündigungsmerkmal = 'K', 'U' oder ' '
               15  'XX'-DAT-VERTR-AUFLOESUNG      PIC 9(08).
      *               Datum Vertragsauflösung  bzw. vorzeitige
      *               Vertragskündigung
               15  'XX'-BONIFIKATION              PIC  9(003)V9(04).
               15  'XX'-IBAN                      PIC  X(034).
      *            IBAN zur Lastschrift für den Rateneinzug
 
               15  'XX'-BIC                       PIC  X(011).
      *            BIC zur Lastschrift für den Rateneinzug
 
               15  'XX'-MANDREF                   PIC  X(035).
      *            Mandatsreferenz
 
               15  'XX'-SP-STATUS                 PIC  X(001).
      *            Sparplan Status         
 
               15  'XX'-RESERVE                   PIC  X(040).
      *            RESERVE
      ***********************************************************
      *                                                         *
      *          NACHSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-NACHSATZ    REDEFINES     'XX'-VORSATZ.
      *
               15  'XX'-N-SA               PIC  X(001).
      *            SATZART             'E'
               15  'XX'-N-ANZ              PIC  9(007) COMP-3.
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               15  FILLER                  PIC  X(295).
      *            RESERVE
 
 
 
 
 

