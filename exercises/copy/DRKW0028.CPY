      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  28.12.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0028.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKW0028.CPv  $
      *
      *   Rev 2.1   Jan 06 2021 10:19:18   u130685
      *AKI01821: DVAG Wien Sparplan
      *
      *   Rev 2.0   Dec 28 2020 10:53:52   u130685
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER EINGABE FÜR DVAG WIEN SPARPLÄNE (G33_SPARPLAN)  *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    LÄNGE = 400                                                 *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01821!22.12.20!1.0 !NEU FUER DVAG WIEN SPARPLAN! U130685  *
      * -------------------------------------------------------------*
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
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
               15  'XX'-V-ABSENDER      PIC  X(005).
      *            DWIEN'
               15  'XX'-V-DATUM         PIC  9(008).
      *            DATUM = 'JJJJMMTT'
               15  'XX'-V-EMPFAENGER    PIC  X(004).
      *            EMPFAENGER = 'DVAG'
               15  'XX'-V-TEXT          PIC  X(025).
      *            'SPARPLANDATEN'
               15  'XX'-V-ART           PIC  X(004).
      *            'INIT'
               15  FILLER               PIC  X(353).
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
      *            GESELLSCHAFT   = '33'
               15  'XX'-KEY.
                   20  'XX'-VNR                   PIC  9(015).
                   20  'XX'-LFDNR                 PIC  9(002).
 
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
      *               TAG IM MONAT FÜR DIE EINZAHLUNG
               15  'XX'-DATUM-VERTRAGSENDE.
                   20  JAHR-VERTRAGSENDE          PIC  9(004).
                   20  MONAT-VERTRAGSENDE         PIC  9(002).
                   20  TAG-VERTRAGSENDE           PIC  9(002).
      *               ENDE DER EINZAHLUNGSFRIST BEI VL-VERTRÄGEN
               15  'XX'-VERTRAGSTYP               PIC  X(002).
      *               VERTRAGSTYP......0 : WENN EINMALANLAGE
      *               VERTRAGSTYP......1 : WENN VL
      *               VERTRAGSTYP......2 : WENN SPARPLAN
      *               VERTRAGSTYP......3 : ENTNAHMEPLAN
      *               VERTRAGSTYP......4 : LEBENSPHASENMODELL
      *               VERTRAGSTYP......5 : VERMÖGENSSPARPLAN
               15  'XX'-BEGINN-RATENAUSSETZUNG    PIC  9(008).
      *               BEGINNDATUM DER RATENAUSSETZUNG
               15  'XX'-ENDE-RATENAUSSETZUNG      PIC  9(008).
      *               ENDEDATUM DER RATENAUSSETZUNG
               15  'XX'-KOSTEN-PROZ               PIC 9(01)V99.
      *               KOSTEN IN %
               15  'XX'-URSPR-GES-SPAR-LEIST      PIC 9(06)V99.
      *               URSPRÜNGLICHE GESAMTSPARLEISTUNG IN EUR
               15  'XX'-URSPR-SPAR-RATE           PIC 9(04)V99.
      *               URSPRÜNGLICHE SPARRATE
               15  'XX'-AUSG-AUFSCHL-URSPR        PIC 9(02)V99.
      *         15  FILLER                         PIC 9(04).
      *               AUSGABEAUFSCHLAG DES URSPRÜNGLICHEN FONDS
               15  'XX'-AUSG-AUFSCHL-AKTUELL      PIC 9(02)V99.
      *               AUSGABEAUFSCHLAG DES AKTUELLEN FONDS
               15  'XX'-VERTR-LAUFZEIT            PIC 9(02).
      *               VERTRAGSLAUFZEIT
               15  'XX'-BEG-DYNAM                 PIC 9(08).
      *               BEGINN DYNAMISIERUNG
               15  'XX'-ZAEHLER-LASTSCHR-RUECK    PIC 9(02).
      *               ZÄHLER LASTSCHRIFTRÜCKGABEN
               15  'XX'-KND-MERKMAL               PIC X(01).
      *               KÜNDIGUNGSMERKMAL = 'K', 'U' ODER ' '
               15  'XX'-DAT-VERTR-AUFLOESUNG      PIC 9(08).
      *               DATUM VERTRAGSAUFLÖSUNG  BZW. VORZEITIGE
      *               VERTRAGSKÜNDIGUNG
               15  'XX'-BONIFIKATION              PIC  9(003)V9(04).
               15  'XX'-IBAN                      PIC  X(034).
      *            IBAN ZUR LASTSCHRIFT FÜR DEN RATENEINZUG

               15  'XX'-BIC                       PIC  X(011).
      *            BIC ZUR LASTSCHRIFT FÜR DEN RATENEINZUG

               15  'XX'-MANDREF                   PIC  X(035).
      *            MANDATSREFERENZ

               15  'XX'-ABRKTO                    PIC  X(020).
      *            ABRECHNUNGSKONTO

               15  'XX'-PIN                       PIC  9(010).
      *            PIN
               15  'XX'-RESERVE                   PIC  X(111).
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
               15  'XX'-N-ANZ              PIC  9(009).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               15  FILLER                  PIC  X(390).
      *            RESERVE
