      *----------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.09.2018
      *            Kategorie  :  Copystrecke
      *----------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   DRKI3496.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI3496.CPv  $
      *
      *   Rev 2.6   Nov 22 2018 12:46:26   t13ae52
      *AKI01615: Erm./Aufb./Akt. SPARPLAN
      *
      *   Rev 2.5   Nov 05 2018 16:30:00   t13ex78
      *AKI01615: Erm./Aufb./Akt. SPARPLAN
      *
      *   Rev 2.4   Nov 05 2018 15:57:32   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTBESTAND
      *
      *   Rev 2.3   Oct 31 2018 17:21:22   t13ex78
      *AKI01615: Erm./Aufb./Akt. SPARPLAN
      *
      *   Rev 2.2   Oct 29 2018 12:02:00   t13ex78
      *AKI01615: Erm./Aufb./Akt. SPARPLAN
      *
      *   Rev 2.1   Oct 25 2018 14:50:40   t13ex78
      *AKI01615: Erm./Aufb./Akt. SPARPLAN
      *
      *   Rev 2.0   Sep 13 2018 14:51:50   t13ex78
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *  Copy-Member Ausgabe Ermittlung SPARPLAN G42 Generali Inv.     *
      *                                                                *
      *  Satzlänge -
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01615!17.09.18! Neuerstellung                    !tfl        *
      *--------!-------------------------------------------!-----------*
      *        !        !                                  !           *
      *        !        !                                  !           *
      ******************************************************************
      *
       01  'XX'-SATZ.
      * Verarbeitungskennzeichen
      * I = INSERT, U = UPDATE, D = DELETE
           05  'XX'-VERARB-KZ                         PIC  X(001).
      * Satzart = C
           05  'XX'-SA                                PIC  X(001).
      * Gesellschaft = 48
           05  'XX'-GES                               PIC  9(002).
      * Vertragsnummer mit führenden Nullen und LFDNR Sparplan
           05  'XX'-KEY.
               10  'XX'-VNR                           PIC  9(015).
               10  'XX'-PLAN-NR                       PIC  9(002).
      * Datum der Eröffnung
           05  'XX'-DATUM-EROEFFNUNG.
               10  JAHR-EROEFFNUNG                    PIC  9(004).
               10  MONAT-EROEFFNUNG                   PIC  9(002).
               10  TAG-EROEFFNUNG                     PIC  9(002).
           05  'XX'-DAT-E-KZ                          PIC  X(001).
      * Datum der Schließung
           05  'XX'-DATUM-SCHLIESSUNG.
               10  JAHR-SCHLIESSUNG                   PIC  9(004).
               10  MONAT-SCHLIESSUNG                  PIC  9(002).
               10  TAG-SCHLIESSUNG                    PIC  9(002).
           05  'XX'-DAT-S-KZ                          PIC  X(001).
      * Abweichender Zahler/Kontoinhaber
           05  'XX'-ABW-KONTOINHABER                  PIC  X(030).
           05  'XX'-KTOINH-KZ                         PIC  X(001).
      * Wertpapierkennnummer (WKN)
           05  'XX'-WKN                               PIC  X(006).
      * ISIN
           05  'XX'-ISIN                              PIC  X(012).
      * Sparrate
           05  'XX'-SPARRATE                          PIC  9(007)V99.
           05  'XX'-SPARRATE-KZ                       PIC  X(001).
      * Dynamik
           05  'XX'-DYNAM                             PIC  9(002)V99.
           05  'XX'-DYNAM-KZ                          PIC  X(001).
      * Zahlungsweise
           05  'XX'-ZAHLWEISE                         PIC  X(001).
           05  'XX'-ZW-KZ                             PIC  X(001).
      * Tag der Einzahlung
           05  'XX'-TAG                               PIC  9(002).
           05  'XX'-TAG-KZ                            PIC  X(001).
      * Ende der Einzahlungsfrist bei VL-Verträgen
           05  'XX'-DATUM-VERTRAGSENDE.
               10  JAHR-VERTRAGSENDE                  PIC  9(004).
               10  MONAT-VERTRAGSENDE                 PIC  9(002).
               10  TAG-VERTRAGSENDE                   PIC  9(002).
           05  'XX'-DAT-V-KZ                          PIC  X(001).
      * Vertragstyp
           05  'XX'-VERTRAGSTYP                       PIC  X(002).
           05  'XX'-VERTRTYP-KZ                       PIC  X(001).
      * Währungskennzeichen
           05  'XX'-WKZ                               PIC  X(001).
      * Buchungsdatum aus Vorsatz
           05  'XX'-LETZT-AENDDAT                     PIC  9(008).
      * Beginndatum der Ratenaussetzung
           05  'XX'-BEGINN-RATENAUSSETZUNG            PIC  9(008).
           05  'XX'-BEG-RATENAUS-KZ                   PIC  X(001).
      * Endedatum der Ratenaussetzung
           05  'XX'-ENDE-RATENAUSSETZUNG              PIC  9(008).
           05  'XX'-ENDE-RATENAUS-KZ                  PIC  X(001).
      * Kosten in %
           05  'XX'-KOSTEN-PROZ                       PIC  9(01)V99.
           05  'XX'-KOSTEN-PROZ-KZ                    PIC  X(01).
      * Ursprüngliche Gesamtsparleistung in EUR
           05  'XX'-URSPR-GES-SPAR-LEIST              PIC  9(06)V99.
           05  'XX'-URSPR-GES-SPAR-LE-KZ              PIC  X(01).
      * Ursprüngliche Sparrate
           05  'XX'-URSPR-SPAR-RATE                   PIC  9(04)V99.
           05  'XX'-URSPR-SPAR-RATE-KZ                PIC  X(01).
      * Ausgabeaufschlag des ursprünglichen Fonds
      *    05  'XX'-AUSG-AUFSCHL-URSPR                PIC  9(02)V99.
      *    05  'XX'-AUSG-AUFSCHL-URSP-KZ              PIC  X(01).
           05  FILLER                                 PIC  X(05).
      * Ausgabeaufschlag des aktuellen Fonds
           05  'XX'-AUSG-AUFSCHL-AKTUELL              PIC  9(02)V99.
           05  'XX'-AUSG-AUFSCHL-AKTU-KZ              PIC  X(01).
      * Vertragslaufzeit
           05  'XX'-VERTR-LAUFZEIT                    PIC  9(02).
           05  'XX'-VERTR-LAUFZEIT-KZ                 PIC  X(01).
      * Beginn Dynamisierung
           05  'XX'-BEG-DYNAM                         PIC  9(08).
           05  'XX'-BEG-DYNAM-KZ                      PIC  X(01).
      * Zähler Lastschriftrückgaben
           05  'XX'-ZAEHLER-LASTSCHR-RUECK            PIC  9(02).
           05  'XX'-ZAEHLER-LASTSCHR-KZ               PIC  X(01).
      * Kündigungsmerkmal = 'K' bzw. ' '
           05  'XX'-KND-MERKMAL                       PIC  X(01).
           05  'XX'-KND-MERKMAL-KZ                    PIC  X(01).
      * Datum Vertragsauflösung  bzw. vorzeitige Vertragskündigung
           05  'XX'-DAT-VERTR-AUFLOESUNG              PIC  9(08).
           05  'XX'-DAT-VERTR-AUFLOES-KZ              PIC  X(01).
      * Bonifikation
           05  'XX'-BONIFIKATION                      PIC  9(003)V9(04).
           05  'XX'-BONIFIKATION-KZ                   PIC  X(01).
      * IBAN zum Einzug der Sparrate
           05  'XX'-IBAN                              PIC  X(034).
           05  'XX'-IBAN-KZ                           PIC  X(001).
      * BIC zum Einzug der Sparrate
           05  'XX'-BIC                               PIC  X(011).
           05  'XX'-BIC-KZ                            PIC  X(001).
      * Mandatsreferenz der SEPA-Lastschrift
           05  'XX'-MANDREF                           PIC  X(035).
           05  'XX'-MANDREF-KZ                        PIC  X(001).
      *
           05  'XX'-RESERVE                           PIC  X(069).
 
 
 
 
 

