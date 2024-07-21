      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  28.12.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0029.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKW0029.CPv  $
      *
      *   Rev 2.1   Jan 06 2021 10:19:34   u130685
      *AKI01821: DVAG Wien Sparplan
      *
      *   Rev 2.0   Dec 28 2020 10:54:06   u130685
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *  COPY-MEMBER AUSGABE ERMITTLUNG SPARPLAN G33 DVAG WIEN         *
      *                                                                *
      *  SATZLÄNGE 400
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !DATUM   !TEXT                              !NAME       *
      *--------!-------------------------------------------!-----------*
      *AKI01821!22.12.20!NEU FUER DVAG WIEN SPARPLAN       ! U130685   *
      *--------!-------------------------------------------!-----------*
      *        !        !                                  !           *
      *        !        !                                  !           *
      ******************************************************************
      *
       01  'XX'-SATZ.
      * VERARBEITUNGSKENNZEICHEN
      * I = INSERT, U = UPDATE, D = DELETE
           05  'XX'-VERARB-KZ                         PIC  X(001).
      * GESELLSCHAFT = 33
           05  'XX'-GES                               PIC  9(002).
      * VERTRAGSNUMMER MIT FÜHRENDEN NULLEN UND LFDNR SPARPLAN
           05  'XX'-KEY.
               10  'XX'-VNR                           PIC  9(015).
               10  'XX'-LFDNR                         PIC  9(002).
      * DATUM DER ERÖFFNUNG
           05  'XX'-DATUM-EROEFFNUNG.
               10  JAHR-EROEFFNUNG                    PIC  9(004).
               10  MONAT-EROEFFNUNG                   PIC  9(002).
               10  TAG-EROEFFNUNG                     PIC  9(002).
           05  'XX'-DAT-E-KZ                          PIC  X(001).
      * DATUM DER SCHLIESSUNG
           05  'XX'-DATUM-SCHLIESSUNG.
               10  JAHR-SCHLIESSUNG                   PIC  9(004).
               10  MONAT-SCHLIESSUNG                  PIC  9(002).
               10  TAG-SCHLIESSUNG                    PIC  9(002).
           05  'XX'-DAT-S-KZ                          PIC  X(001).
      * ABWEICHENDER ZAHLER/KONTOINHABER
           05  'XX'-ABW-KONTOINHABER                  PIC  X(030).
           05  'XX'-ABW-KTOINH-KZ                     PIC  X(001).
      * WERTPAPIERKENNNUMMER (WKN)
           05  'XX'-WKN                               PIC  X(006).
      * ISIN
           05  'XX'-ISIN                              PIC  X(012).
           05  'XX'-ISIN-KZ                           PIC  X(012).
      * SPARRATE
           05  'XX'-SPARRATE                          PIC  9(007)V99.
           05  'XX'-SPARRATE-KZ                       PIC  X(001).
      * DYNAMIK
           05  'XX'-DYNAM                             PIC  9(002)V99.
           05  'XX'-DYNAM-KZ                          PIC  X(001).
      * ZAHLUNGSWEISE
           05  'XX'-ZAHLWEISE                         PIC  9(001).
           05  'XX'-ZW-KZ                             PIC  X(001).
      * TAG DER EINZAHLUNG
           05  'XX'-TAG                               PIC  9(002).
           05  'XX'-TAG-KZ                            PIC  X(001).
      * ENDE DER EINZAHLUNGSFRIST BEI VL-VERTRÄGEN
           05  'XX'-DATUM-VERTRAGSENDE.
               10  JAHR-VERTRAGSENDE                  PIC  9(004).
               10  MONAT-VERTRAGSENDE                 PIC  9(002).
               10  TAG-VERTRAGSENDE                   PIC  9(002).
           05  'XX'-DAT-V-KZ                          PIC  X(001).
      * VERTRAGSTYP
           05  'XX'-VERTRAGSTYP                       PIC  X(002).
           05  'XX'-VERTRTYP-KZ                       PIC  X(001).

      * BEGINNDATUM DER RATENAUSSETZUNG
           05  'XX'-BEGINN-RATENAUSSETZUNG            PIC  X(008).
           05  'XX'-BEG-RATENAUS-KZ                   PIC  X(001).
      * ENDEDATUM DER RATENAUSSETZUNG
           05  'XX'-ENDE-RATENAUSSETZUNG              PIC  X(008).
           05  'XX'-ENDE-RATENAUS-KZ                  PIC  X(001).
      * KOSTEN IN %
           05  'XX'-KOSTEN-PROZ                       PIC  9(01)V99.
           05  'XX'-KOSTEN-PROZ-KZ                    PIC  X(01).
      * URSPRÜNGLICHE GESAMTSPARLEISTUNG IN EUR
           05  'XX'-URSPR-GES-SPAR-LEIST              PIC  9(06)V99.
           05  'XX'-URSPR-GES-SPAR-LE-KZ              PIC  X(01).
      * URSPRÜNGLICHE SPARRATE
           05  'XX'-URSPR-SPAR-RATE                   PIC  9(04)V99.
           05  'XX'-URSPR-SPAR-RATE-KZ                PIC  X(01).
      * AUSGABEAUFSCHLAG DES URSPRÜNGLICHEN FONDS
          05  'XX'-AUSG-AUFSCHL-URSPR                PIC  9(02)V99.
          05  'XX'-AUSG-AUFSCHL-URSP-KZ              PIC  X(01).
      *     05  FILLER                                 PIC  X(05).
      * AUSGABEAUFSCHLAG DES AKTUELLEN FONDS
           05  'XX'-AUSG-AUFSCHL-AKTUELL              PIC  9(02)V99.
           05  'XX'-AUSG-AUFSCHL-AKTU-KZ              PIC  X(01).
      * VERTRAGSLAUFZEIT
           05  'XX'-VERTR-LAUFZEIT                    PIC  9(02).
           05  'XX'-VERTR-LAUFZEIT-KZ                 PIC  X(01).
      * BEGINN DYNAMISIERUNG
           05  'XX'-BEG-DYNAM                         PIC  9(08).
           05  'XX'-BEG-DYNAM-KZ                      PIC  X(01).
      * ZÄHLER LASTSCHRIFTRÜCKGABEN
           05  'XX'-ZAEHLER-LASTSCHR-RUECK            PIC  9(02).
           05  'XX'-ZAEHLER-LASTSCHR-KZ               PIC  X(01).
      * KÜNDIGUNGSMERKMAL = 'K' BZW. ' '
           05  'XX'-KND-MERKMAL                       PIC  X(01).
           05  'XX'-KND-MERKMAL-KZ                    PIC  X(01).
      * DATUM VERTRAGSAUFLÖSUNG  BZW. VORZEITIGE VERTRAGSKÜNDIGUNG
           05  'XX'-DAT-VERTR-AUFLOESUNG              PIC  9(08).
           05  'XX'-DAT-VERTR-AUFLOES-KZ              PIC  X(01).
      * BONIFIKATION
           05  'XX'-BONIFIKATION                      PIC  9(003)V9(04).
           05  'XX'-BONIFIKATION-KZ                   PIC  X(01).
      * IBAN ZUM EINZUG DER SPARRATE
           05  'XX'-IBAN                              PIC  X(034).
           05  'XX'-IBAN-KZ                           PIC  X(001).
      * BIC ZUM EINZUG DER SPARRATE
           05  'XX'-BIC                               PIC  X(011).
           05  'XX'-BIC-KZ                            PIC  X(001).
      * MANDATSREFERENZ DER SEPA-LASTSCHRIFT
           05  'XX'-MANDREF                           PIC  X(035).
           05  'XX'-MANDREF-KZ                        PIC  X(001).
      * ABRECHNUNGSKONTO
           05  'XX'-ABRKTO                            PIC  X(020).
           05  'XX'-ABRKTO-KZ                         PIC  X(001).
      * PIN
           05  'XX'-PIN                               PIC  9(010).
           05  'XX'-PIN-KZ                            PIC  X(001).
      *
           05  'XX'-RESERVE                           PIC  X(072).
