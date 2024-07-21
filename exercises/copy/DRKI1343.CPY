      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1343.CPY  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRKI1343.CPv  $
      *
      *   Rev 2.11   Feb 28 2021 10:44:00   t13ex39
      *AKI01790: Deutsche Bank - SPARPLÄNE KSDS--->TT
      *
      *   Rev 2.10   Sep 02 2013 10:29:48   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Sparpläne
      *
      *   Rev 2.9   Jul 02 2012 10:34:44   t13ex39
      *AKI01149: PIN bei DWS und DeuBa
      *
      *   Rev 2.8   22 Apr 2008 16:58:20   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten ZAHLUNG
      *
      *   Rev 2.7   Jan 16 2006 17:54:50   t13ex39
      *AKI00494: Änderung der Lieferung DeuBa-Sparpläne
      *
      *   Rev 2.3   Jan 19 2005 14:30:08   t13ex39
      *AKI00249: SPARPLÄNE
      *
      *   Rev 2.2   Feb 11 2003 12:05:50   t13ex12
      *FKI00700 Feld für alte ISIN
      *
      *   Rev 2.1   Jul 30 2002 16:48:36   t13ex12
      *AKI00274 Einführung DB24/DWS
      *
      *   Rev 2.0   Dec 14 2001 09:20:56   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                  ###############                               *
      *         #######  D R K I 1 3 4 3  #######                      *
      *                  ###############                               *
      *                                                                *
      *    COPY-MEMBER FÜR AUSGABEDATEI VERäNDRUNGSBESTAND             *
      *          Investmentsparpläne von DB24                          *
      *                                                                *
      *    LAENGE =  300 BYTES       ERSTELLT  AM 18.04.02             *
      *    LAENGE =  500 BYTES       ERSTELLT  AM 19.06.13             *
      *                                                                *
      *                                                                *
      *                      DATEILÄNGE = 300                          *
      *                      DATEILÄNGE = 500                          *
      *                      DATEITYP   = QSAM                         *
      *                                                                *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00274!21.03.02! Neuerstellung                    !v.d.Goltz  *
      *--------!-------------------------------------------!-----------*
      *FKI00700!10.02.03! Neues Feld für alte ISIN.        !v.d.Goltz  *
      *--------!-------------------------------------------!-----------*
      *AKI00249!18.10.04! Neue Felder                      !Hornung    *
      *        !        ! Begin/Ende der Ratenaussetzung   !           *
      *        !        !                                  !           *
      *--------!--------!----------------------------------!-----------*
      *AKI00512!27.07.05! zus. Attribute für Vermögens-    !Hornung    *
      *        !        ! sparplan                         !           *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      *AKI00494!11.11.05! neuer Aufbaudes Primary-Keys     !Hornung    *
      *        !        !                                  !           *
      *--------!--------!----------------------------------!-----------*
      *AKI00708!14.04.08! neues Attribut                   ! WHO       *
      *--------!--------!----------------------------------!-----------*
      *AKI01149!11.06.12! PIN verarbeiten                  !WHO        *
      *----------------------------------------------------------------*
      *AKI01204!19.06.13! NEUE FELDER EINGEFÜGT f. SEPA    !WMI        *
      *        !        !   UND DEN FILLER ANGEPASST       !           *
      *        !        !                                  !           *
      *        !        !                                  !           *
      ******************************************************************
      *
       01  DRKI1343.
           05  BNK-SA                     PIC  X(001).
      *          SATZART
           05  BNK-GES                    PIC  9(002).
      *            GESELLSCHAFT
      *    05  BNK-KEY.
>>    *        10  BNK-VNR.
      *            15 FILIALNUMMER        PIC  9(003).
      *            15 KUNDENNUMMER        PIC  9(007).
      *        10  UNTERDEPOTNUMMER       PIC  9(002).
      *        10  WAEHRUNG               PIC  9(003).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
      *        10  BNK-PROD-ID.
      *            15 BNK-PLAN-NR         PIC  9(002).
      *            15 PROD-ID-REST        PIC  X(018).
           05  BNK-KEY.
               10  BNK-VNR.
                   15 BNK-FILIALNUMMER        PIC  9(003).
                   15 BNK-KUNDENNUMMER        PIC  9(007).
               10  BNK-PLAN-NR                PIC  9(002).
               10  BNK-WAEHRUNG               PIC  9(003).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)

           05  BNK-UNTERDEPOTNUMMER       PIC  9(002).

<<         05  BNK-PROD-ID.
               15 PROD-ID-REST            PIC  X(018).

           05  BNK-VERARB-KZ              PIC  X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *                I = INSERT
      *                U = UPDATE
      *                U = DELETE
           05  BNK-DATUM-EROEFFNUNG.
               10  JAHR-EROEFFNUNG        PIC  9(004).
               10  MONAT-EROEFFNUNG       PIC  9(002).
               10  TAG-EROEFFNUNG         PIC  9(002).
           05  BNK-AEND-DAT-E-KZ          PIC  X(001).
      *            J = Änderung Datum Eroeffnung
           05  BNK-DATUM-SCHLIESSUNG.
               10  JAHR-SCHLIESSUNG       PIC  9(004).
               10  MONAT-SCHLIESSUNG      PIC  9(002).
               10  TAG-SCHLIESSUNG        PIC  9(002).
           05  BNK-AEND-DAT-S-KZ          PIC  X(001).
      *            J = Änderung Datum Schliessung
           05  BNK-BANKVERBINDUNG         PIC  X(020).
           05  BNK-AEND-BANKVERB-KZ       PIC  X(001).
      *            J = Änderung Bankverbindung
           05  BNK-BLZ                    PIC  9(011).
           05  BNK-AEND-BLZ-KZ            PIC  X(001).
      *            J = Änderung BLZ
           05  BNK-KONTONUMMER            PIC  9(018).
           05  BNK-AEND-KTONR-KZ          PIC  X(001).
      *            J = Änderung Kontonummer
           05  BNK-ABW-KONTOINHABER       PIC  X(027).
           05  BNK-AEND-KTOINH-KZ         PIC  X(001).
      *            J = Änderung Kontoinhaber
           05  BNK-WKN                    PIC  X(006).
      *               WERTPAPIERKENN-NUMMER
           05  BNK-AEND-WKN-KZ            PIC  X(001).
      *            J = Änderung Wertpapierkennnr.
           05  BNK-ISIN                   PIC  X(012).
           05  BNK-AEND-ISIN-KZ           PIC  X(001).
      *           J = Änderung ISIN
           05  BNK-SPARRATE               PIC  9(007)V99.
      *               SPARLEISTUNG FUER SPARPLAENE
           05  BNK-AEND-SPARRATE-KZ       PIC  X(001).
      *            J = Änderung Sparrate
           05  BNK-DYNAM                  PIC  9(002)V99.
      *               DYNAMISIERUNG FUER SPARPLAENE
           05  BNK-AEND-DYNAM-KZ          PIC  X(001).
      *           J = Änderung Dynamisierung
           05  BNK-ZAHLWEISE              PIC  X(001).
           05  BNK-AEND-ZW-KZ             PIC  X(001).
      *            J = Änderung Zahlweise
           05  BNK-TAG                    PIC  9(002).
      *               Tag im Monat für die Einzahlung
           05  BNK-AEND-TAG-KZ            PIC  X(001).
      *            J = Änderung Einzahlungstag
           05  BNK-DATUM-VERTRAGSENDE.
               10  JAHR-VERTRAGSENDE      PIC  9(004).
               10  MONAT-VERTRAGSENDE     PIC  9(002).
               10  TAG-VERTRAGSENDE       PIC  9(002).
      *               Ende der Einzahlungsfrist bei VL-Verträgen
           05  BNK-AEND-DAT-V-KZ          PIC  X(001).
      *            J = Änderung Datum Ende der Einzahlungsfrist
           05  BNK-VERTRAGSTYP            PIC  X(002).
           05  BNK-AEND-VERTRTYP-KZ       PIC  X(001).
      *            J = Änderung Vrtragstyp

           05  BNK-WKZ                    PIC  X(001).
           05  BNK-LETZT-AENDDAT          PIC  9(008).
      *            Buchungsdatum aus Vorsatz

           05  BNK-ISIN-ALT               PIC  X(012).

           05  BNK-BEGINN-RATENAUSSETZUNG PIC  9(008).
      *            Beginndatum der Ratenaussetzung
           05  BNK-AEND-BEG-RATENAUS-KZ   PIC  X(001).
      *            J = Änderung Beginn Ratenaussetzung
           05  BNK-ENDE-RATENAUSSETZUNG   PIC  9(008).
      *            Endedatum der Ratenaussetzung
           05  BNK-AEND-ENDE-RATENAUS-KZ  PIC  X(001).
      *            J = Änderung Ende Ratenaussetzung
           05  BNK-KOSTEN-PROZ                       PIC 9(01)V99.
>>    *               Kosten in %
           05  BNK-AEND-KOSTEN-PROZ-KZ               PIC X(01).
           05  BNK-URSPR-GES-SPAR-LEIST              PIC 9(06)V99.
      *               Ursprüngliche Gesamtsparleistung in EUR
           05  BNK-AEND-URSPR-GES-SPAR-LE-KZ         PIC X(01).
           05  BNK-URSPR-SPAR-RATE                   PIC 9(04)V99.
      *               Ursprüngliche Sparrate
           05  BNK-AEND-URSPR-SPAR-RATE-KZ           PIC X(01).
           05  BNK-AUSG-AUFSCHL-URSPR                PIC 9(02)V99.
      *               Ausgabeaufschlag des ursprünglichen Fonds
           05  BNK-AEND-AUSG-AUFSCHL-URSP-KZ         PIC X(01).
           05  BNK-AUSG-AUFSCHL-AKTUELL              PIC 9(02)V99.
      *               Ausgabeaufschlag des aktuellen Fonds
           05  BNK-AEND-AUSG-AUFSCHL-AKTU-KZ         PIC X(01).
           05  BNK-VERTR-LAUFZEIT                    PIC 9(02).
      *               Vertragslaufzeit
           05  BNK-AEND-VERTR-LAUFZEIT-KZ            PIC X(01).
           05  BNK-BEG-DYNAM                         PIC 9(08).
      *               Beginn Dynamisierung
           05  BNK-AEND-BEG-DYNAM-KZ                 PIC X(01).
           05  BNK-ZAEHLER-LASTSCHR-RUECK            PIC 9(02).
      *               Zähler Lastschriftrückgaben
           05  BNK-AEND-ZAEHLER-LASTSCHR-KZ          PIC X(01).
           05  BNK-KND-MERKMAL                       PIC X(01).
      *               Kündigungsmerkmal = 'K' bzw. ' '
           05  BNK-AEND-KND-MERKMAL-KZ               PIC X(01).
           05  BNK-DAT-VERTR-AUFLOESUNG              PIC 9(08).
      *               Datum Vertragsauflösung  bzw. vorzeitige
      *               Vertragskündigung
>>         05  BNK-AEND-DAT-VERTR-AUFLOES-KZ         PIC X(01).
      *
           05  BNK-UNR-ALT                           PIC 9(02).
           05  BNK-AEND-UNR-ALT-KZ                   PIC X(01).
           05  BNK-BONIFIKATION                      PIC 9(003)V9(04).
           05  BNK-AEND-BONIFIKATION-KZ              PIC X(01).
           05  BNK-URSPR-VERT-NR                     PIC X(012).
           05  BNK-AEND-URSPR-VERT-NR-KZ             PIC X(01).
           05  BNK-PIN                               PIC 9(09).
           05  BNK-AEND-PIN-KZ                       PIC X(01).
           05  BNK-IBAN                              PIC X(034).
>>    *        IBAN zur Lastschrift für den Rateneinzug

           05  BNK-AEND-IBAN-KZ                      PIC X(001).
      *        J WENN IBAN GEÄNDERT

           05  BNK-BIC                               PIC X(011).
      *        BIC zur Lastschrift für den Rateneinzug

           05  BNK-AEND-BIC-KZ                       PIC X(001).
      *        J WENN BIC GEÄNDERT

           05  BNK-MANDREF                           PIC X(035).
      *        Mandatsreferenz

           05  BNK-AEND-MANDREF-KZ                   PIC X(001).
      *        J WENN MANDATSREFERENZ GEÄNDERT

      *    05  BNK-RESERVE                           PIC  X(100).
    <<*    05  BNK-RESERVE                           PIC  X(090).
           05  BNK-RESERVE                           PIC  X(069).



