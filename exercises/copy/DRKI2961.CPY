      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2961.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2961.CPv  $
      *
      *   Rev 2.2   Sep 02 2015 15:10:12   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:46:08   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Jan 28 2015 11:39:22   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 9 6 1    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER G90 DVM Aenderungsdaten Vertrag              *
      *                                                                *
      *    SATZLAENGE  520 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      AENDERUNG                        !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *01.02.15!NEUERSTELLUNG                          !DDY      ! 01  *
      *--------!---------------------------------------!---------!-----*
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
      *    Verarbeitungskennzeichen  I = Insert U = Update
           05 'XX'-VERARB-KZ                    PIC  X(001).
      *        Die Gesellschaftsnummer, sollte immer die 90 sein
           05 'XX'-GES                        PIC  9(002).
      *
      *    Die Vertragsnummer mit fuehrenden Nullen
           05 'XX'-VNR                        PIC  9(015).
      *
      *    Die VB-Nummer des Abschlussvermittlers
           05 'XX'-VB-NR                      PIC  9(007).
           05 'XX'-VB-NR-KZ                   PIC  X(001).
      *
      *    Produktart, 1 = Debitoren- und Forderungsmanagement
      *                    2 = Factoring *
           05 'XX'-PRODUKT-ART                PIC  9(001).
           05 'XX'-PRODUKT-ART-KZ             PIC  X(001).
      *
      *    DVM Abrechnungsgebuehr als Basissatz (%)
           05 'XX'-ABRECH-GEBUEHR             PIC  9(001)V99.
           05 'XX'-ABRECH-GEBUEHR-KZ          PIC  X(001).
      *
      *    DVM Factoringgebuehr als Basissatz (%)
           05 'XX'-FACTOR-GEBUEHR             PIC  9(001)V99.
           05 'XX'-FACTOR-GEBUEHR-KZ          PIC  X(001).
      *
      *    DVM Factoring Ankaufshöhe
           05 'XX'-FACTOR-ANK-BETR            PIC  9(010)V99.
           05 'XX'-FACTOR-ANK-BETR-KZ         PIC  X(001).
      *
      *    Vertragsbeginn- und Endedatum im Format JHJJMMTT
           05 'XX'-BEGINN-DATUM.
              10 'XX'-BEGINN-DATUM-JHJJ       PIC 9(004).
              10 'XX'-BEGINN-DATUM-MM         PIC 9(002).
              10 'XX'-BEGINN-DATUM-TT         PIC 9(002).
           05 'XX'-BEGINN-DATUM-KZ            PIC X(001).
           05 'XX'-ENDE-DATUM.
              10 'XX'-ENDE-DATUM-JHJJ         PIC 9(004).
              10 'XX'-ENDE-DATUM-MM           PIC 9(002).
              10 'XX'-ENDE-DATUM-TT           PIC 9(002).
           05 'XX'-ENDE-DATUM-KZ              PIC X(001).
      *
      *    DVM-Einrichtungsgebuehr in Eurocentbetrag
           05 'XX'-EIN-GEBUEHR-BTRG           PIC  9(008)V99.
           05 'XX'-EIN-GEBUEHR-BTRG-KZ        PIC  X(001).
      *
      *    SEPA-Verbindungsdaten
           05 'XX'-IBAN                       PIC  X(034).
           05 'XX'-IBAN-KZ                    PIC  X(001).
           05 'XX'-MANDATS-REFNR              PIC  X(035).
           05 'XX'-MANDATS-REFNR-KZ           PIC  X(001).
      *
      *    Daten zum abweichenden Kontoinhaber
      *    Die felder nach dem Kennzeichen sind nur gefullt wenn
      *    ein abweichender Kontoinhaber vorhanden ist.
           05 'XX'-ABW-BZ-KZ                  PIC  X(001).
              88 'XX'-ABW-BZ-KZ-J             VALUE 'J'.
              88 'XX'-ABW-BZ-KZ-N             VALUE 'N'.
           05 'XX'-ABW-BZ-KZ-KZ               PIC  X(001).
           05 'XX'-ABW-VORNAME                PIC  X(030).
           05 'XX'-ABW-VORNAME-KZ             PIC  X(001).
           05 'XX'-ABW-NACHNAME               PIC  X(030).
           05 'XX'-ABW-NACHNAME-KZ            PIC  X(001).
           05 'XX'-ABW-STRASSE                PIC  X(040).
           05 'XX'-ABW-STRASSE-KZ             PIC  X(001).
           05 'XX'-ABW-PLZ                    PIC  9(009).
           05 'XX'-ABW-PLZ-KZ                 PIC  X(001).
           05 'XX'-ABW-ORT                    PIC  X(030).
           05 'XX'-ABW-ORT-KZ                 PIC  X(001).
      *
      *    Infosperre Kennzeichen
           05 'XX'-INFOSPERRE-KZ              PIC  X(001).
           05 'XX'-INFOSPERRE-KZ-KZ           PIC  X(001).
      *
      *    DVM-Prognose, Prognose fuer die moegliche Provision
      *    die der VB in dem mitgelieferten Zeitraum bekommt.
      *    Geiefert wird der Zeitraum sowie die Anzahl der
      *    eingereichten Rechnungen sowie deren Umsatzsumme.
           05 'XX'-PROG-VON-DATUM.
              10 'XX'-PROG-VON-DATUM-JHJJ     PIC 9(004).
              10 'XX'-PROG-VON-DATUM-MM       PIC 9(002).
              10 'XX'-PROG-VON-DATUM-TT       PIC 9(002).
           05 'XX'-PROG-VON-DATUM-KZ          PIC X(001).
           05 'XX'-PROG-BIS-DATUM.
              10 'XX'-PROG-BIS-DATUM-JHJJ     PIC 9(004).
              10 'XX'-PROG-BIS-DATUM-MM       PIC 9(002).
              10 'XX'-PROG-BIS-DATUM-TT       PIC 9(002).
           05 'XX'-PROG-BIS-DATUM-KZ          PIC X(001).
           05 'XX'-PROG-ANZ-RECHNUNGEN        PIC 9(008).
           05 'XX'-PROG-ANZ-RECHNUNGEN-KZ     PIC X(001).
           05 'XX'-PROG-UMSATZ                PIC 9(008)V99.
           05 'XX'-PROG-UMSATZ-KZ             PIC X(001).
 
           05 FILLER                          PIC  X(185).
      ******************************************************************

