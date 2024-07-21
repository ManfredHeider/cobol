      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2959.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2959.CPv  $
      *
      *   Rev 2.3   Mar 13 2018 08:20:42   t13ex27
      *AKI01572: FW-Gesellschaft DV(G90)
      *
      *   Rev 2.2   Sep 02 2015 15:01:46   t13ex73
      *AKI01342: DVM G90 Erstellung - MDTA Copystrecken
      *
      *   Rev 2.1   Jul 09 2015 10:37:58   t13ex73
      *AKI01342: DVM G90 Erstellung - MDTA Copystrecken
      *
      *   Rev 2.0   Jan 19 2015 09:28:16   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 9 5 9    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER G90 DV Vertragdaten DTA                      *
      *                                                                *
      *                                                                *
      *    SATZLAENGE  500 Bytes                                       *
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
       01  DRKI2959.
      *
      *    A fuer Vorsatz, D fuer Datensatz und E fuer Nachsatz
           05  'XX'-SA                      PIC  X(001).
               88 'XX'-SATZART-VORSATZ          VALUE 'A'.
               88 'XX'-SATZART-DATENSATZ        VALUE 'D'.
               88 'XX'-SATZART-ENDESATZ         VALUE 'E'.
      *
           05  'XX'-DATENSATZ.
      *
      *        Die Gesellschaftsnummer, sollte immer die 90 sein
               10 'XX'-D-GES                PIC  9(002).
      *
      *        Die Vertragsnummer mit fuehrenden Nullen
               10 'XX'-D-VNR                PIC  9(015).
      *
      *        Die VB-Nummer des Abschlussvermittlers
               10 'XX'-D-VB-NR              PIC  9(007).
      *
      *        Produktart, 1 = Debitoren- und Forderungsmanagement
      *                    2 = Factoring *
               10 'XX'-D-PRODUKT-ART        PIC  9(001).
      *
      *        DV  Abrechnungsgebuehrt als Basispunkt (%)
               10 'XX'-D-ABRECH-GEBUEHR     PIC  9(001)V99.
      *
      *        DV  Factoringgebuehr als Basispunkt (%)
               10 'XX'-D-FACTOR-GEBUEHR     PIC  9(001)V99.
      *
      *        DV Factoring Ankaufshöhe
               10 'XX'-D-FACTOR-ANK-BETR    PIC  9(010)V99.
      *
      *        Vertragsbeginn- und Endedatum im Format JHJJMMTT
               10 'XX'-D-BEGINN-DATUM.
                  15 'XX'-D-BEGINN-DATUM-JHJJ   PIC 9(004).
                  15 'XX'-D-BEGINN-DATUM-MM     PIC 9(002).
                  15 'XX'-D-BEGINN-DATUM-TT     PIC 9(002).
               10 'XX'-D-ENDE-DATUM.
                  15 'XX'-D-ENDE-DATUM-JHJJ     PIC 9(004).
                  15 'XX'-D-ENDE-DATUM-MM       PIC 9(002).
                  15 'XX'-D-ENDE-DATUM-TT       PIC 9(002).
      *
      *        DV -Einrichtungsgebuehr in Eurocentbetrag
               10 'XX'-D-EIN-GEBUEHR-BTRG   PIC  9(008)V99.
      *
      *        SEPA-Verbindungsdaten
               10 'XX'-D-IBAN               PIC  X(034).
               10 'XX'-D-MANDATS-REFNR      PIC  X(035).
      *
      *        Daten zum abweichenden Kontoinhaber
      *        Die felder nach dem Kennzeichen sind nur gefullt wenn
      *        ein abweichender Kontoinhaber vorhanden ist.
               10 'XX'-D-ABW-BZ-KZ          PIC  X(001).
                  88 'XX'-D-ABW-BZ-KZ-J         VALUE 'J'.
                  88 'XX'-D-ABW-BZ-KZ-N         VALUE 'N'.
               10 'XX'-D-ABW-VORNAME        PIC  X(030).
               10 'XX'-D-ABW-NACHNAME       PIC  X(030).
               10 'XX'-D-ABW-STRASSE        PIC  X(040).
               10 'XX'-D-ABW-PLZ            PIC  9(009).
               10 'XX'-D-ABW-ORT            PIC  X(030).
      *
      *        InfoSperre Kennzeichen
               10 'XX'-D-INFOSPERRE-KZ    PIC  X(001).
                  88 'XX'-D-INFOSPERRE-KZ-J         VALUE 'J'.
                  88 'XX'-D-INFOSPERRE-KZ-N         VALUE 'N'.
      *
      *        DV -Prognose, Prognose fuer die moegliche Provision
      *        die der VB in dem mitgelieferten Zeitraum bekommt.
      *        Geiefert wird der Zeitraum sowie die Anzahl der
      *        eingereichten Rechnungen sowie deren Umsatzsumme.
               10 'XX'-D-PROG-VON-DATUM.
                  15 'XX'-D-PROG-VON-DATUM-JHJJ PIC 9(004).
                  15 'XX'-D-PROG-VON-DATUM-MM   PIC 9(002).
                  15 'XX'-D-PROG-VON-DATUM-TT   PIC 9(002).
               10 'XX'-D-PROG-BIS-DATUM.
                  15 'XX'-D-PROG-BIS-DATUM-JHJJ PIC 9(004).
                  15 'XX'-D-PROG-BIS-DATUM-MM   PIC 9(002).
                  15 'XX'-D-PROG-BIS-DATUM-TT   PIC 9(002).
               10 'XX'-D-PROG-ANZ-RECHNUNGEN    PIC 9(008).
               10 'XX'-D-PROG-UMSATZ            PIC 9(008)V99.
 
               10 FILLER                    PIC  X(186).
      *
           05  'XX'-VORSATZ REDEFINES 'XX'-DATENSATZ.
      *
      *        Hier steht der Absender, es sollte immer DV  sein
               10 'XX'-V-ABSENDER           PIC  X(003).
      *
      *        Erstelldatum des DTAs im Format JHJJMMTT
               10 'XX'-V-DATUM.
                  15 'XX'-V-DATUM-JHJJ      PIC  9(004).
                  15 'XX'-V-DATUM-MM        PIC  9(002).
                  15 'XX'-V-DATUM-TT        PIC  9(002).
      *
      *        Der empfangender Mandant DVAG oder ALLF
               10 'XX'-V-EMPFAENGER         PIC  X(010).
                  88 'XX'-V-EMPFAENGER-DVAG     VALUE 'DVAG'.
                  88 'XX'-V-EMPFAENGER-ALLF     VALUE 'ALLF'.
      *
      *        Der Inhalt des DTAs, in diesem Fall "VERTRAGSDATEN"
               10 'XX'-V-TEXT               PIC  X(020).
      *
      *        Die ART der Lieferung, Aenderungs- oder Komplettbestand
               10 'XX'-V-LIEFERUNG          PIC  X(001).
                  88 'XX'-V-LIEFERUNG-DELTA     VALUE 'D'.
                  88 'XX'-V-LIEFERUNG-KOMPL     VALUE 'K'.
      *
      *
           05  'XX'-NACHSATZ REDEFINES 'XX'-DATENSATZ.
               10 'XX'-N-ANZ                PIC  9(008).
      *
      ******************************************************************


