      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1557.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1557.CPv  $
      *
      *   Rev 2.1   Jul 10 2003 18:34:24   t13ex12
      *AKI00354  Preise von IT-Future
      *
      *   Rev 2.0   May 19 2003 09:59:06   p13sp14
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR RUECKNAHMEPREISE VON IT-Future (DTA)        *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEINAME = KI086400                                        *
      *    LAENGE    = 80 BYTES
      *                                                                *
      *    Die Datei kommt TÄGLICH von IT-Future für Fremdfonds von DB.*
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *12.06.03!NEUERSTELLUNG                           !v.d.Goltz! 01 *
      *AKI00354!                                        !         !    *
      * -------------------------------------------------------------*
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *
CCC009*    SKIP1
       01  DRKI1557.
           10  ITF-VORSATZ.
      *
               20  ITF-V-SA            PIC X(001).
      *            SATZART
               20  ITF-V-ABSENDER      PIC X(008).
      *            ABSENDER
               20  ITF-V-DATUM         PIC 9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  ITF-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER
               20  ITF-V-TEXT          PIC X(008).
      *            INHALTSANGABE
               20  FILLER              PIC X(051).

      *
           10  ITF-SATZ     REDEFINES      ITF-VORSATZ.
      *
               20  ITF-SA                  PIC  X(001).
      *            SATZART
               20  ITF-GES                 PIC  9(003).
      *            GESELLSCHAFT
               20  ITF-WP-KENN-NR          PIC  X(006).
      *            WERTPAPIERKENN-NUMMER
               20  ITF-ISIN-NR             PIC  X(012).
      *            DATUM WERTENTWICKLUNG
      *        20  ITF-RUECK-PREIS         PIC  9(007)V99.
               20  ITF-RUECK-PREIS         PIC  X(009).
      *            RUECKNAHMEPREIS
               20  ITF-AUS-PREIS           PIC  X(009).
      *            AUSGABEPREIS
               20  ITF-PREIS-DAT           PIC  9(008).
      *            DATUM Preise
               20  ITF-WERT-ENTW           PIC  X(009).
      *            WERTENTWICKLUNG
               20  ITF-WERT-ENTW-DAT       PIC  9(008).
      *            DATUM WERTENTWICKLUNG
               20  ITF-WKZ                 PIC  X(003).
               20  FILLER                  PIC  X(012).

      *
           10  ITF-NACHSATZ    REDEFINES     ITF-VORSATZ.
      *
               20  ITF-N-SA             PIC X(001).
      *            SATZART             'E'
160802         20  ITF-N-ANZ            PIC X(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
160802         20  FILLER               PIC X(072).
      *            RESERVE


