      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1418.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/cpy/DRKI1418.CPv  $
      *
      *   Rev 2.2   Sep 10 2002 10:50:36   t13ex12
      *FKI00635 Korrektur im Nachsatz
      *
      *   Rev 2.0   Apr 23 2002 16:01:54   p13sp16
      *Archiverstellung
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR RUECKNAHMEPREISE VON DWS-FONDS (DTA)        *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEINAME = KI086400                                        *
      *    LAENGE    = 60 BYTES
      *                                                                *
      *    DIE DATEI KOMMT VON DWS TÄGLICH, für DWS-Fonds von DB24.    *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *24.05.02!NEUERSTELLUNG                           !v.d.Goltz! 01 *
      *AKI00274!                                        !         !    *
      * -------------------------------------------------------------*
      *16.08.02!Anz. Sätze in Nachsatz 7 statt 9-stellig!v.d.Goltz! 02 *
      *FKI00635!                                        !         !    *
      *        !                                        !         !    *
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *
CCC009*    SKIP1
       01  DRKI1418.
           10  DWS-VORSATZ.
      *
               20  DWS-V-SA            PIC X(001).
      *            SATZART
               20  DWS-V-ABSENDER      PIC X(008).
      *            ABSENDER
               20  DWS-V-DATUM         PIC 9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  DWS-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER
               20  DWS-V-TEXT          PIC X(008).
      *            INHALTSANGABE
               20  FILLER              PIC X(051).

      *
           10  DWS-SATZ     REDEFINES      DWS-VORSATZ.
      *
               20  DWS-SA                  PIC  X(001).
      *            SATZART
               20  DWS-GES                 PIC  9(002).
      *            GESELLSCHAFT
               20  DWS-WP-KENN-NR          PIC  X(006).
      *            WERTPAPIERKENN-NUMMER
               20  DWS-ISIN-NR             PIC  X(012).
      *            DATUM WERTENTWICKLUNG
      *        20  DWS-RUECK-PREIS         PIC  9(007)V99.
               20  DWS-RUECK-PREIS         PIC  X(009).
      *            RUECKNAHMEPREIS
      *        20  DWS-AUS-PREIS           PIC  9(007)V99.
               20  DWS-AUS-PREIS           PIC  X(009).
      *            AUSGABEPREIS
               20  DWS-PREIS-DAT           PIC  9(008).
      *            DATUM Preise
      *        20  DWS-WERT-ENTW           PIC S9(006)V99.
               20  DWS-WERT-ENTW           PIC  X(009).
      *            WERTENTWICKLUNG in Prozent
               20  DWS-WERT-ENTW-DAT       PIC  9(008).
      *            DATUM WERTENTWICKLUNG
               20  DWS-WKZ                 PIC  X(003).
               20  FILLER                  PIC  X(013).

      *
           10  DWS-NACHSATZ    REDEFINES     DWS-VORSATZ.
      *
               20  DWS-N-SA             PIC X(001).
      *            SATZART             'E'
      *        20  DWS-N-ANZ            PIC X(009).
               20  DWS-N-ANZ            PIC X(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
      *        20  FILLER               PIC X(070).
               20  FILLER               PIC X(072).
      *            RESERVE

