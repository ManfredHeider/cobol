      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1231.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRKI1231.CPv  $
      *
      *   Rev 2.1   Apr 09 2003 09:32:32   t13ex39
      *AKI00194
      *
      *   Rev 2.0   Feb 27 1999 12:52:20   P13SP20
      *NT-Umstellung
      *
      *
      *--------------------------------------------------------------*
      *                                                              *
      *    COPY-MEMBER FÜR NEUE RUECKNAHMEPREISE SEB - IMMO (DTA)    *
      *    ----------------------------------------------------------*
      *                                                              *
      *    DATEINAME = P130006.SEB.FP                                *
      *    LAENGE    = 40 BYTES                                      *
      *                                                              *
      *    DIE DATEI KOMMT PER ISDN-LEITUNG VON SEB TÄGLICH ZU UNS   *
      *    UND ENTHÄLT DIE RÜCKNAHMEPREISE DER EINZELNEN FONDS.      *
      *                                                              *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00194!30.03.01!01.0! NEU                       ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  DRKI1231.
           10  SEB-VORSATZ.
      *
               20  SEB-V-SA            PIC X(001).
      *            SATZART = 'A'
               20  SEB-V-ABSENDER      PIC X(008).
      *            ABSENDER = 'SEB'
               20  SEB-V-DATUM         PIC 9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  SEB-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER = 'DVAG'
               20  SEB-V-TEXT          PIC X(010).
      *            INHALTSANGABE = 'KUNDEN,PRE'
               20  FILLER              PIC  X(09).

      *
           10  SEB-SATZ     REDEFINES      SEB-VORSATZ.
      *
               20  SEB-SA                  PIC  X(001).
      *            SATZART        = 'C'
               20  SEB-GES                 PIC  9(002).
      *            GESELLSCHAFT   = '40'
               20  SEB-WP-KENN-NR          PIC  X(006).
      *            WERTPAPIERKENN-NUMMER
               20  SEB-RUECK-PREIS         PIC  9(03)V99.
      *            RUECKNAHMEPREIS
               20  SEB-AUS-PREIS           PIC  9(03)V99.
      *            AUSGABEPREIS
               20  SEB-WERT-ENTW           PIC S9(03)V99.
      *            WERTENTWICKLUNG
               20  SEB-WERT-ENTW-DAT       PIC  9(08).
      *            DATUM WERTENTWICKLUNG
               20  FILLER                  PIC  X(08).
      *            RESERVE

      *
           10  SEB-NACHSATZ    REDEFINES     SEB-VORSATZ.
      *
               20  SEB-N-SA            PIC X(001).
      *            SATZART             'E'
               20  SEB-N-ANZ           PIC 9(009).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               20  FILLER              PIC X(030).
      *            RESERVE
