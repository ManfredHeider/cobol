      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU704S.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/UU704S.CPv  $
      *
      *   Rev 2.1   Oct 18 2017 12:48:50   t13ex18
      *ARZ00049: Auftrag zum Reaktivieren von Programmen
      *
      *   Rev 2.0   Feb 27 1999 15:15:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:04:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***** VERSION   2    15.09.95 14:47:46
      ****************************************************************
      *                                                              *
      *   COPYMEMBER: STAMMSATZ VON AACHEN                           *
      *   =================================                          *
      *                                                              *
      *   SATZLÄNGE :  125  FEST                                     *
      *   STAND :      13.04.1993                                    *
      *                                                              *
      *   SATZAUFBAU  FÜR FD-EINTRAG    UU704S                       *
      ****************************************************************
      *
      ****************************************************************  0010000
      * AENDERUNGEN:                                                 *  0011000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  0012000
      * -------------------------------------------------------------*  0013000
      * AFVB0049!15.09.95! 02 ! HAFTUNGSVOLUMEN EINGEFÜHRT!BOHLINGER *  0014000
      *         !        !    !                           !          *  0017000
      *                                                              *  0018000
      * ENDE-LOGBUCH                                                 *  0019000
      ****************************************************************  0020000
      *
       01  UU704S.
           05  HAUPT-KONTO               PIC X(010).
           05  EROEFF-DAT.
               10  EROEFF-JJ             PIC 9(004).
               10  EROEFF-MM             PIC 9(002).
               10  EROEFF-TT             PIC 9(002).
           05  SCHLIE-DAT.
               10  SCHLIE-JJ             PIC 9(004).
               10  SCHLIE-MM             PIC 9(002).
               10  SCHLIE-TT             PIC 9(002).
           05  KONTO-DAT.
               10  KONTO-JJ              PIC 9(004).
               10  KONTO-MM              PIC 9(002).
               10  KONTO-TT              PIC 9(002).
           05  KONTOSTAENDE.
               10  DISKONT               PIC S9(009)V99.
               10  RUECKST               PIC S9(009)V99.
               10  VORSCHUSS             PIC S9(009)V99.
               10  ABSCHREIBUNG          PIC S9(009)V99.
950915         10  UEBERDECKUNG          PIC S9(009)V99.
           05  HAFTUNGSVOLUMEN           PIC S9(009)V99.
           05  KENNZEICHEN.
               10  AKT-LFD-GJ            PIC 9(006).
               10  KZ-BEWEG              PIC X(001).
               10  KZ-LOESCH             PIC X(001).
               10  KZ-SELEKT             PIC X(001).
           05  FILLER                    PIC X(016).
      *

