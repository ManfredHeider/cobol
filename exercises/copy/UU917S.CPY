      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU917S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU917S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:06:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *    --------------------------------------------------------    *
      *                                                                *
      *    BILD-TEXT-TABELLE FUER KEY-BILD                             *
      *                                                                *
      *    TRANSAKTIONEN                   ERSTELLT: H.SCHORN (AGS)    *
      *    STATISTIK / SOLLVORGABEN/             AM: 16.02.81          *
      *    TURCK-ANWENDUNGEN                                           *
      *                                                                *
      *    STAND VOM 29.09.1982                                        *
      *                                                                *
      *    --------------------------------------------------------    *
CCC009*    SKIP1
       01  UU917S.
           02 FILLER PIC X(32) VALUE '001      VERMOEGENSBERATER-NR.:1'.
           02 FILLER PIC X(32) VALUE '002             MONAT UND JAHR:2'.
           02 FILLER PIC X(32) VALUE '004                     AKTION:3'.
           02 FILLER PIC X(32) VALUE '008            VOR- UND ZUNAME:2'.
           02 FILLER PIC X(32) VALUE '016                            3'.
CCC009*    SKIP1
       01  TEXT-TAB  REDEFINES  TEXT-TAB3.
           02  TEXT-TB2                OCCURS 5.
               03  TEXTNR3             PIC 9(3).
               03  TEXT3               PIC X(28).
               03  ZEILE3              PIC 9.

