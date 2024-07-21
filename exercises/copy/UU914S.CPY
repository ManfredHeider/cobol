      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU914S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU914S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:06:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *            VERSION 01 880705.
      *    --------------------------------------------------------
      *
      *    BILD-TEXT-TABELLE FUER KEY-BILD
      *
      *    TRANSAKTIONEN:
      *    VB-BEARBEITUNG                  ERSTELLT: H.SCHORN (AGS)
      *                                       DATUM: 26.03.80
      *
      *    --------------------------------------------------------

       01  UU914S.
           02 FILLER PIC X(32) VALUE '001                  VB-NUMMER:1'.
           02 FILLER PIC X(32) VALUE '002       ODER      MATCH-CODE:2'.
           02 FILLER PIC X(32) VALUE '004       UND  DATUM (TTMMJJ)  2'.
           02 FILLER PIC X(32) VALUE '008 OHNE DATUM = GUELTIGES AG8:3'.
           02 FILLER PIC X(32) VALUE '016        UND       DIREKTION:2'.
           02 FILLER PIC X(32) VALUE '032             DATUM (TTMMJJ):3'.

       01  TEXT-TAB  REDEFINES  TEXT-TAB2.
           02  TEXT-TB2                OCCURS 6.
               03  TEXTNR2             PIC 9(3).
               03  TEXT2               PIC X(28).
               03  ZEILE2              PIC 9.

