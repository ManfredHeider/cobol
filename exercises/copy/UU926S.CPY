      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU926S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU926S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:08:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *    --------------------------------------------------------    *
      *                                                                *
      *    BILD-TEXT-TABELLE FUER KEY-BILD                             *
      *                                                                *
      *    TRANSAKTIONEN                   ERSTELLT: H.SCHORN (AGS)    *
      *    KUNDEN-DATEI                          AM: 16.11.82          *
      *                            AUFRUF: TEXT-TAB4 COPY UU926S       *
      *    STAND VOM 16.11.1982                                        *
      *                                                                *
      *    --------------------------------------------------------    *
CCC009*    SKIP1
       01  UU926S.
           02 FILLER PIC X(32) VALUE '001               GESELLSCHAFT:1'.
           02 FILLER PIC X(32) VALUE '002          UND  VERTRAGS-NR.:2'.
           02 FILLER PIC X(32) VALUE '004                            3'.
           02 FILLER PIC X(32) VALUE '008                            2'.
           02 FILLER PIC X(32) VALUE '016                            3'.
CCC009*    SKIP1
       01  TEXT-TAB  REDEFINES  TEXT-TAB4.
           02  TEXT-TB4                OCCURS 5.
               03  TEXTNR4             PIC 9(3).
               03  TEXT4               PIC X(28).
               03  ZEILE4              PIC 9.

