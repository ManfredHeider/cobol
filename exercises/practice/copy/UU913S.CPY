      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU913S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU913S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:05:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *    --------------------------------------------------------    *
      *                                                                *
      *    BILD-TEXT-TABELLE FUER KEY-BILD                             *
      *                                                                *
      *    TRANSAKTIONEN                   ERSTELLT: H.WIECHERS        *
      *    O P A L                               AM: 12.06.85          *
      *                            AUFRUF: TEXT-TAB5 COPY UU913S       *
      *                                                                *
      *    --------------------------------------------------------    *
CCC009*    SKIP1
       01  UU913S.
           02 FILLER PIC X(26) VALUE '001    GESELLSCHAFTS-NR.:1'.
           02 FILLER PIC X(26) VALUE '002         VERTRAGS-NR.:2'.
           02 FILLER PIC X(26) VALUE '004             FAELLIG :3'.
           02 FILLER PIC X(26) VALUE '008   PRODUKTSCHLUESSEL :4'.
           02 FILLER PIC X(26) VALUE '016     LAUFENDE NUMMER :5'.
           02 FILLER PIC X(26) VALUE '032   PRODUKTSCHLUESSEL :2'.
CCC009*    SKIP1
       01  TEXT-TAB  REDEFINES  TEXT-TAB5.
           02  TEXT-TB5                OCCURS 6.
               03  TEXTNR5             PIC 9(3).
               03  TEXT5               PIC X(22).
               03  ZEILE5              PIC 9.

