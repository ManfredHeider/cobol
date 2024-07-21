      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0386.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0386.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:56:54   I13PC19
      * 
      *
      *   Rev 2.0   Apr 16 1999 10:41:40   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-FINDATSPALTE-GESAMT.
         10 EA-NI-FINDATSPALTE-TAB.
           20 EA-NI-FINDATSPALTE-ELE PIC X
                  OCCURS 8 TIMES.
         10 EA-NI-FINDATSPALTE
                 REDEFINES EA-NI-FINDATSPALTE-TAB.
           20 EA-NI-FDSPSPALTEID             PIC X.
           20 EA-NI-FDSPAUSRICHTUNG          PIC X.
           20 EA-NI-FDSPAUTOBREITE           PIC X.
           20 EA-NI-FDSPTABELLE              PIC X.
           20 EA-NI-FDSPBREITE               PIC X.
           20 EA-NI-FDSPTABELLENPOS          PIC X.
           20 EA-NI-FDSPHEADER               PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-FINDATSPALTE.
           20 EA-FDSPSPALTEID                PIC X(13).
           20 EA-FDSPAUSRICHTUNG             PIC -----9    .
           20 EA-FDSPAUTOBREITE              PIC X(1).
           20 EA-FDSPTABELLE                 PIC X(13).
           20 EA-FDSPBREITE                  PIC -----9    .
           20 EA-FDSPTABELLENPOS             PIC -----9    .
           20 EA-FDSPHEADER.
              49 EA-FDSPHEADER-LEN           PIC -----9    .
              49 EA-FDSPHEADER-TEXT          PIC X(80).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
