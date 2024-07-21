      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0388.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0388.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:56:58   I13PC19
      * 
      *
      *   Rev 2.0   Apr 16 1999 10:41:42   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-FINDATWERT-GESAMT.
         10 EA-NI-FINDATWERT-TAB.
           20 EA-NI-FINDATWERT-ELE PIC X
                  OCCURS 6 TIMES.
         10 EA-NI-FINDATWERT
                 REDEFINES EA-NI-FINDATWERT-TAB.
           20 EA-NI-FDWEZELLEID              PIC X.
           20 EA-NI-FDWESPALTEPOSITION       PIC X.
           20 EA-NI-FDWEWERT                 PIC X.
           20 EA-NI-FDWESPALTE               PIC X.
           20 EA-NI-FDWEEDITIERBAR           PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-FINDATWERT.
           20 EA-FDWEZELLEID                 PIC X(13).
           20 EA-FDWESPALTEPOSITION          PIC -----9    .
           20 EA-FDWEWERT.
              49 EA-FDWEWERT-LEN             PIC -----9    .
              49 EA-FDWEWERT-TEXT            PIC X(30).
           20 EA-FDWESPALTE                  PIC X(13).
           20 EA-FDWEEDITIERBAR              PIC X(1).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
