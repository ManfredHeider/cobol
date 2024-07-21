      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0615.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0615.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:46   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:06   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PCLINIE-GESAMT.
         10 EA-NI-PCLINIE-TAB.
           20 EA-NI-PCLINIE-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-PCLINIE
                 REDEFINES EA-NI-PCLINIE-TAB.
           20 EA-NI-PCLINIEID                PIC X.
           20 EA-NI-BEZEICHNUNG              PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-PCLINIE.
           20 EA-PCLINIEID                   PIC X(2).
           20 EA-BEZEICHNUNG                 PIC X(30).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
