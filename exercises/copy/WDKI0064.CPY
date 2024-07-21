      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0064.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0064.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:06:40   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:18   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VEREIN-GESAMT.
         10 EA-NI-VEREIN-TAB.
           20 EA-NI-VEREIN-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-VEREIN
                 REDEFINES EA-NI-VEREIN-TAB.
           20 EA-NI-VEREINPERSONENID         PIC X.
           20 EA-NI-VEREINLFDNUMMER          PIC X.
           20 EA-NI-VEREINTEXT               PIC X.
           20 EA-NI-VEREINFUNKTION           PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-VEREIN.
           20 EA-VEREINPERSONENID            PIC X(13).
           20 EA-VEREINLFDNUMMER             PIC --------9 .
           20 EA-VEREINTEXT                  PIC X(30).
           20 EA-VEREINFUNKTION              PIC X(30).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
