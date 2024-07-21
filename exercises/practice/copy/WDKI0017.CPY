      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0017.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0017.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:04:22   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:04   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-DATENFELDWERT-GESAMT.
         10 EA-NI-DATENFELDWERT-TAB.
           20 EA-NI-DATENFELDWERT-ELE PIC X
                  OCCURS 7 TIMES.
         10 EA-NI-DATENFELDWERT
                 REDEFINES EA-NI-DATENFELDWERT-TAB.
           20 EA-NI-DATFWERTPERSONENID       PIC X.
           20 EA-NI-DATFWLFDNUMMER           PIC X.
           20 EA-NI-DATFWERTVBID             PIC X.
           20 EA-NI-DATFWERTVBROLLE          PIC X.
           20 EA-NI-DATFWERTLFDNUMMER        PIC X.
           20 EA-NI-DATFWERTWERT             PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-DATENFELDWERT.
           20 EA-DATFWERTPERSONENID          PIC X(13).
           20 EA-DATFWLFDNUMMER              PIC --------9 .
           20 EA-DATFWERTVBID                PIC X(13).
           20 EA-DATFWERTVBROLLE             PIC X(01).
           20 EA-DATFWERTLFDNUMMER           PIC --------9 .
           20 EA-DATFWERTWERT                PIC X(60).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
