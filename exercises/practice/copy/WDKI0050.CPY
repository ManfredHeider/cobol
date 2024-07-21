      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0050.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0050.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:05:00   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:14   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PERSONENROLLE-GESAMT.
         10 EA-NI-PERSONENROLLE-TAB.
           20 EA-NI-PERSONENROLLE-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-PERSONENROLLE
                 REDEFINES EA-NI-PERSONENROLLE-TAB.
           20 EA-NI-PERSROLLENID             PIC X.
           20 EA-NI-PERSROLLENKZ             PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-PERSONENROLLE.
           20 EA-PERSROLLENID                PIC X(13).
           20 EA-PERSROLLENKZ                PIC X(01).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
