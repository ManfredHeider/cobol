      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0024.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0024.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:04:30   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:06   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-HOBBY-GESAMT.
         10 EA-NI-HOBBY-TAB.
           20 EA-NI-HOBBY-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-HOBBY
                 REDEFINES EA-NI-HOBBY-TAB.
           20 EA-NI-HOBBYPERSONENID          PIC X.
           20 EA-NI-HOBBYLFDNUMMER           PIC X.
           20 EA-NI-HOBBYTEXT                PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-HOBBY.
           20 EA-HOBBYPERSONENID             PIC X(13).
           20 EA-HOBBYLFDNUMMER              PIC --------9 .
           20 EA-HOBBYTEXT                   PIC X(30).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
