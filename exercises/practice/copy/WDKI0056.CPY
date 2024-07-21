      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0056.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0056.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:06:32   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:16   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-ZIELEWUENSCHE-GESAMT.
         10 EA-NI-ZIELEWUENSCHE-TAB.
           20 EA-NI-ZIELEWUENSCHE-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-ZIELEWUENSCHE
                 REDEFINES EA-NI-ZIELEWUENSCHE-TAB.
           20 EA-NI-ZWPERSONENID             PIC X.
           20 EA-NI-ZWPERSONENROLLE          PIC X.
           20 EA-NI-ZWZWTEXTID               PIC X.
           20 EA-NI-ZWPRIORITAET             PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-ZIELEWUENSCHE.
           20 EA-ZWPERSONENID                PIC X(13).
           20 EA-ZWPERSONENROLLE             PIC X(01).
           20 EA-ZWZWTEXTID                  PIC -----9    .
           20 EA-ZWPRIORITAET                PIC -----9    .
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
