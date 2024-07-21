      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0062.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0062.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:22:02   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:18   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KUNDENSTATUS-GESAMT.
         10 EA-NI-KUNDENSTATUS-TAB.
           20 EA-NI-KUNDENSTATUS-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-KUNDENSTATUS
                 REDEFINES EA-NI-KUNDENSTATUS-TAB.
           20 EA-NI-KUNDENSTATUSID           PIC X.
           20 EA-NI-KUNDENSTATUSTEXT         PIC X.
         10 EA-DCL-KUNDENSTATUS.
           20 EA-KUNDENSTATUSID              PIC X(01).
           20 EA-KUNDENSTATUSTEXT            PIC X(15).
      ******************************************************************
