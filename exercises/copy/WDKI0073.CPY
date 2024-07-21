      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0073.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0073.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:22:42   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:22   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KUNDENNUMMER-GESAMT.
         10 EA-NI-KUNDENNUMMER-TAB.
           20 EA-NI-KUNDENNUMMER-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-KUNDENNUMMER
                 REDEFINES EA-NI-KUNDENNUMMER-TAB.
           20 EA-NI-KDNR-GRUPPE              PIC X.
           20 EA-NI-KDNR-ID-AKTUELL          PIC X.
           20 EA-NI-KDNR-ID-BIS              PIC X.
           20 EA-NI-KDNR-PARALLEL            PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-KUNDENNUMMER.
           20 EA-KDNR-GRUPPE                 PIC -----9    .
           20 EA-KDNR-ID-AKTUELL             PIC --------9 .
           20 EA-KDNR-ID-BIS                 PIC --------9 .
           20 EA-KDNR-PARALLEL               PIC X(250).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
