      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0049.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0049.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:21:36   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:12   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PERSREFNUMMERART-GESAMT.
         10 EA-NI-PERSREFNUMMERART-TAB.
           20 EA-NI-PERSREFNUMMERART-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-PERSREFNUMMERART
                 REDEFINES EA-NI-PERSREFNUMMERART-TAB.
           20 EA-NI-PREFNUMARTID             PIC X.
           20 EA-NI-PREFNUMARTTEXT           PIC X.
         10 EA-DCL-PERSREFNUMMERART.
           20 EA-PREFNUMARTID                PIC -----9    .
           20 EA-PREFNUMARTTEXT              PIC X(30).
      ******************************************************************
