      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0306.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0306.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:23:32   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:26   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-DIREKTVERSICHERER-GESAMT.
         10 EA-NI-DIREKTVERSICHERER-TAB.
           20 EA-NI-DIREKTVERSICHERER-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-DIREKTVERSICHERER
                 REDEFINES EA-NI-DIREKTVERSICHERER-TAB.
           20 EA-NI-DIREKTVERSID             PIC X.
           20 EA-NI-DIREKTVERSTEXT           PIC X.
         10 EA-DCL-DIREKTVERSICHERER.
           20 EA-DIREKTVERSID                PIC X(01).
           20 EA-DIREKTVERSTEXT              PIC X(30).
      ******************************************************************
