      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0303.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0303.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:23:14   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:26   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-BAUSPARKTOSTANDKZ-GESAMT.
         10 EA-NI-BAUSPARKTOSTANDKZ-TAB.
           20 EA-NI-BAUSPARKTOSTANDKZ-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-BAUSPARKTOSTANDKZ
                 REDEFINES EA-NI-BAUSPARKTOSTANDKZ-TAB.
           20 EA-NI-BSKSKONTOSTANDID         PIC X.
           20 EA-NI-BSKSKONTOSTANDTEXT       PIC X.
         10 EA-DCL-BAUSPARKTOSTANDKZ.
           20 EA-BSKSKONTOSTANDID            PIC -----9    .
           20 EA-BSKSKONTOSTANDTEXT          PIC X(30).
      ******************************************************************
