      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0363.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0363.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:26:30   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:48   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERTRAGSZUSTAND-GESAMT.
         10 EA-NI-VERTRAGSZUSTAND-TAB.
           20 EA-NI-VERTRAGSZUSTAND-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-VERTRAGSZUSTAND
                 REDEFINES EA-NI-VERTRAGSZUSTAND-TAB.
           20 EA-NI-VERTRAGSZUSTANDID        PIC X.
           20 EA-NI-VERTRAGSZUSTANDTXT       PIC X.
         10 EA-DCL-VERTRAGSZUSTAND.
           20 EA-VERTRAGSZUSTANDID           PIC -----9    .
           20 EA-VERTRAGSZUSTANDTXT          PIC X(35).
      ******************************************************************
