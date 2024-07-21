      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0808.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0808.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:32:54   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:20   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-IIMPKCOLUMN-GESAMT.
         10 EA-NI-IIMPKCOLUMN-TAB.
           20 EA-NI-IIMPKCOLUMN-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-IIMPKCOLUMN
                 REDEFINES EA-NI-IIMPKCOLUMN-TAB.
           20 EA-NI-IIMROWID                 PIC X.
           20 EA-NI-IIMNAME                  PIC X.
           20 EA-NI-IIMTHEVALUE              PIC X.
         10 EA-DCL-IIMPKCOLUMN.
           20 EA-IIMROWID                    PIC --------9 .
           20 EA-IIMNAME                     PIC X(18).
           20 EA-IIMTHEVALUE                 PIC X(01).
      ******************************************************************
