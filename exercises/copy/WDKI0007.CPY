      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0007.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0007.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 15:20:08   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabelle
      *
      *   Rev 2.0   Apr 15 1999 13:32:00   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-BANKBLZ-GESAMT.
         10 EA-NI-BANKBLZ-TAB.
           20 EA-NI-BANKBLZ-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-BANKBLZ
                 REDEFINES EA-NI-BANKBLZ-TAB.
           20 EA-NI-BANKBLZID                PIC X.
           20 EA-NI-BANKBLZLEITZAHL          PIC X.
           20 EA-NI-BANKLEITZAHLTEXT         PIC X.
         10 EA-DCL-BANKBLZ.
           20 EA-BANKBLZID                   PIC --------9 .
           20 EA-BANKBLZLEITZAHL             PIC --------9 .
           20 EA-BANKLEITZAHLTEXT            PIC X(30).
      ******************************************************************