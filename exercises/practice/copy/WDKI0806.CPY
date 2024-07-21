      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0806.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0806.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:32:38   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:20   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-IIMFOREIGNKEY-GESAMT.
         10 EA-NI-IIMFOREIGNKEY-TAB.
           20 EA-NI-IIMFOREIGNKEY-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-IIMFOREIGNKEY
                 REDEFINES EA-NI-IIMFOREIGNKEY-TAB.
           20 EA-NI-IIMID                    PIC X.
           20 EA-NI-IIMROWID                 PIC X.
         10 EA-DCL-IIMFOREIGNKEY.
           20 EA-IIMID                       PIC --------9 .
           20 EA-IIMROWID                    PIC --------9 .
      ******************************************************************