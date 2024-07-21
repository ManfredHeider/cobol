      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0804.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0804.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:32:22   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:18   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-IIMCOLUMN-GESAMT.
         10 EA-NI-IIMCOLUMN-TAB.
           20 EA-NI-IIMCOLUMN-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-IIMCOLUMN
                 REDEFINES EA-NI-IIMCOLUMN-TAB.
           20 EA-NI-IIMMODIFICATIONID        PIC X.
           20 EA-NI-IIMNAME                  PIC X.
         10 EA-DCL-IIMCOLUMN.
           20 EA-IIMMODIFICATIONID           PIC --------9 .
           20 EA-IIMNAME                     PIC X(18).
      ******************************************************************
