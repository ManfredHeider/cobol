      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0805.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0805.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:32:30   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:20   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-IIMFKCOLUMN-GESAMT.
         10 EA-NI-IIMFKCOLUMN-TAB.
           20 EA-NI-IIMFKCOLUMN-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-IIMFKCOLUMN
                 REDEFINES EA-NI-IIMFKCOLUMN-TAB.
           20 EA-NI-IIMNAME                  PIC X.
           20 EA-NI-IIMFOREIGNKEYID          PIC X.
           20 EA-NI-IIMOLDVALUE              PIC X.
           20 EA-NI-IIMTHEVALUE              PIC X.
         10 EA-DCL-IIMFKCOLUMN.
           20 EA-IIMNAME                     PIC X(18).
           20 EA-IIMFOREIGNKEYID             PIC --------9 .
           20 EA-IIMOLDVALUE                 PIC X(01).
           20 EA-IIMTHEVALUE                 PIC X(01).
      ******************************************************************
