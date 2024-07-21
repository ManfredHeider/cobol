      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0723.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0723.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:33:18   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:14   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-TMPSELTREFFER-GESAMT.
         10 EA-NI-TMPSELTREFFER-TAB.
           20 EA-NI-TMPSELTREFFER-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-TMPSELTREFFER
                 REDEFINES EA-NI-TMPSELTREFFER-TAB.
           20 EA-NI-TREFFERPERSONENID        PIC X.
           20 EA-NI-TREFFERSCHRITTNR         PIC X.
           20 EA-NI-TREFFERGRUNDNUMMER       PIC X.
           20 EA-NI-TREFFERWERT              PIC X.
         10 EA-DCL-TMPSELTREFFER.
           20 EA-TREFFERPERSONENID           PIC X(13).
           20 EA-TREFFERSCHRITTNR            PIC -----9    .
           20 EA-TREFFERGRUNDNUMMER          PIC -----9    .
           20 EA-TREFFERWERT                 PIC X(100).
      ******************************************************************
