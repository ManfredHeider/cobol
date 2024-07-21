      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0310.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0310.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:23:46   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:28   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-GEFAHR-GESAMT.
         10 EA-NI-GEFAHR-TAB.
           20 EA-NI-GEFAHR-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-GEFAHR
                 REDEFINES EA-NI-GEFAHR-TAB.
           20 EA-NI-GEFGEFAHRCODE            PIC X.
           20 EA-NI-GEFGEFAHRKTEXT           PIC X.
           20 EA-NI-GEFGEFAHRTEXT            PIC X.
         10 EA-DCL-GEFAHR.
           20 EA-GEFGEFAHRCODE               PIC -----9    .
           20 EA-GEFGEFAHRKTEXT              PIC X(3).
           20 EA-GEFGEFAHRTEXT               PIC X(30).
      ******************************************************************
