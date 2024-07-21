      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0611.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0611.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:29:28   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:04   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-BENUTZER-SETTINGS-GESAMT.
         10 EA-NI-BENUTZER-SETTINGS-TAB.
           20 EA-NI-BENUTZER-SETTINGS-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-BENUTZER-SETTINGS
                 REDEFINES EA-NI-BENUTZER-SETTINGS-TAB.
           20 EA-NI-BESBENUTZERID            PIC X.
           20 EA-NI-BESBENUTZERROLLE         PIC X.
           20 EA-NI-BESPARAMETERNAME         PIC X.
           20 EA-NI-BESPARAMETERWERT         PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-BENUTZER-SETTINGS.
           20 EA-BESBENUTZERID               PIC X(13).
           20 EA-BESBENUTZERROLLE            PIC X(01).
           20 EA-BESPARAMETERNAME.
              49 EA-BESPARAMETERNAME-LEN     PIC -----9    .
              49 EA-BESPARAMETERNAME-TEXT    PIC X(200).
           20 EA-BESPARAMETERWERT.
              49 EA-BESPARAMETERWERT-LEN     PIC -----9    .
              49 EA-BESPARAMETERWERT-TEXT    PIC X(200).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
