      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0730.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0730.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:32:06   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:18   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-INFOCENTERENTRY-GESAMT.
         10 EA-NI-INFOCENTERENTRY-TAB.
           20 EA-NI-INFOCENTERENTRY-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-INFOCENTERENTRY
                 REDEFINES EA-NI-INFOCENTERENTRY-TAB.
           20 EA-NI-ICELOGMARKER             PIC X.
           20 EA-NI-ICEID                    PIC X.
           20 EA-NI-ICELFDNR                 PIC X.
           20 EA-NI-ICETYPE                  PIC X.
           20 EA-NI-ICEKEYS                  PIC X.
         10 EA-DCL-INFOCENTERENTRY.
           20 EA-ICELOGMARKER                PIC X(26).
           20 EA-ICEID                       PIC X(13).
           20 EA-ICELFDNR                    PIC --------9 .
           20 EA-ICETYPE.
              49 EA-ICETYPE-LEN              PIC -----9    .
              49 EA-ICETYPE-TEXT             PIC X(40).
           20 EA-ICEKEYS.
              49 EA-ICEKEYS-LEN              PIC -----9    .
              49 EA-ICEKEYS-TEXT             PIC X(128).
      ******************************************************************
