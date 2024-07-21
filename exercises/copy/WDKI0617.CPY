      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0617.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0617.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:30:42   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:06   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PROGUPDATESTATUS-GESAMT.
         10 EA-NI-PROGUPDATESTATUS-TAB.
           20 EA-NI-PROGUPDATESTATUS-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-PROGUPDATESTATUS
                 REDEFINES EA-NI-PROGUPDATESTATUS-TAB.
           20 EA-NI-PUSTATUSID               PIC X.
           20 EA-NI-PUSBEDEUTUNG             PIC X.
           20 EA-NI-PUSHINWEISTEXT           PIC X.
         10 EA-DCL-PROGUPDATESTATUS.
           20 EA-PUSTATUSID                  PIC -----9    .
           20 EA-PUSBEDEUTUNG.
              49 EA-PUSBEDEUTUNG-LEN         PIC -----9    .
              49 EA-PUSBEDEUTUNG-TEXT        PIC X(120).
           20 EA-PUSHINWEISTEXT.
              49 EA-PUSHINWEISTEXT-LEN       PIC -----9    .
              49 EA-PUSHINWEISTEXT-TEXT      PIC X(50).
      ******************************************************************
