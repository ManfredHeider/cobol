      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0381.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0381.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:28:54   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:56   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERTRAGKZ-GESAMT.
         10 EA-NI-VERTRAGKZ-TAB.
           20 EA-NI-VERTRAGKZ-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-VERTRAGKZ
                 REDEFINES EA-NI-VERTRAGKZ-TAB.
           20 EA-NI-VKZID                    PIC X.
           20 EA-NI-VKZTEXT                  PIC X.
         10 EA-DCL-VERTRAGKZ.
           20 EA-VKZID                       PIC X(01).
           20 EA-VKZTEXT.
              49 EA-VKZTEXT-LEN              PIC -----9    .
              49 EA-VKZTEXT-TEXT             PIC X(30).
      ******************************************************************
