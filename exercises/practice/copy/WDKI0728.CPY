      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0728.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0728.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:31:48   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:16   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-SELKRITSCHLAGWORT-GESAMT.
         10 EA-NI-SELKRITSCHLAGWORT-TAB.
           20 EA-NI-SELKRITSCHLAGWORT-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-SELKRITSCHLAGWORT
                 REDEFINES EA-NI-SELKRITSCHLAGWORT-TAB.
           20 EA-NI-SKSID                    PIC X.
           20 EA-NI-SKSPFADTEXT              PIC X.
           20 EA-NI-SKSERFASSBEREICH         PIC X.
         10 EA-DCL-SELKRITSCHLAGWORT.
           20 EA-SKSID                       PIC -----9    .
           20 EA-SKSPFADTEXT                 PIC X(40).
           20 EA-SKSERFASSBEREICH.
              49 EA-SKSERFASSBEREICH-LEN     PIC -----9    .
              49 EA-SKSERFASSBEREICH-TEXT    PIC X(20).
      ******************************************************************
