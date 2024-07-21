      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0361.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0361.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:26:22   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:48   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERTRAGSART-GESAMT.
         10 EA-NI-VERTRAGSART-TAB.
           20 EA-NI-VERTRAGSART-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-VERTRAGSART
                 REDEFINES EA-NI-VERTRAGSART-TAB.
           20 EA-NI-VERTRAGSARTID            PIC X.
           20 EA-NI-VERTRAGSARTTEXT          PIC X.
         10 EA-DCL-VERTRAGSART.
           20 EA-VERTRAGSARTID               PIC -----9    .
           20 EA-VERTRAGSARTTEXT             PIC X(25).
      ******************************************************************
