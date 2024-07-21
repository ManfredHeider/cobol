      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0004.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0004.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 15:19:58   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabelle
      *
      *   Rev 2.0   Apr 15 1999 13:32:00   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-ANREDE-GESAMT.
         10 EA-NI-ANREDE-TAB.
           20 EA-NI-ANREDE-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-ANREDE
                 REDEFINES EA-NI-ANREDE-TAB.
           20 EA-NI-ANREDEID                 PIC X.
           20 EA-NI-ANREDETEXT               PIC X.
         10 EA-DCL-ANREDE.
           20 EA-ANREDEID                    PIC -----9    .
           20 EA-ANREDETEXT                  PIC X(20).
      ******************************************************************
