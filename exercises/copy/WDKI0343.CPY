      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0343.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0343.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:26:06   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:40   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERSBEDINGUNG-GESAMT.
         10 EA-NI-VERSBEDINGUNG-TAB.
           20 EA-NI-VERSBEDINGUNG-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-VERSBEDINGUNG
                 REDEFINES EA-NI-VERSBEDINGUNG-TAB.
           20 EA-NI-VEBEDINGUNGID            PIC X.
           20 EA-NI-VEBPARTNERGESID          PIC X.
           20 EA-NI-VEBEDINGUNGTEXT          PIC X.
         10 EA-DCL-VERSBEDINGUNG.
           20 EA-VEBEDINGUNGID               PIC --------9 .
           20 EA-VEBPARTNERGESID             PIC -----9    .
           20 EA-VEBEDINGUNGTEXT             PIC X(30).
      ******************************************************************
