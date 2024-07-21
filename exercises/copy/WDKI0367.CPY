      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0367.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0367.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:26:54   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:50   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-WOHNGEBBAUARTKL-GESAMT.
         10 EA-NI-WOHNGEBBAUARTKL-TAB.
           20 EA-NI-WOHNGEBBAUARTKL-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-WOHNGEBBAUARTKL
                 REDEFINES EA-NI-WOHNGEBBAUARTKL-TAB.
           20 EA-NI-WGEBBAUARTKLID           PIC X.
           20 EA-NI-WGEBBAUARTKLTEXT         PIC X.
         10 EA-DCL-WOHNGEBBAUARTKL.
           20 EA-WGEBBAUARTKLID              PIC -----9    .
           20 EA-WGEBBAUARTKLTEXT            PIC X(30).
      ******************************************************************
