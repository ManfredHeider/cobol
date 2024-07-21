      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0063.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0063.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:22:08   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:18   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-STRUKTURSTUFE-GESAMT.
         10 EA-NI-STRUKTURSTUFE-TAB.
           20 EA-NI-STRUKTURSTUFE-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-STRUKTURSTUFE
                 REDEFINES EA-NI-STRUKTURSTUFE-TAB.
           20 EA-NI-STRUKTURSTUFEID          PIC X.
           20 EA-NI-STRSTUFEKURZTEXT         PIC X.
           20 EA-NI-STRSTUFELANGTEXT         PIC X.
         10 EA-DCL-STRUKTURSTUFE.
           20 EA-STRUKTURSTUFEID             PIC --------9 .
           20 EA-STRSTUFEKURZTEXT            PIC X(5).
           20 EA-STRSTUFELANGTEXT            PIC X(30).
      ******************************************************************
