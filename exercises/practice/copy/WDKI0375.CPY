      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0375.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0375.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:36:56   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:54   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-BEIHILFESTATUS-GESAMT.
         10 EA-NI-BEIHILFESTATUS-TAB.
           20 EA-NI-BEIHILFESTATUS-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-BEIHILFESTATUS
                 REDEFINES EA-NI-BEIHILFESTATUS-TAB.
           20 EA-NI-BEIHILFESTATUSID         PIC X.
           20 EA-NI-BEIHILFESTATUSTEXT       PIC X.
         10 EA-DCL-BEIHILFESTATUS.
           20 EA-BEIHILFESTATUSID            PIC -----9    .
           20 EA-BEIHILFESTATUSTEXT          PIC X(50).
      ******************************************************************
