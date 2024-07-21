      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0376.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0376.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:37:04   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:54   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-BEIHILFEVORSCHRIFT-GESAMT.
         10 EA-NI-BEIHILFEVORSCHRIFT-TAB.
           20 EA-NI-BEIHILFEVORSCHRIFT-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-BEIHILFEVORSCHRIFT
                 REDEFINES EA-NI-BEIHILFEVORSCHRIFT-TAB.
           20 EA-NI-BEIHILFEVORSCHRID        PIC X.
           20 EA-NI-BEIHILFEVORSCHRTXT       PIC X.
         10 EA-DCL-BEIHILFEVORSCHRIFT.
           20 EA-BEIHILFEVORSCHRID           PIC -----9    .
           20 EA-BEIHILFEVORSCHRTXT          PIC X(50).
      ******************************************************************