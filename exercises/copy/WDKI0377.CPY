      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0377.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0377.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:28:38   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:54   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KFZSFKLASSEN-GESAMT.
         10 EA-NI-KFZSFKLASSEN-TAB.
           20 EA-NI-KFZSFKLASSEN-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-KFZSFKLASSEN
                 REDEFINES EA-NI-KFZSFKLASSEN-TAB.
           20 EA-NI-KFZSFKLID                PIC X.
           20 EA-NI-KFZSFKLTEXT              PIC X.
         10 EA-DCL-KFZSFKLASSEN.
           20 EA-KFZSFKLID                   PIC -----9    .
           20 EA-KFZSFKLTEXT                 PIC X(5).
      ******************************************************************
