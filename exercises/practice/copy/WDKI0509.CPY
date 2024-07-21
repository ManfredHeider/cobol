      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0509.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0509.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:10   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:00   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PARTNERTEXTFELD-GESAMT.
         10 EA-NI-PARTNERTEXTFELD-TAB.
           20 EA-NI-PARTNERTEXTFELD-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-PARTNERTEXTFELD
                 REDEFINES EA-NI-PARTNERTEXTFELD-TAB.
           20 EA-NI-DOKUMENTID               PIC X.
           20 EA-NI-LAUFENDENUMMER           PIC X.
           20 EA-NI-PTFTEXT                  PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-PARTNERTEXTFELD.
           20 EA-DOKUMENTID                  PIC X(13).
           20 EA-LAUFENDENUMMER              PIC -----9    .
           20 EA-PTFTEXT.
              49 EA-PTFTEXT-LEN              PIC -----9    .
              49 EA-PTFTEXT-TEXT             PIC X(80).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
