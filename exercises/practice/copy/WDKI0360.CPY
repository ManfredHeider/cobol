      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0360.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0360.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:11:04   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:46   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERTRAGRABATT-GESAMT.
         10 EA-NI-VERTRAGRABATT-TAB.
           20 EA-NI-VERTRAGRABATT-ELE PIC X
                  OCCURS 7 TIMES.
         10 EA-NI-VERTRAGRABATT
                 REDEFINES EA-NI-VERTRAGRABATT-TAB.
           20 EA-NI-VRAVERTRAGSID            PIC X.
           20 EA-NI-VRALFDNR                 PIC X.
           20 EA-NI-VRARABTEXT               PIC X.
           20 EA-NI-VRARABWERT               PIC X.
           20 EA-NI-VRARABWERWKZ             PIC X.
           20 EA-NI-VRARABEINHEIT            PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-VERTRAGRABATT.
           20 EA-VRAVERTRAGSID               PIC X(13).
           20 EA-VRALFDNR                    PIC -----9    .
           20 EA-VRARABTEXT                  PIC X(20).
           20 EA-VRARABWERT                  PIC ---------9.99    .
           20 EA-VRARABWERWKZ                PIC X(01).
           20 EA-VRARABEINHEIT               PIC -----9    .
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
