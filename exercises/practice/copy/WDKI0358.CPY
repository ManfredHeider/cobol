      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0358.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0358.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:11:00   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:46   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERTRAGBANKVBDG-GESAMT.
         10 EA-NI-VERTRAGBANKVBDG-TAB.
           20 EA-NI-VERTRAGBANKVBDG-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-VERTRAGBANKVBDG
                 REDEFINES EA-NI-VERTRAGBANKVBDG-TAB.
           20 EA-NI-VBVVERTRAGSID            PIC X.
           20 EA-NI-VBVPERSONENID            PIC X.
           20 EA-NI-VBVLAUFENDENUMMER        PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-VERTRAGBANKVBDG.
           20 EA-VBVVERTRAGSID               PIC X(13).
           20 EA-VBVPERSONENID               PIC X(13).
           20 EA-VBVLAUFENDENUMMER           PIC --------9 .
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
