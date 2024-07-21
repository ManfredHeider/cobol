      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0705.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0705.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:58   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:08   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-TERMIN-GESAMT.
         10 EA-NI-TERMIN-TAB.
           20 EA-NI-TERMIN-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-TERMIN
                 REDEFINES EA-NI-TERMIN-TAB.
           20 EA-NI-TERMINID                 PIC X.
           20 EA-NI-STATUSID                 PIC X.
           20 EA-NI-ARBEITSAUFTRAGID         PIC X.
           20 EA-NI-ORT                      PIC X.
         10 EA-DCL-TERMIN.
           20 EA-TERMINID                    PIC X(13).
           20 EA-STATUSID                    PIC --------9 .
           20 EA-ARBEITSAUFTRAGID            PIC X(13).
           20 EA-ORT.
              49 EA-ORT-LEN                  PIC -----9    .
              49 EA-ORT-TEXT                 PIC X(200).
      ******************************************************************
