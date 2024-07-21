      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0719.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0719.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:14:14   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:14   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-SELSUCHFELD-GESAMT.
         10 EA-NI-SELSUCHFELD-TAB.
           20 EA-NI-SELSUCHFELD-ELE PIC X
                  OCCURS 7 TIMES.
         10 EA-NI-SELSUCHFELD
                 REDEFINES EA-NI-SELSUCHFELD-TAB.
           20 EA-NI-SSFID                    PIC X.
           20 EA-NI-SSFANFRAGID              PIC X.
           20 EA-NI-SSFOPERATORID            PIC X.
           20 EA-NI-SSFKRITERIUMID           PIC X.
           20 EA-NI-SSFLAUFENDENUMMER        PIC X.
           20 EA-NI-SSFISTFREIGEGEBEN        PIC X.
           20 EA-NI-SSFWIRDPROJIZIERT        PIC X.
         10 EA-DCL-SELSUCHFELD.
           20 EA-SSFID                       PIC X(13).
           20 EA-SSFANFRAGID                 PIC X(13).
           20 EA-SSFOPERATORID               PIC -----9    .
           20 EA-SSFKRITERIUMID              PIC -----9    .
           20 EA-SSFLAUFENDENUMMER           PIC -----9    .
           20 EA-SSFISTFREIGEGEBEN           PIC X(1).
           20 EA-SSFWIRDPROJIZIERT           PIC X(1).
      ******************************************************************
