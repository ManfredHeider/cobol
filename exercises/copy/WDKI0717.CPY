      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0717.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0717.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:14:10   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:14   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-SELPARAMETERSATZ-GESAMT.
         10 EA-NI-SELPARAMETERSATZ-TAB.
           20 EA-NI-SELPARAMETERSATZ-ELE PIC X
                  OCCURS 6 TIMES.
         10 EA-NI-SELPARAMETERSATZ
                 REDEFINES EA-NI-SELPARAMETERSATZ-TAB.
           20 EA-NI-SPSID                    PIC X.
           20 EA-NI-SPSBENUTZTANFRAGID       PIC X.
           20 EA-NI-SPSBENUTZTFREIGID        PIC X.
           20 EA-NI-SPSERSTELLPID            PIC X.
           20 EA-NI-SPSERSTELLPROLLE         PIC X.
           20 EA-NI-SPSERSTELLDATUM          PIC X.
         10 EA-DCL-SELPARAMETERSATZ.
           20 EA-SPSID                       PIC X(13).
           20 EA-SPSBENUTZTANFRAGID          PIC X(13).
           20 EA-SPSBENUTZTFREIGID           PIC X(13).
           20 EA-SPSERSTELLPID               PIC X(13).
           20 EA-SPSERSTELLPROLLE            PIC X(1).
           20 EA-SPSERSTELLDATUM             PIC X(10).
      ******************************************************************
