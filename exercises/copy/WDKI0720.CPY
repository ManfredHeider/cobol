      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0720.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0720.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:14:16   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:14   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-SELPARAMETER-GESAMT.
         10 EA-NI-SELPARAMETER-TAB.
           20 EA-NI-SELPARAMETER-ELE PIC X
                  OCCURS 6 TIMES.
         10 EA-NI-SELPARAMETER
                 REDEFINES EA-NI-SELPARAMETER-TAB.
           20 EA-NI-SPASUCHFELDID            PIC X.
           20 EA-NI-SPAPARAMETERSATZID       PIC X.
           20 EA-NI-SPALAUFENDENUMMER        PIC X.
           20 EA-NI-SPAWERT1                 PIC X.
           20 EA-NI-SPAWERT2                 PIC X.
           20 EA-NI-SPAISTEINSTELLIG         PIC X.
         10 EA-DCL-SELPARAMETER.
           20 EA-SPASUCHFELDID               PIC X(13).
           20 EA-SPAPARAMETERSATZID          PIC X(13).
           20 EA-SPALAUFENDENUMMER           PIC -----9    .
           20 EA-SPAWERT1.
              49 EA-SPAWERT1-LEN             PIC -----9    .
              49 EA-SPAWERT1-TEXT            PIC X(50).
           20 EA-SPAWERT2.
              49 EA-SPAWERT2-LEN             PIC -----9    .
              49 EA-SPAWERT2-TEXT            PIC X(50).
           20 EA-SPAISTEINSTELLIG            PIC X(1).
      ******************************************************************
