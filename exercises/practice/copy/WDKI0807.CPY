      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0807.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0807.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:32:46   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:20   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-IIMMODIFICATION-GESAMT.
         10 EA-NI-IIMMODIFICATION-TAB.
           20 EA-NI-IIMMODIFICATION-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-IIMMODIFICATION
                 REDEFINES EA-NI-IIMMODIFICATION-TAB.
           20 EA-NI-IIMID                    PIC X.
           20 EA-NI-IIMTRANSID               PIC X.
           20 EA-NI-IIMTABLENAME             PIC X.
           20 EA-NI-IIMOPERATION             PIC X.
         10 EA-DCL-IIMMODIFICATION.
           20 EA-IIMID                       PIC --------9 .
           20 EA-IIMTRANSID                  PIC --------9 .
           20 EA-IIMTABLENAME                PIC X(18).
           20 EA-IIMOPERATION                PIC X(2).
      ******************************************************************
