      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0713.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0713.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:30:58   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:12   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-BESUCHSAUFTRAGART-GESAMT.
         10 EA-NI-BESUCHSAUFTRAGART-TAB.
           20 EA-NI-BESUCHSAUFTRAGART-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-BESUCHSAUFTRAGART
                 REDEFINES EA-NI-BESUCHSAUFTRAGART-TAB.
           20 EA-NI-BESUCHSAUFTRARTID        PIC X.
           20 EA-NI-BESUCHSAUFTRAGTEXT       PIC X.
         10 EA-DCL-BESUCHSAUFTRAGART.
           20 EA-BESUCHSAUFTRARTID           PIC X(01).
           20 EA-BESUCHSAUFTRAGTEXT          PIC X(5).
      ******************************************************************
