      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0324.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0324.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:07:40   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:32   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KFZUNFALL-GESAMT.
         10 EA-NI-KFZUNFALL-TAB.
           20 EA-NI-KFZUNFALL-ELE PIC X
                  OCCURS 11 TIMES.
         10 EA-NI-KFZUNFALL
                 REDEFINES EA-NI-KFZUNFALL-TAB.
           20 EA-NI-KFUVERTRAGSID            PIC X.
           20 EA-NI-KFUVERSTARLFDNR          PIC X.
           20 EA-NI-KFUUNFALLART             PIC X.
           20 EA-NI-KFUBEITRAG               PIC X.
           20 EA-NI-KFUBEITRAGWKZ            PIC X.
           20 EA-NI-KFUINVALIDSUMME          PIC X.
           20 EA-NI-KFUINVSUMMEWKZ           PIC X.
           20 EA-NI-KFUPROGRESSINVPROZ       PIC X.
           20 EA-NI-KFUTODSUMME              PIC X.
           20 EA-NI-KFUTODSUMMEWKZ           PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-KFZUNFALL.
           20 EA-KFUVERTRAGSID               PIC X(13).
           20 EA-KFUVERSTARLFDNR             PIC -----9    .
           20 EA-KFUUNFALLART                PIC X(01).
           20 EA-KFUBEITRAG                  PIC ---------9.99    .
           20 EA-KFUBEITRAGWKZ               PIC X(01).
           20 EA-KFUINVALIDSUMME             PIC ---------9.99    .
           20 EA-KFUINVSUMMEWKZ              PIC X(01).
           20 EA-KFUPROGRESSINVPROZ          PIC ---9.99          .
           20 EA-KFUTODSUMME                 PIC ---------9.99    .
           20 EA-KFUTODSUMMEWKZ              PIC X(01).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
