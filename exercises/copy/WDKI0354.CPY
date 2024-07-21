      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0354.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0354.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:10:52   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:44   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERSTARIFROLLE-GESAMT.
         10 EA-NI-VERSTARIFROLLE-TAB.
           20 EA-NI-VERSTARIFROLLE-ELE PIC X
                  OCCURS 8 TIMES.
         10 EA-NI-VERSTARIFROLLE
                 REDEFINES EA-NI-VERSTARIFROLLE-TAB.
           20 EA-NI-VTRVERTRAGSID            PIC X.
           20 EA-NI-VTRVERSTARLFDNR          PIC X.
           20 EA-NI-VTRPERSONENID            PIC X.
           20 EA-NI-VTRPERSONENROLLEKZ       PIC X.
           20 EA-NI-VTRABLAUFDATUM           PIC X.
           20 EA-NI-VTRGUELTIGKEITSDAT       PIC X.
           20 EA-NI-VTRBEZIEHUNGSARTKZ       PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-VERSTARIFROLLE.
           20 EA-VTRVERTRAGSID               PIC X(13).
           20 EA-VTRVERSTARLFDNR             PIC -----9    .
           20 EA-VTRPERSONENID               PIC X(13).
           20 EA-VTRPERSONENROLLEKZ          PIC X(01).
           20 EA-VTRABLAUFDATUM              PIC X(10).
           20 EA-VTRGUELTIGKEITSDAT          PIC X(10).
           20 EA-VTRBEZIEHUNGSARTKZ          PIC X(01).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
