      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0322.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0322.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:07:36   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:32   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KFZKASKO-GESAMT.
         10 EA-NI-KFZKASKO-TAB.
           20 EA-NI-KFZKASKO-ELE PIC X
                  OCCURS 14 TIMES.
         10 EA-NI-KFZKASKO
                 REDEFINES EA-NI-KFZKASKO-TAB.
           20 EA-NI-KFKVERTRAGSID            PIC X.
           20 EA-NI-KFKVERSTARLFDNR          PIC X.
           20 EA-NI-KFKKASKOART              PIC X.
           20 EA-NI-KFKKASKOBEITRAG          PIC X.
           20 EA-NI-KFKKASKOBEITRWKZ         PIC X.
           20 EA-NI-KFKBEITRSATZFZINPR       PIC X.
           20 EA-NI-KFKREGIONALKLASSE        PIC X.
           20 EA-NI-KFKSCHADFREIHEITID       PIC X.
           20 EA-NI-KFKSEBLSTBETEILTK        PIC X.
           20 EA-NI-KFKSBTEILKASKOWKZ        PIC X.
           20 EA-NI-KFKSELBSTBETEILVK        PIC X.
           20 EA-NI-KFKSBVOLLKASKOWKZ        PIC X.
           20 EA-NI-KFKSELBSTBETEILKZ        PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-KFZKASKO.
           20 EA-KFKVERTRAGSID               PIC X(13).
           20 EA-KFKVERSTARLFDNR             PIC -----9    .
           20 EA-KFKKASKOART                 PIC X(01).
           20 EA-KFKKASKOBEITRAG             PIC ---------9.99    .
           20 EA-KFKKASKOBEITRWKZ            PIC X(01).
           20 EA-KFKBEITRSATZFZINPR          PIC ---9.99          .
           20 EA-KFKREGIONALKLASSE           PIC X(3).
           20 EA-KFKSCHADFREIHEITID          PIC -----9    .
           20 EA-KFKSEBLSTBETEILTK           PIC ---------9.99    .
           20 EA-KFKSBTEILKASKOWKZ           PIC X(01).
           20 EA-KFKSELBSTBETEILVK           PIC ---------9.99    .
           20 EA-KFKSBVOLLKASKOWKZ           PIC X(01).
           20 EA-KFKSELBSTBETEILKZ           PIC X(01).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
