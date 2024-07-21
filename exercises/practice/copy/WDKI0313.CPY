      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0313.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/WDKI0313.CPv  $
      *
      *   Rev 2.3   May 27 2003 11:39:24   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.2   Oct 15 2002 08:15:50   t13ex27
      *AKI00308 DB-Änderung Fixpack 26
      *
      *   Rev 2.1   Apr 16 1999 10:07:20   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:28   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-GUTHABENANTEIL-GESAMT.
         10 EA-NI-GUTHABENANTEIL-TAB.
           20 EA-NI-GUTHABENANTEIL-ELE PIC X OCCURS 23 TIMES.
         10 EA-NI-GUTHABENANTEIL
                 REDEFINES EA-NI-GUTHABENANTEIL-TAB.
           20 EA-NI-GHAVERTRAGSID            PIC X.
           20 EA-NI-GHAGUTHABPRODLFDNR       PIC X.
           20 EA-NI-GHAFONDID                PIC X.
           20 EA-NI-GHALFDNR                 PIC X.
           20 EA-NI-GHAFONDTEXT              PIC X.
           20 EA-NI-GHAGUTHABEN              PIC X.
           20 EA-NI-GHAGUTHABENWKZ           PIC X.
           20 EA-NI-GHAGUTHABENANT           PIC X.
           20 EA-NI-GHAKUMULAUSZAHLUNG       PIC X.
           20 EA-NI-GHAKUMAUSZWKZ            PIC X.
           20 EA-NI-GHAKUMULEINZAHLUNG       PIC X.
           20 EA-NI-GHAKUMEINZWKZ            PIC X.
           20 EA-NI-GHAWERTENTWGES           PIC X.
           20 EA-NI-GHAWERTENTWLFDJAHR       PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-GHASPEKULATIONANT        PIC X.
           20 EA-NI-GHASPEKULATIONBET        PIC X.
           20 EA-NI-GHASPEKULATBETWKZ        PIC X.
           20 EA-NI-GHASPEKULATIONDAT        PIC X.
           20 EA-NI-GHALIEFERDATUM           PIC X.
           20 EA-NI-GHAPERFLFDJAHR           PIC X.
           20 EA-NI-GHAPERFGESAMT            PIC X.
           20 EA-NI-GHAANLAGEZIEL            PIC X.
         10 EA-DCL-GUTHABENANTEIL.
           20 EA-GHAVERTRAGSID               PIC X(13).
           20 EA-GHAGUTHABPRODLFDNR          PIC -----9.
           20 EA-GHAFONDID                   PIC -----9.
           20 EA-GHALFDNR                    PIC -----9.
           20 EA-GHAFONDTEXT                 PIC X(10).
           20 EA-GHAGUTHABEN                 PIC ---------9.99.
           20 EA-GHAGUTHABENWKZ              PIC X(1).
           20 EA-GHAGUTHABENANT              PIC ------9.9999.
           20 EA-GHAKUMULAUSZAHLUNG          PIC -------------9.
           20 EA-GHAKUMAUSZWKZ               PIC X(1).
           20 EA-GHAKUMULEINZAHLUNG          PIC -------------9.
           20 EA-GHAKUMEINZWKZ               PIC X(1).
           20 EA-GHAWERTENTWGES              PIC ---9.99.
           20 EA-GHAWERTENTWLFDJAHR          PIC ---9.99.
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-GHASPEKULATIONANT           PIC ------9.999.
           20 EA-GHASPEKULATIONBET           PIC ---------9.99.
           20 EA-GHASPEKULATBETWKZ           PIC X(1).
           20 EA-GHASPEKULATIONDAT           PIC X(010).
           20 EA-GHALIEFERDATUM              PIC X(010).
           20 EA-GHAPERFLFDJAHR              PIC -----9.99.
           20 EA-GHAPERFGESAMT               PIC -----9.99.
           20 EA-GHAANLAGEZIEL               PIC X(1).
      *****************************************************************
