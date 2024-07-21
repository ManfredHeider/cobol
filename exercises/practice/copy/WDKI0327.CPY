      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0327.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WDKI0327.CPv  $
      *
      *   Rev 2.2   May 30 2005 09:22:12   t13ex27
      *AKI00485: DB-Änderung für Fix 75 EA-Programme
      *
      *   Rev 2.1   Apr 16 1999 10:07:42   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:34   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-LEBENFOND-GESAMT.
         10 EA-NI-LEBENFOND-TAB.
           20 EA-NI-LEBENFOND-ELE PIC X OCCURS 13 TIMES.
         10 EA-NI-LEBENFOND
                 REDEFINES EA-NI-LEBENFOND-TAB.
           20 EA-NI-LFOVERTRAGSID            PIC X.
           20 EA-NI-LFOVERSTARLFDNR          PIC X.
           20 EA-NI-LFOLFDNR                 PIC X.
           20 EA-NI-LFOANZAHLANTEILE         PIC X.
           20 EA-NI-LFORUECKNAHMEBETR        PIC X.
           20 EA-NI-LFOFONDISIN              PIC X.
           20 EA-NI-LFOFONDBEZEICHNUNG       PIC X.
           20 EA-NI-LFOPROZSPARBETR          PIC X.
           20 EA-NI-LFODECKUNGSKAPITAL       PIC X.
           20 EA-NI-LFODECKKAPITWKZ          PIC X.
           20 EA-NI-LFOBETRAG                PIC X.
           20 EA-NI-LFOBETRAGWKZ             PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-LEBENFOND.
           20 EA-LFOVERTRAGSID               PIC X(13).
           20 EA-LFOVERSTARLFDNR             PIC -----9.
           20 EA-LFOLFDNR                    PIC -----9.
           20 EA-LFOANZAHLANTEILE            PIC ------9.999.
           20 EA-LFORUECKNAHMEBETR           PIC ---9.99.
           20 EA-LFOFONDISIN                 PIC X(12).
           20 EA-LFOFONDBEZEICHNUNG          PIC X(40).
           20 EA-LFOPROZSPARBETR             PIC ---9.99.
           20 EA-LFODECKUNGSKAPITAL          PIC -----------9.99.
           20 EA-LFODECKKAPITWKZ             PIC X(1).
           20 EA-LFOBETRAG                   PIC ---------9.99.
           20 EA-LFOBETRAGWKZ                PIC X(1).
           20 EA-LETZTEAENDERUNG             PIC X(026).
      *****************************************************************
