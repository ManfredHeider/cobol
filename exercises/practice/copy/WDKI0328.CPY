      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0328.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/WDKI0328.CPv  $
      *
      *   Rev 2.4   Oct 12 2006 12:16:40   t13ex27
      *AKI00624: DB-Änderung für Fix 93 EA-Programme
      *
      *   Rev 2.3   May 24 2004 10:53:30   t13ex27
      *DB-Änderung für Fix 61 EA-Programme und WDKI's
      *
      *   Rev 2.2   Apr 07 2003 10:12:46   t13ex27
      *AKI00347 - DB-Änderung Fix 38
      *
      *   Rev 2.1   Apr 16 1999 10:07:44   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:34   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-LEBENRECHENWERTE-GESAMT.
         10 EA-NI-LEBENRECHENWERTE-TAB.
           20 EA-NI-LEBENRECHENWERTE-ELE PIC X OCCURS 45 TIMES.
         10 EA-NI-LEBENRECHENWERTE
                 REDEFINES EA-NI-LEBENRECHENWERTE-TAB.
           20 EA-NI-LRWVERTRAGSID            PIC X.
           20 EA-NI-LRWVERSTARLFDNR          PIC X.
           20 EA-NI-LRWDATUMSTAND            PIC X.
           20 EA-NI-LRWABLEISTVOR            PIC X.
           20 EA-NI-LRWABLEISTVORWKZ         PIC X.
           20 EA-NI-LRWKAPABFVOR             PIC X.
           20 EA-NI-LRWKAPABFVORWKZ          PIC X.
           20 EA-NI-LRWKAPABFGAR             PIC X.
           20 EA-NI-LRWKAPABFGARWKZ          PIC X.
           20 EA-NI-LRWVSRTOD                PIC X.
           20 EA-NI-LRWVSRTODWKZ             PIC X.
           20 EA-NI-LRWUEBETTOD              PIC X.
           20 EA-NI-LRWUEBETODWKZ            PIC X.
           20 EA-NI-LRWBEITRFREIESUM         PIC X.
           20 EA-NI-LRWBETRFSUMWKZ           PIC X.
           20 EA-NI-LRWRKWERTGES             PIC X.
           20 EA-NI-LRWRKWERTGESWKZ          PIC X.
           20 EA-NI-LRWBONUSRENTE            PIC X.
           20 EA-NI-LRWBONUSRENTEWKZ         PIC X.
           20 EA-NI-LRWKAPABFINKDYN          PIC X.
           20 EA-NI-LRWKAPABFINKDWKZ         PIC X.
           20 EA-NI-LRWABLEISTSIM            PIC X.
           20 EA-NI-LRWABLEISTSIMWKZ         PIC X.
           20 EA-NI-LRWVORAUSZDARLBETR       PIC X.
           20 EA-NI-LRWVORAUSZDBWKZ          PIC X.
           20 EA-NI-LRWMOEGLDARLBETR         PIC X.
           20 EA-NI-LRWMOEGLDBWKZ            PIC X.
           20 EA-NI-LRWGBURENTE              PIC X.
           20 EA-NI-LRWGBURENTEWKZ           PIC X.
           20 EA-NI-LRWGBUZVRENTE            PIC X.
           20 EA-NI-LRWGBUZVRENTEWKZ         PIC X.
           20 EA-NI-LRWGUZVSUMME             PIC X.
           20 EA-NI-LRWGUZVSUMMEWKZ          PIC X.
           20 EA-NI-LRWERMABL                PIC X.
           20 EA-NI-LRWGARLSTABL             PIC X.
           20 EA-NI-LRWGARLSTABLWKZ          PIC X.
           20 EA-NI-LRWFORENDITE             PIC X.
           20 EA-NI-LRWZAHLBEITRAG           PIC X.
           20 EA-NI-LRWZAHLBEITRWKZ          PIC X.
           20 EA-NI-LRWGKIRENTE              PIC X.
           20 EA-NI-LRWGKIRENTEWKZ           PIC X.
           20 EA-NI-LRWGPFRENTE              PIC X.
           20 EA-NI-LRWGPFRENTEWKZ           PIC X.
           20 EA-NI-LRWGPFRENTEANW           PIC X.
           20 EA-NI-LRWGPFRENTEANWWKZ        PIC X.
         10 EA-DCL-LEBENRECHENWERTE.
           20 EA-LRWVERTRAGSID               PIC X(13).
           20 EA-LRWVERSTARLFDNR             PIC -----9.
           20 EA-LRWDATUMSTAND               PIC X(010).
           20 EA-LRWABLEISTVOR               PIC ---------9.99.
           20 EA-LRWABLEISTVORWKZ            PIC X(1).
           20 EA-LRWKAPABFVOR                PIC ---------9.99.
           20 EA-LRWKAPABFVORWKZ             PIC X(1).
           20 EA-LRWKAPABFGAR                PIC ---------9.99.
           20 EA-LRWKAPABFGARWKZ             PIC X(1).
           20 EA-LRWVSRTOD                   PIC ---------9.99.
           20 EA-LRWVSRTODWKZ                PIC X(1).
           20 EA-LRWUEBETTOD                 PIC ---------9.99.
           20 EA-LRWUEBETODWKZ               PIC X(1).
           20 EA-LRWBEITRFREIESUM            PIC ---------9.99.
           20 EA-LRWBETRFSUMWKZ              PIC X(1).
           20 EA-LRWRKWERTGES                PIC ---------9.99.
           20 EA-LRWRKWERTGESWKZ             PIC X(1).
           20 EA-LRWBONUSRENTE               PIC ---------9.99.
           20 EA-LRWBONUSRENTEWKZ            PIC X(1).
           20 EA-LRWKAPABFINKDYN             PIC ---------9.99.
           20 EA-LRWKAPABFINKDWKZ            PIC X(1).
           20 EA-LRWABLEISTSIM               PIC ---------9.99.
           20 EA-LRWABLEISTSIMWKZ            PIC X(1).
           20 EA-LRWVORAUSZDARLBETR          PIC ---------9.99.
           20 EA-LRWVORAUSZDBWKZ             PIC X(1).
           20 EA-LRWMOEGLDARLBETR            PIC ---------9.99.
           20 EA-LRWMOEGLDBWKZ               PIC X(1).
           20 EA-LRWGBURENTE                 PIC -------9.99.
           20 EA-LRWGBURENTEWKZ              PIC X(1).
           20 EA-LRWGBUZVRENTE               PIC -------9.99.
           20 EA-LRWGBUZVRENTEWKZ            PIC X(1).
           20 EA-LRWGUZVSUMME                PIC -----------9.99.
           20 EA-LRWGUZVSUMMEWKZ             PIC X(1).
           20 EA-LRWERMABL                   PIC X(010).
           20 EA-LRWGARLSTABL                PIC -----------9.99.
           20 EA-LRWGARLSTABLWKZ             PIC X(1).
           20 EA-LRWFORENDITE                PIC ---9.99.
           20 EA-LRWZAHLBEITRAG              PIC ---------9.99.
           20 EA-LRWZAHLBEITRWKZ             PIC X(1).
           20 EA-LRWGKIRENTE                 PIC ---------9.99.
           20 EA-LRWGKIRENTEWKZ              PIC X(1).
           20 EA-LRWGPFRENTE                 PIC ---------9.99.
           20 EA-LRWGPFRENTEWKZ              PIC X(1).
           20 EA-LRWGPFRENTEANW              PIC ---------9.99.
           20 EA-LRWGPFRENTEANWWKZ           PIC X(1).
      *****************************************************************
