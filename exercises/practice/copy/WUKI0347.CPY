      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.09.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0347.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0347.CPv  $
      *
      *   Rev 2.1   Oct 17 2005 10:07:50   t13ex27
      *AKI00521: DB-Änderung für Fix 80 PMKI0079 und WUKI's
      *
      *   Rev 2.0   Sep 20 2005 09:07:06   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-VERSGEFAHRBEZ.
           10 VEGVERTRAGSID               PIC X(13).
           10 VEGVERSTARLFDNR             PIC S9(4) COMP.
           10 VEGGEFAHRCODE               PIC S9(4) COMP.
           10 VEGSELBSTBETEIL             PIC S9(9)V9(2) COMP-3.
           10 NIL-VEGSELBSTBETEIL         PIC X(01).
           10 VEGSELBSTBETWKZ             PIC X(1).
           10 VEGTARIFZONE                PIC S9(4) COMP.
           10 NIL-VEGTARIFZONE            PIC X(01).
           10 VEGSBVSUMME                 PIC S9(2)V COMP-3.
           10 NIL-VEGSBVSUMME             PIC X(01).
           10 VEGSBSCHADEN                PIC S9(2)V COMP-3.
           10 NIL-VEGSBSCHADEN            PIC X(01).
           10 VEGSBMIN                    PIC S9(5)V COMP-3.
           10 NIL-VEGSBMIN                PIC X(01).
           10 VEGSBMAX                    PIC S9(5)V COMP-3.
           10 NIL-VEGSBMAX                PIC X(01).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
           10 VEGKZSBNL                   PIC X(1).
           10 NIL-VEGKZSBNL               PIC X(01).
      *****************************************************************
