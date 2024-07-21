      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.06.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0086.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0086.CPv  $
      *
      *   Rev 2.1   Jun 11 2003 13:08:38   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.0   Jun 05 2003 10:08:52   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-SONSTIGESEINKOMMEN.
           10 SEINKOPERSONENID            PIC X(13).
           10 SEINKOPERSROLLE             PIC X(1).
           10 SEINKOLAUFENDENNUM          PIC S9(4) COMP.
           10 SEINKOBRUTTOEINKO           PIC S9(9)V9(2) COMP-3.
           10 NIL-SEINKOBRUTTOEINKO       PIC X(01).
           10 SEINKONETTOEINKO            PIC S9(9)V9(2) COMP-3.
           10 NIL-SEINKONETTOEINKO        PIC X(01).
           10 SEINKOANZAHLMONATE          PIC S9(4) COMP.
           10 NIL-SEINKOANZAHLMONATE      PIC X(01).
           10 SEINKOART                   PIC X(30).
           10 NIL-SEINKOART               PIC X(01).
      *****************************************************************
