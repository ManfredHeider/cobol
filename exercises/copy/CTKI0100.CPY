      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.02.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0100.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0100.CPv  $
      *
      *   Rev 2.1   Feb 11 2004 12:50:12   t13ex27
      *AKI00403 - DB-Änderung Fix 57
      *
      *   Rev 2.0   Feb 10 2004 08:44:36   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE BESGRUPPESTUFE
      ******************************************************************
CCC005*    EXEC SQL DECLARE BESGRUPPESTUFE TABLE
CCC004*    ( BGSKEY                         SMALLINT NOT NULL,
CCC004*      BGSGRUPPE                      CHAR(5) NOT NULL,
CCC004*      BGSSTUFE                       CHAR(5) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  BESGRUPPESTUFE
      ******************************************************************
       01  BESGRUPPESTUFE-GESAMT.
         05 NI-BESGRUPPESTUFE-TAB.
           10 NI-BESGRUPPESTUFE-ELE PIC S9(4) COMP OCCURS 3 TIMES.
         05 NI-BESGRUPPESTUFE REDEFINES NI-BESGRUPPESTUFE-TAB.
           10 NI-BGSKEY             PIC S9(4) COMP.
           10 NI-BGSGRUPPE          PIC S9(4) COMP.
           10 NI-BGSSTUFE           PIC S9(4) COMP.
         05 DCL-BESGRUPPESTUFE.
           10 BGSKEY                PIC S9(4) COMP.
           10 BGSGRUPPE             PIC X(5).
           10 BGSSTUFE              PIC X(5).
      ******************************************************************