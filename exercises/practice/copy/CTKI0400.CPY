      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0400.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0400.CPv  $
      *
      *   Rev 2.1   Mar 03 2005 10:13:02   t13ex27
      *AKI00474: DB-Änderung für Fix 73 CTKIxxxx
      *
      *   Rev 2.0   Mar 03 2005 09:37:50   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE AENDERUNGAVM
      ******************************************************************
CCC005*    EXEC SQL DECLARE AENDERUNGAVM TABLE
CCC004*    ( AAVVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      AAVLAUFENDENUMMER              SMALLINT NOT NULL,
CCC004*      AAVABSCHLVBNEU                 CHAR(13) NOT NULL,
CCC004*      AAVABSCHLVBNEURKZ              CHAR(1) NOT NULL,
CCC004*      AAVABSCHLVBALT                 CHAR(13),
CCC004*      AAVABSCHLVBALTRKZ              CHAR(1),
CCC004*      AAVBEARBEITER                  CHAR(8),
CCC004*      LETZTEAENDERUNG                TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  AENDERUNGAVM
      ******************************************************************
       01  AENDERUNGAVM-GESAMT.
         05 NI-AENDERUNGAVM-TAB.
           10 NI-AENDERUNGAVM-ELE PIC S9(4) COMP OCCURS 8 TIMES.
         05 NI-AENDERUNGAVM REDEFINES NI-AENDERUNGAVM-TAB.
           10 NI-AAVVERTRAGSID        PIC S9(4) COMP.
           10 NI-AAVLAUFENDENUMMER    PIC S9(4) COMP.
           10 NI-AAVABSCHLVBNEU       PIC S9(4) COMP.
           10 NI-AAVABSCHLVBNEURKZ    PIC S9(4) COMP.
           10 NI-AAVABSCHLVBALT       PIC S9(4) COMP.
           10 NI-AAVABSCHLVBALTRKZ    PIC S9(4) COMP.
           10 NI-AAVBEARBEITER        PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG      PIC S9(4) COMP.
         05 DCL-AENDERUNGAVM.
           10 AAVVERTRAGSID        PIC X(13).
           10 AAVLAUFENDENUMMER    PIC S9(4) COMP.
           10 AAVABSCHLVBNEU       PIC X(13).
           10 AAVABSCHLVBNEURKZ    PIC X(1).
           10 AAVABSCHLVBALT       PIC X(13).
           10 AAVABSCHLVBALTRKZ    PIC X(1).
           10 AAVBEARBEITER        PIC X(8).
           10 LETZTEAENDERUNG      PIC X(26).
      ******************************************************************
