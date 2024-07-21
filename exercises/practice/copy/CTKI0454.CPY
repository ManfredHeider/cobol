      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05/09/12
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0454.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0454.CPv  $
      *
      *   Rev 2.2   Sep 06 2012 19:49:28   t13ex39
      *AKI01175: DeuBa-Anspracheschutz: Anpassungen in DeuBa-Aktual
      *isierung
      *
      *   Rev 2.1   Sep 06 2012 19:41:08   t13ex39
      *AKI01175: DeuBa-Anspracheschutz: Anpassungen in DeuBa-Aktual
      *isierung
      *
      *   Rev 2.0   Sep 5 2012 07:37:57   T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE ANSPRSCHUTZDB TABLE
CCC004*    ( ASSVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      ASSGESNR                       SMALLINT NOT NULL,
CCC004*      ASSVNR                         CHAR(15) NOT NULL,
CCC004*      ASSABSCHLVBNR                  INTEGER,
CCC004*      ASSERSTELLTTST                 TIMESTAMP NOT NULL,
CCC004*      ASSERLKZ                       CHAR(1) NOT NULL,
CCC004*      ASSERLDAT                      DATE,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE ANSPRSCHUTZDB               *
      ******************************************************************
       01  ANSPRSCHUTZDB-GESAMT.
        05 NI-ANSPRSCHUTZDB-TAB.
           10 NI-ANSPRSCHUTZDB-ELE PIC S9(04) COMP OCCURS 8 TIMES.
        05 NI-ANSPRSCHUTZDB REDEFINES NI-ANSPRSCHUTZDB-TAB.
           10 NI-ASSVERTRAGSID      PIC S9(4) COMP.
           10 NI-ASSGESNR           PIC S9(4) COMP.
           10 NI-ASSVNR             PIC S9(4) COMP.
           10 NI-ASSABSCHLVBNR      PIC S9(4) COMP.
           10 NI-ASSERSTELLTTST     PIC S9(4) COMP.
           10 NI-ASSERLKZ           PIC S9(4) COMP.
           10 NI-ASSERLDAT          PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-ANSPRSCHUTZDB.
           10 ASSVERTRAGSID        PIC X(13).
           10 ASSGESNR             PIC S9(4) USAGE COMP.
           10 ASSVNR               PIC X(15).
           10 ASSABSCHLVBNR        PIC S9(9) USAGE COMP.
           10 ASSERSTELLTTST       PIC X(26).
           10 ASSERLKZ             PIC X(1).
           10 ASSERLDAT            PIC X(10).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************
