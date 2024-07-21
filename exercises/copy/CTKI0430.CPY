      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0430.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0430.CPv  $
      *
      *   Rev 2.1   17 Oct 2007 11:08:40   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-DCLGENs und DB-Änderung
      *
      *   Rev 2.0   27 Sep 2007 12:03:18   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE VERTRISIMERKMAL
      ******************************************************************
CCC005*    EXEC SQL DECLARE VERTRISIMERKMAL TABLE
CCC004*    ( VRMVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VRMVETLFDNR                    SMALLINT NOT NULL,
CCC004*      VRMRISIGRUPPEID                SMALLINT NOT NULL,
CCC004*      VRMRISIMERKMALID               SMALLINT NOT NULL,
CCC004*      VRMEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      VRMBEZEICHNUNG                 CHAR(50) NOT NULL,
CCC004*      VRMRISMERKMWERT                CHAR(50) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  VERTRISIMERKMAL
      ******************************************************************
       01  VERTRISIMERKMAL-GESAMT.
         05 NI-VERTRISIMERKMAL-TAB.
           10 NI-VERTRISIMERKMAL-ELE PIC S9(4) COMP OCCURS 8 TIMES.
         05 NI-VERTRISIMERKMAL REDEFINES NI-VERTRISIMERKMAL-TAB.
           10 NI-VRMVERTRAGSID        PIC S9(4) COMP.
           10 NI-VRMVETLFDNR          PIC S9(4) COMP.
           10 NI-VRMRISIGRUPPEID      PIC S9(4) COMP.
           10 NI-VRMRISIMERKMALID     PIC S9(4) COMP.
           10 NI-VRMEINZVTGHOSTID     PIC S9(4) COMP.
           10 NI-VRMBEZEICHNUNG       PIC S9(4) COMP.
           10 NI-VRMRISMERKMWERT      PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG      PIC S9(4) COMP.
         05 DCL-VERTRISIMERKMAL.
           10 VRMVERTRAGSID        PIC X(13).
           10 VRMVETLFDNR          PIC S9(4) COMP.
           10 VRMRISIGRUPPEID      PIC S9(4) COMP.
           10 VRMRISIMERKMALID     PIC S9(4) COMP.
           10 VRMEINZVTGHOSTID     PIC X(15).
           10 VRMBEZEICHNUNG       PIC X(50).
           10 VRMRISMERKMWERT      PIC X(50).
           10 LETZTEAENDERUNG      PIC X(26).
      ******************************************************************