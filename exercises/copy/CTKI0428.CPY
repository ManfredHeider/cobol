      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0428.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0428.CPv  $
      *
      *   Rev 2.1   17 Oct 2007 11:08:18   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-DCLGENs und DB-Änderung
      *
      *   Rev 2.0   27 Sep 2007 12:02:58   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE VERTRAGSLEISTUNG
      ******************************************************************
CCC005*    EXEC SQL DECLARE VERTRAGSLEISTUNG TABLE
CCC004*    ( VLTVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VLTVETLFDNR                    SMALLINT NOT NULL,
CCC004*      VLTRISIGRUPPEID                SMALLINT NOT NULL,
CCC004*      VLTLEISTUNGID                  SMALLINT NOT NULL,
CCC004*      VLTEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      VLTTHEMGSCHL_KTY               SMALLINT NOT NULL,
CCC004*      VLTTHEMGSCHL_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      VLTBEZEICHNUNG                 CHAR(50) NOT NULL,
CCC004*      VLTWERT                        DECIMAL(11, 2) NOT NULL,
CCC004*      VLTSONSTWERT                   DECIMAL(10, 2) NOT NULL,
CCC004*      VLTSONSTWERTEINH               CHAR(20) NOT NULL,
CCC004*      VLTLEISTUNGSTXT                CHAR(30) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  VERTRAGSLEISTUNG
      ******************************************************************
       01  VERTRAGSLEISTUNG-GESAMT.
         05 NI-VERTRAGSLEISTUNG-TAB.
           10 NI-VERTRAGSLEISTUNG-ELE PIC S9(4) COMP OCCURS 13 TIMES.
         05 NI-VERTRAGSLEISTUNG REDEFINES NI-VERTRAGSLEISTUNG-TAB.
           10 NI-VLTVERTRAGSID        PIC S9(4) COMP.
           10 NI-VLTVETLFDNR          PIC S9(4) COMP.
           10 NI-VLTRISIGRUPPEID      PIC S9(4) COMP.
           10 NI-VLTLEISTUNGID        PIC S9(4) COMP.
           10 NI-VLTEINZVTGHOSTID     PIC S9(4) COMP.
           10 NI-VLTTHEMGSCHL-KTY     PIC S9(4) COMP.
           10 NI-VLTTHEMGSCHL-KNR     PIC S9(4) COMP.
           10 NI-VLTBEZEICHNUNG       PIC S9(4) COMP.
           10 NI-VLTWERT              PIC S9(4) COMP.
           10 NI-VLTSONSTWERT         PIC S9(4) COMP.
           10 NI-VLTSONSTWERTEINH     PIC S9(4) COMP.
           10 NI-VLTLEISTUNGSTXT      PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG      PIC S9(4) COMP.
         05 DCL-VERTRAGSLEISTUNG.
           10 VLTVERTRAGSID        PIC X(13).
           10 VLTVETLFDNR          PIC S9(4) COMP.
           10 VLTRISIGRUPPEID      PIC S9(4) COMP.
           10 VLTLEISTUNGID        PIC S9(4) COMP.
           10 VLTEINZVTGHOSTID     PIC X(15).
           10 VLTTHEMGSCHL-KTY     PIC S9(4) COMP.
           10 VLTTHEMGSCHL-KNR     PIC S9(15)V COMP-3.
           10 VLTBEZEICHNUNG       PIC X(50).
           10 VLTWERT              PIC S9(9)V9(2) COMP-3.
           10 VLTSONSTWERT         PIC S9(8)V9(2) COMP-3.
           10 VLTSONSTWERTEINH     PIC X(20).
           10 VLTLEISTUNGSTXT      PIC X(30).
           10 LETZTEAENDERUNG      PIC X(26).
      ******************************************************************