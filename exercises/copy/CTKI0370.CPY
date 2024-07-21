      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0370.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0370.CPv  $
      *
      *   Rev 2.1   Jun 23 2016 08:10:02   t13ae31
      *AKI01426: G10 SUH VSP Reiseversicherung OZP
      *
      *   Rev 1.1   09 May 1998 13:30:18   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:01:06   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE REISEVERSICHRUNG TABLE
CCC004*    ( RVSVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      RVSVETLFDNR                    SMALLINT NOT NULL,
CCC004*      RVSEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      RVSBEDINGUNG_KTY               SMALLINT,
CCC004*      RVSBEDINGUNG_KNR               DECIMAL(15, 0),
CCC004*      RVSVSUMRGEPCK                  DECIMAL(11, 2),
CCC004*      RVSVSUMRUECK                   DECIMAL(11, 2),
CCC004*      RVSMAILING_KTY                 SMALLINT,
CCC004*      RVSMAILING_KNR                 DECIMAL(15, 0),
CCC004*      RVSZUSINFO_KTY                 SMALLINT,
CCC004*      RVSZUSINFO_KNR                 DECIMAL(15, 0),
CCC004*      RVSINFOQUELLE_KTY              SMALLINT,
CCC004*      RVSINFOQUELLE_KNR              DECIMAL(15, 0),
CCC004*      RVSDIFDECKKZ                   CHAR(1) NOT NULL,
CCC004*      RVSGESELLSCH_KTY               SMALLINT,
CCC004*      RVSGESELLSCH_KNR               DECIMAL(15, 0),
CCC004*      RVSVERSNUMMER                  CHAR(40),
CCC004*      RVSVUMSTELLUNGDAT              DATE,
CCC004*      RVSKUENDIGUNGKZ                CHAR(1) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE REISEVERSICHRUNG            *
      ******************************************************************
       01  REISEVERSICHRUNG-GESAMT.
        05 NI-REISEVERSICHRUNG-TAB.
           10 NI-REISEVERSICHRUNG-ELE PIC S9(04) COMP OCCURS 20 TIMES.
        05 NI-REISEVERSICHRUNG REDEFINES NI-REISEVERSICHRUNG-TAB.
           10 NI-RVSVERTRAGSID      PIC S9(4) COMP.
           10 NI-RVSVETLFDNR        PIC S9(4) COMP.
           10 NI-RVSEINZVTGHOSTID   PIC S9(4) COMP.
           10 NI-RVSBEDINGUNG-KTY   PIC S9(4) COMP.
           10 NI-RVSBEDINGUNG-KNR   PIC S9(4) COMP.
           10 NI-RVSVSUMRGEPCK      PIC S9(4) COMP.
           10 NI-RVSVSUMRUECK       PIC S9(4) COMP.
           10 NI-RVSMAILING-KTY     PIC S9(4) COMP.
           10 NI-RVSMAILING-KNR     PIC S9(4) COMP.
           10 NI-RVSZUSINFO-KTY     PIC S9(4) COMP.
           10 NI-RVSZUSINFO-KNR     PIC S9(4) COMP.
           10 NI-RVSINFOQUELLE-KTY  PIC S9(4) COMP.
           10 NI-RVSINFOQUELLE-KNR  PIC S9(4) COMP.
           10 NI-RVSDIFDECKKZ       PIC S9(4) COMP.
           10 NI-RVSGESELLSCH-KTY   PIC S9(4) COMP.
           10 NI-RVSGESELLSCH-KNR   PIC S9(4) COMP.
           10 NI-RVSVERSNUMMER      PIC S9(4) COMP.
           10 NI-RVSVUMSTELLUNGDAT  PIC S9(4) COMP.
           10 NI-RVSKUENDIGUNGKZ    PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-REISEVERSICHRUNG.
           10 RVSVERTRAGSID        PIC X(13).
           10 RVSVETLFDNR          PIC S9(4) USAGE COMP.
           10 RVSEINZVTGHOSTID     PIC X(15).
           10 RVSBEDINGUNG-KTY     PIC S9(4) USAGE COMP.
           10 RVSBEDINGUNG-KNR     PIC S9(15)V USAGE COMP-3.
           10 RVSVSUMRGEPCK        PIC S9(9)V9(2) USAGE COMP-3.
           10 RVSVSUMRUECK         PIC S9(9)V9(2) USAGE COMP-3.
           10 RVSMAILING-KTY       PIC S9(4) USAGE COMP.
           10 RVSMAILING-KNR       PIC S9(15)V USAGE COMP-3.
           10 RVSZUSINFO-KTY       PIC S9(4) USAGE COMP.
           10 RVSZUSINFO-KNR       PIC S9(15)V USAGE COMP-3.
           10 RVSINFOQUELLE-KTY    PIC S9(4) USAGE COMP.
           10 RVSINFOQUELLE-KNR    PIC S9(15)V USAGE COMP-3.
           10 RVSDIFDECKKZ         PIC X(1).
           10 RVSGESELLSCH-KTY     PIC S9(4) USAGE COMP.
           10 RVSGESELLSCH-KNR     PIC S9(15)V USAGE COMP-3.
           10 RVSVERSNUMMER        PIC X(40).
           10 RVSVUMSTELLUNGDAT    PIC X(10).
           10 RVSKUENDIGUNGKZ      PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************

