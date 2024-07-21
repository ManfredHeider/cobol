      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0009.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0009.CPv  $
      *
      *   Rev 2.1   Feb 19 2018 07:42:50   t13ac20
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 1.1   09 May 1998 13:26:44   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:52:14   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE PRODKLASSETARIF TABLE
CCC004*    ( PKTPRODKLAS_KTY                SMALLINT NOT NULL,
CCC004*      PKTPRODKLAS_KNR                DECIMAL(15, 0) NOT NULL,
CCC004*      PKTGSMTTARIF_BEZ               CHAR(25) NOT NULL,
CCC004*      PKTPRODUKTID                   SMALLINT NOT NULL,
CCC004*      PKTSTVD0067_KTY                SMALLINT NOT NULL,
CCC004*      PKTSTVD0067_KNR                DECIMAL(15, 0)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE PRODKLASSETARIF             *
      ******************************************************************
       01  PRODKLASSETARIF-GESAMT.
        05 NI-PRODKLASSETARIF-TAB.
           10 NI-PRODKLASSETARIF-ELE PIC S9(04) COMP OCCURS 6 TIMES.
        05 NI-PRODKLASSETARIF REDEFINES NI-PRODKLASSETARIF-TAB.
           10 NI-PKTPRODKLAS-KTY    PIC S9(4) COMP.
           10 NI-PKTPRODKLAS-KNR    PIC S9(4) COMP.
           10 NI-PKTGSMTTARIF-BEZ   PIC S9(4) COMP.
           10 NI-PKTPRODUKTID       PIC S9(4) COMP.
           10 NI-PKTSTVD0067-KTY    PIC S9(4) COMP.
           10 NI-PKTSTVD0067-KNR    PIC S9(4) COMP.
        05 DCL-PRODKLASSETARIF.
           10 PKTPRODKLAS-KTY      PIC S9(4) USAGE COMP.
           10 PKTPRODKLAS-KNR      PIC S9(15)V USAGE COMP-3.
           10 PKTGSMTTARIF-BEZ     PIC X(25).
           10 PKTPRODUKTID         PIC S9(4) USAGE COMP.
           10 PKTSTVD0067-KTY      PIC S9(4) USAGE COMP.
           10 PKTSTVD0067-KNR      PIC S9(15)V USAGE COMP-3.
      ************************** ENDE-DCLGEN *************************

