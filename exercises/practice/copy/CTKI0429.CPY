      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0429.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0429.CPv  $
      *
      *   Rev 2.3   Mar 18 2015 15:11:26   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 MultiRow OZP ZM
      *KI0429
      *
      *   Rev 2.2   15 May 2008 12:40:22   t13ex27
      *AKL00665: Allfinanz; AMV-DB-Änderung
      *
      *   Rev 2.1   17 Oct 2007 11:08:30   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-DCLGENs und DB-Änderung
      *
      *   Rev 2.0   27 Sep 2007 12:03:08   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE VERTRAGSRISIKO
      ******************************************************************
CCC005*    EXEC SQL DECLARE VERTRAGSRISIKO TABLE
CCC004*    ( VRIVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VRIVETLFDNR                    SMALLINT NOT NULL,
CCC004*      VRIRISIKOGRUPPID               SMALLINT NOT NULL,
CCC004*      VRITHEMGEBSL_KTY               SMALLINT NOT NULL,
CCC004*      VRITHEMGEBSL_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      VRIRISIKOLFDNR                 SMALLINT NOT NULL,
CCC004*      VRIEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      VRIRSKKENNZIFFER               CHAR(10) NOT NULL,
CCC004*      VRIRSKNAME                     CHAR(50) NOT NULL,
CCC004*      VRIRSKBETRAG                   DECIMAL(9, 2) NOT NULL,
CCC004*      VRIRSKSUMME                    DECIMAL(11, 2) NOT NULL,
CCC004*      VRIRSKENTSCHGREN               DECIMAL(10, 2) NOT NULL,
CCC004*      VRIRSKENTGRZ_KTY               SMALLINT NOT NULL,
CCC004*      VRIRSKENTGRZ_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      VRIRSKMENGE                    INTEGER NOT NULL,
CCC004*      VRIRSKMENGEEINH                CHAR(25) NOT NULL,
CCC004*      VRIRSKSATZ                     DECIMAL(10, 7) NOT NULL,
CCC004*      VRIRSKSATZEINH                 INTEGER NOT NULL,
CCC004*      VRIRSKTEXT                     CHAR(30) NOT NULL,
CCC004*      VRIRSKMITVERSKZ                CHAR(2) NOT NULL,
CCC004*      VRIRSKSUMMEEINH                CHAR(20) NOT NULL,
CCC004*      VRIRSKSUMUNBEGRZ               CHAR(2) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      VRIMERKMAL                     DECIMAL(9, 0) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  VERTRAGSRISIKO
      ******************************************************************
       01  VERTRAGSRISIKO-GESAMT.
         05 NI-VERTRAGSRISIKO-TAB.
           10 NI-VERTRAGSRISIKO-ELE PIC S9(4) COMP OCCURS 33 TIMES.
         05 NI-VERTRAGSRISIKO REDEFINES NI-VERTRAGSRISIKO-TAB.
           10 NI-VRIVERTRAGSID        PIC S9(4) COMP.
           10 NI-VRIVETLFDNR          PIC S9(4) COMP.
           10 NI-VRIRISIKOGRUPPID     PIC S9(4) COMP.
           10 NI-VRITHEMGEBSL-KTY     PIC S9(4) COMP.
           10 NI-VRITHEMGEBSL-KNR     PIC S9(4) COMP.
           10 NI-VRIRISIKOLFDNR       PIC S9(4) COMP.
           10 NI-VRIEINZVTGHOSTID     PIC S9(4) COMP.
           10 NI-VRIRSKKENNZIFFER     PIC S9(4) COMP.
           10 NI-VRIRSKNAME           PIC S9(4) COMP.
           10 NI-VRIRSKBETRAG         PIC S9(4) COMP.
           10 NI-VRIRSKSUMME          PIC S9(4) COMP.
           10 NI-VRIRSKENTSCHGREN     PIC S9(4) COMP.
           10 NI-VRIRSKENTGRZ-KTY     PIC S9(4) COMP.
           10 NI-VRIRSKENTGRZ-KNR     PIC S9(4) COMP.
           10 NI-VRIRSKMENGE          PIC S9(4) COMP.
           10 NI-VRIRSKMENGEEINH      PIC S9(4) COMP.
           10 NI-VRIRSKSATZ           PIC S9(4) COMP.
           10 NI-VRIRSKSATZEINH       PIC S9(4) COMP.
           10 NI-VRIRSKTEXT           PIC S9(4) COMP.
           10 NI-VRIRSKMITVERSKZ      PIC S9(4) COMP.
           10 NI-VRIRSKSUMMEEINH      PIC S9(4) COMP.
           10 NI-VRIRSKSUMUNBEGRZ     PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG      PIC S9(4) COMP.
           10 NI-VRIMERKMAL           PIC S9(4) COMP.
         05 DCL-VERTRAGSRISIKO.
           10 VRIVERTRAGSID        PIC X(13).
           10 VRIVETLFDNR          PIC S9(4) COMP.
           10 VRIRISIKOGRUPPID     PIC S9(4) COMP.
           10 VRITHEMGEBSL-KTY     PIC S9(4) COMP.
           10 VRITHEMGEBSL-KNR     PIC S9(15)V COMP-3.
           10 VRIRISIKOLFDNR       PIC S9(4) COMP.
           10 VRIEINZVTGHOSTID     PIC X(15).
           10 VRIRSKKENNZIFFER     PIC X(10).
           10 VRIRSKNAME           PIC X(50).
           10 VRIRSKBETRAG         PIC S9(7)V9(2) COMP-3.
           10 VRIRSKSUMME          PIC S9(9)V9(2) COMP-3.
           10 VRIRSKENTSCHGREN     PIC S9(8)V9(2) COMP-3.
           10 VRIRSKENTGRZ-KTY     PIC S9(4) COMP.
           10 VRIRSKENTGRZ-KNR     PIC S9(15)V COMP-3.
           10 VRIRSKMENGE          PIC S9(9) COMP.
           10 VRIRSKMENGEEINH      PIC X(25).
           10 VRIRSKSATZ           PIC S9(3)V9(7) COMP-3.
           10 VRIRSKSATZEINH       PIC S9(9) COMP.
           10 VRIRSKTEXT           PIC X(30).
           10 VRIRSKMITVERSKZ      PIC X(2).
           10 VRIRSKSUMMEEINH      PIC X(20).
           10 VRIRSKSUMUNBEGRZ     PIC X(2).
           10 LETZTEAENDERUNG      PIC X(26).
           10 VRIMERKMAL           PIC S9(9)V COMP-3.
      ******************************************************************

