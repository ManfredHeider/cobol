      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPV0005.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTPV0005.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:49:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   23 Dec 1998 13:20:16   T13AE34
      *APA00035 PROGRAMMVERWALTUNG
      *
      *   Rev 1.0   Nov 04 1995 11:50:32   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPV0005)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTPV0005))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPV0005 TABLE
CCC004*    ( CPY_ID                    CHAR(8) NOT NULL,
CCC004*      CPY_BESCHR                CHAR(150) NOT NULL,
CCC004*      KNR_DR_CPY_VERW           SMALLINT NOT NULL WITH DEFAULT,
CCC004*      KTY_DR_CPY_VERW           SMALLINT NOT NULL WITH DEFAULT,
CCC004*      CPY_FUNKTIONS_ID          CHAR (8) NOT NULL WITH DEFAULT,
CCC004*      PM_ID                     CHAR (8) NOT NULL WITH DEFAULT,
CCC004*      CPY_LAST_UPDATE           TIMESTAMP NOT NULL WITH DEFAULT,
CCC004*      CPY_KENNUNG               CHAR (8) NOT NULL WITH DEFAULT
970908*      CPY_UEBERGABE                CHAR(1) NOT NULL,
970908*      CPY_TUPEL                    CHAR(1) NOT NULL,
970908*      CPY_KONSTANTE                CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPV0005                           *
      ******************************************************************
970908 01  DCL-TBPV0005.
           10 CPY-ID                    PIC X(8).
           10 CPY-BESCHR                PIC X(150).
970908     10 KNR-DR-CPY-VERW           PIC S9(4) COMP.
970908     10 KTY-DR-CPY-VERW           PIC S9(4) COMP.
970908     10 CPY-FUNKTIONS-ID          PIC X(8).
970908     10 PM-ID                     PIC X(8).
970908     10 CPY-LAST-UPDATE           PIC X(26).
970908     10 CPY-KENNUNG               PIC X(8).
      *    10 CPY-UEBERGABE        PIC X(1).
      *    10 CPY-TUPEL            PIC X(1).
      *    10 CPY-KONSTANTE        PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
