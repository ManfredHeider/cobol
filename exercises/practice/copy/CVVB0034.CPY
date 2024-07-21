      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.04.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0034.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVVB0034.CPv  $
      *
      *   Rev 2.2   Nov 29 2005 17:28:10   t13ae30
      *AVB00827: Neuauflage Riester-WB mit Neukundenausprägung
      *
      *   Rev 2.1   May 18 2004 13:40:16   t13ae30
      *Verkaufsförderung Riesterprodukte
      *
      *   Rev 2.0   Apr 05 2004 11:41:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0034)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0034))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0034 TABLE
CCC004*    (  RINFO_VBNR                     INT       NOT NULL,
CCC004*       RINFO_VERSNR                   CHAR (10) NOT NULL,
CCC004*       RINFO_MONAT                    DATE      NOT NULL,
CCC004*       RINFO_KZ                       CHAR (02) NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       RINFO_LAST_UPDATE              TIMESTAMP NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       RINFO_LEISTG1                  DATE,
CCC004*       PF_KZ_LEISTG1                  CHAR (01) NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       RINFO_LEISTG2                  DATE,
CCC004*       PF_KZ_LEISTG2                  CHAR (01) NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       RINFO_LEISTG3                  DATE,
CCC004*       PF_KZ_LEISTG3                  CHAR (01) NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       RINFO_LEISTG4                  DATE,
CCC004*       PF_KZ_LEISTG4                  CHAR (01) NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       RINFO_BEMERKUNG                CHAR (20),
CCC004*       RINFO_NEUKDN_KZ                CHAR (01)

CCC003*    )  END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0034                           *
      ******************************************************************
       01  VIVB0034-GESAMT.
        05  NI-VIVB0034-TAB.
           10  NI-VIVB0034-ELE     PIC S9(04)  COMP   OCCURS  15 TIMES.
        05  NI-VIVB0034            REDEFINES   NI-VIVB0034-TAB.
           10 NI-RINFO-VBNR        PIC S9(04)  COMP.
           10 NI-RINFO-VERSNR      PIC S9(04)  COMP.
           10 NI-RINFO-MONAT       PIC S9(04)  COMP.
           10 NI-RINFO-KZ          PIC S9(04)  COMP.
           10 NI-RINFO-LAST-UPDATE PIC S9(04)  COMP.
           10 NI-RINFO-LEISTG1     PIC S9(04)  COMP.
           10 NI-PF-KZ-LEISTG1     PIC S9(04)  COMP.
           10 NI-RINFO-LEISTG2     PIC S9(04)  COMP.
           10 NI-RINFO-NEUKDN-KZ   PIC S9(04)  COMP.
           10 NI-PF-KZ-LEISTG2     PIC S9(04)  COMP.
           10 NI-RINFO-LEISTG3     PIC S9(04)  COMP.
           10 NI-PF-KZ-LEISTG3     PIC S9(04)  COMP.
           10 NI-RINFO-LEISTG4     PIC S9(04)  COMP.
           10 NI-PF-KZ-LEISTG4     PIC S9(04)  COMP.
           10 NI-RINFO-BEMERKUNG   PIC S9(04)  COMP.
        05 DCL-VIVB0034.
           10 RINFO-VBNR                     PIC S9(9) COMP.
           10 RINFO-VERSNR                   PIC X(10).
           10 RINFO-MONAT                    PIC X(10).
           10 RINFO-KZ                       PIC X(02).
           10 RINFO-LAST-UPDATE              PIC X(26).
           10 RINFO-LEISTG1                  PIC X(10).
           10 PF-KZ-LEISTG1                  PIC X(01).
           10 RINFO-LEISTG2                  PIC X(10).
           10 RINFO-NEUKDN-KZ                PIC X(01).
           10 PF-KZ-LEISTG2                  PIC X(01).
           10 RINFO-LEISTG3                  PIC X(10).
           10 PF-KZ-LEISTG3                  PIC X(01).
           10 RINFO-LEISTG4                  PIC X(10).
           10 PF-KZ-LEISTG4                  PIC X(01).
           10 RINFO-BEMERKUNG                PIC X(20).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************
