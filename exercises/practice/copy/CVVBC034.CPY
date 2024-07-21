      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.10.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBC034.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVVBC034.CPv  $
      *
      *   Rev 2.2   May 11 2011 11:48:40   t13ae30
      *ASL00378: Update Storno-KZ von 'J' auf 'F' für bereits im WB
      *W='81' gekennzeichnete Verträge
      *
      *   Rev 2.1   Nov 09 2009 11:15:38   t13ex13
      *AAS00859: VSP/K-Wettbewerb 2009
      *
      *   Rev 2.0   Oct 14 2009 10:52:24   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * AENDERUNGEN:                                                   *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME       *
      * ---------------------------------------------------------------*
      * AAS00859!16.01.07!2.0 ! NEUERSTELLUNG             ! GUTFLEISCH *
      * ASL00378!11.02.11!2.1 ! FEHLENDES ATTRIBUT EINGEF.! TH.WILLIG  *
      *         !        !    !                           !            *
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      * DCLGEN TABLE(VIVBC034)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBC034))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBC034 TABLE
CCC004*    (  RINFO_VBNR                     INT       NOT NULL,
CCC004*       RINFO_VERSNR                   CHAR (15) NOT NULL,
CCC004*       RINFO_MONAT                    DATE      NOT NULL,
CCC004*       RINFO_KZ                       CHAR (02) NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       KTY_WB_TYP                     SMALLINT NOT NULL,
CCC004*       KNR_WB_TYP                     SMALLINT NOT NULL,
CCC004*       RINFO_LAST_UPDATE              TIMESTAMP NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       RINFO_LEISTG1                  DATE,
CCC004*       PF_KZ_LEISTG1                  CHAR (01) NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       RINFO_LEISTG2                  DATE,
CCC004*       PF_KZ_LEISTG2                  CHAR (01) NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       RINFO_NEUKDN_KZ                CHAR (01),
CCC004*       RINFO_LEISTG3                  DATE,
CCC004*       PF_KZ_LEISTG3                  CHAR (01) NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       RINFO_LEISTG4                  DATE,
CCC004*       PF_KZ_LEISTG4                  CHAR (01) NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       RINFO_BEMERKUNG                CHAR (20),
CCC004*       RINFO_STORNO-KZ                CHAR (01),
CCC004*       RINFO_LEISTG5                  DATE,
CCC004*       PF_KZ_LEISTG5                  CHAR (01) NOT NULL
CCC004*                                      WITH DEFAULT

CCC003*    )  END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBC034                           *
      ******************************************************************
       01  VIVBC034-GESAMT.
        05  NI-VIVBC034-TAB.
           10  NI-VIVBC034-ELE     PIC S9(04)  COMP   OCCURS  20 TIMES.
        05  NI-VIVBC034            REDEFINES   NI-VIVBC034-TAB.
           10 NI-RINFO-VBNR        PIC S9(04)  COMP.
           10 NI-RINFO-VERSNR      PIC S9(04)  COMP.
           10 NI-RINFO-MONAT       PIC S9(04)  COMP.
           10 NI-KTY-WB-TYP        PIC S9(04)  COMP.
           10 NI-KNR-WB-TYP        PIC S9(04)  COMP.
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
110311     10 NI-RINFO-STORNO-KZ   PIC S9(04)  COMP.
           10 NI-RINFO-LEISTG5     PIC S9(04)  COMP.
           10 NI-PF-KZ-LEISTG5     PIC S9(04)  COMP.
        05 DCL-VIVBC034.
           10 RINFO-VBNR                     PIC S9(9) COMP.
           10 RINFO-VERSNR                   PIC X(15).
           10 RINFO-MONAT                    PIC X(10).
           10 KTY-WB-TYP                     PIC S9(4) COMP.
           10 KNR-WB-TYP                     PIC S9(4) COMP.
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
110311     10 RINFO-STORNO-KZ                PIC X(01).
           10 RINFO-LEISTG5                  PIC X(10).
           10 PF-KZ-LEISTG5                  PIC X(01).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 20      *
      ******************************************************************
