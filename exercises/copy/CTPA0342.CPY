      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0342.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTPA0342.CPv  $
      *
      *   Rev 2.1   Feb 13 2018 11:28:34   p13sp14
      *ADB00047: Normalisierung von PDPT_PDPE_DATEN: Aktualisierung
      *sprogramm
      *
      *   Rev 2.0   Feb 13 2018 11:24:50   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(T13AES.TBPA0342)                                  *
      *        LIBRARY(P13SP54.USERTEST.SRCDCL(CTPA0342))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        INDVAR(YES)                                             *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE        TBPA0342 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PDPT_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PDPT_PDPE_NR                   SMALLINT NOT NULL,
CCC004*      PDPE_ST_NR                     INTEGER NOT NULL,
CCC004*      PDPE_NT_PBTRG_BRWG             DECIMAL(11, 2) NOT NULL,
CCC004*      PDPE_PROV_RCK_BRWG             DECIMAL(11, 2) NOT NULL,
CCC004*      PDPE_EINH                      DECIMAL(8, 2) NOT NULL,
CCC004*      PDPE_NT_PBTRG_AZWG             DECIMAL(11, 2) NOT NULL,
CCC004*      PDPE_PROV_RCK_AZWG             DECIMAL(11, 2) NOT NULL,
CCC004*      KNR_AUSZ_WAEHRUNG              SMALLINT NOT NULL,
CCC004*      PDPE_BUCH_RELEVANT             CHAR(1) NOT NULL,
CCC004*      PDPE_UST_PBTRG_BRWG            DECIMAL(11, 2) NOT NULL,
CCC004*      PDPE_UST_RCK_BRWG              DECIMAL(11, 2) NOT NULL,
CCC004*      PDPE_UST_PBTRG_AZWG            DECIMAL(11, 2) NOT NULL,
CCC004*      PDPE_UST_RCK_AZWG              DECIMAL(11, 2) NOT NULL,
CCC004*      PDPE_KZ_UST_BER                CHAR(1) NOT NULL,
CCC004*      PDPE_LAST_UPDATE               TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AES.TBPA0342                    *
      ******************************************************************
       01  DCLTBPA0342.
      *    *************************************************************
           10 PGEB-GES             PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PDPT-ID              PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 PDPT-PDPE-NR         PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PDPE-ST-NR           PIC S9(9) USAGE COMP.
      *    *************************************************************
           10 PDPE-NT-PBTRG-BRWG   PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PDPE-PROV-RCK-BRWG   PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PDPE-EINH            PIC S9(6)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PDPE-NT-PBTRG-AZWG   PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PDPE-PROV-RCK-AZWG   PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 KNR-AUSZ-WAEHRUNG    PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PDPE-BUCH-RELEVANT   PIC X(1).
      *    *************************************************************
           10 PDPE-UST-PBTRG-BRWG  PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PDPE-UST-RCK-BRWG    PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PDPE-UST-PBTRG-AZWG  PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PDPE-UST-RCK-AZWG    PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PDPE-KZ-UST-BER      PIC X(1).
      *    *************************************************************
           10 PDPE-LAST-UPDATE     PIC X(26).
      ******************************************************************
      * INDICATOR VARIABLE STRUCTURE                                   *
      ******************************************************************
       01  ITBPA0342.
           10 INDSTRUC           PIC S9(4) USAGE COMP OCCURS 17 TIMES.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************

