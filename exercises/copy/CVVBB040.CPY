      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.06.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBB040.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBB040.CPv  $
      *
      *   Rev 2.1   30 Oct 2007 11:06:12   t13ae30
      *AVB00905: Allfinanz - Tabellenerweiterungen TWI
      *
      *   Rev 2.0   05 Jun 2007 15:58:52   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00905!05.06.07!2.0 ! NEU                       ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(VIVBB040)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBB040))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBB040 TABLE
CCC004*    ( KL_ST_NR                       INTEGER NOT NULL,
CCC004*      KL_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      KL_DAT_VON                     DATE NOT NULL,
CCC004*      KL_DAT_BIS                     DATE,
CCC004*      KL_PROV_STZ_AP                 DECIMAL(5, 2) NOT NULL,
CCC004*      KL_PROV_STZ_GP                 DECIMAL(5, 2) NOT NULL,
CCC004*      KL_PROZ_STZ                    DECIMAL(5, 2) NOT NULL,
CCC004*      KL_RCK_STZ_DAT                 DATE NOT NULL,
CCC004*      KL_RCK_STZ                     DECIMAL(5, 2) NOT NULL,
CCC004*      MA_ST_NR                       INTEGER,
CCC004*      KNR_KL_TYP                     SMALLINT NOT NULL,
CCC004*      KTY_KL_TYP                     SMALLINT NOT NULL,
CCC004*      KL_AM_AGENT_NR                 INTEGER
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBB040                           *
      ******************************************************************
       01  VIVBB040-GESAMT.
        05  NI-VIVBB040-TAB.
           10  NI-VIVBB040-ELE     PIC S9(04)  COMP   OCCURS 13  TIMES.
        05  NI-VIVBB040            REDEFINES   NI-VIVBB040-TAB.
           10 NI-KL-ST-NR          PIC S9(04)  COMP.
           10 NI-KL-LAST-UPDATE    PIC S9(04)  COMP.
           10 NI-KL-DAT-VON        PIC S9(04)  COMP.
           10 NI-KL-DAT-BIS        PIC S9(04)  COMP.
           10 NI-KL-PROV-STZ-AP    PIC S9(04)  COMP.
           10 NI-KL-PROV-STZ-GP    PIC S9(04)  COMP.
           10 NI-KL-PROZ-STZ       PIC S9(04)  COMP.
           10 NI-KL-RCK-STZ-DAT    PIC S9(04)  COMP.
           10 NI-KL-RCK-STZ        PIC S9(04)  COMP.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-KNR-KL-TYP        PIC S9(04)  COMP.
           10 NI-KTY-KL-TYP        PIC S9(04)  COMP.
           10 NI-KL-AM-AGENT-NR    PIC S9(04)  COMP.
        05  DCL-VIVBB040.
           10 KL-ST-NR             PIC S9(9) USAGE COMP.
           10 KL-LAST-UPDATE       PIC X(26).
           10 KL-DAT-VON           PIC X(10).
           10 KL-DAT-BIS           PIC X(10).
           10 KL-PROV-STZ-AP       PIC S999V99 USAGE COMP-3.
           10 KL-PROV-STZ-GP       PIC S999V99 USAGE COMP-3.
           10 KL-PROZ-STZ          PIC S999V99 USAGE COMP-3.
           10 KL-RCK-STZ-DAT       PIC X(10).
           10 KL-RCK-STZ           PIC S999V99 USAGE COMP-3.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 KNR-KL-TYP           PIC S9(4) USAGE COMP.
           10 KTY-KL-TYP           PIC S9(4) USAGE COMP.
           10 KL-AM-AGENT-NR       PIC S9(9) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
