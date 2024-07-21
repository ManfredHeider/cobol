      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA003.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA003.CPv  $
      *
      *   Rev 2.1   Jun 23 2009 10:25:06   t13ae13
      *AVB01029: Wahlmöglichkeit bei ED-Regelung - Tabelle
      *
      *   Rev 2.0   Mar 17 2009 10:20:52   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01029!17.03.09!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(VIVBA003)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA003))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA003 TABLE
CCC004*    ( ED_ST_NR                       INTEGER NOT NULL,
CCC004*      ED_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      MA_ST_NR                       INTEGER NOT NULL,
CCC004*      ED_RCK_STZ                     DECIMAL(5, 2) NOT NULL,
CCC004*      ED_RCK_STZ_DAT                 DATE NOT NULL,
CCC004*      ED_BEZ                         CHAR(40) NOT NULL,
CCC004*      ED_GENERATION                  SMALLINT,
CCC004*      ED_ZWEIG                       INTEGER,
CCC004*      ED_ZWEIG_VOM                   DATE,
CCC004*      ED_PROV1                       DECIMAL(5, 2),
CCC004*      ED_PROV_DAT1                   DATE,
CCC004*      ED_PROV2                       DECIMAL(5, 2),
CCC004*      ED_PROV_DAT2                   DATE

CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA003                           *
      ******************************************************************
       01  VIVBA003-GESAMT.
        05  NI-VIVBA003-TAB.
           10  NI-VIVBA003-ELE     PIC S9(04) COMP   OCCURS 13  TIMES.
        05  NI-VIVBA003            REDEFINES  NI-VIVBA003-TAB.
           10 NI-ED-ST-NR          PIC S9(04) COMP.
           10 NI-ED-LAST-UPDATE    PIC S9(04) COMP.
           10 NI-MA-ST-NR          PIC S9(04) COMP.
           10 NI-ED-RCK-STZ        PIC S9(04) COMP.
           10 NI-ED-RCK-STZ-DAT    PIC S9(04) COMP.
           10 NI-ED-BEZ            PIC S9(04) COMP.
           10 NI-ED-GENERATION     PIC S9(04) COMP.
           10 NI-ED-ZWEIG          PIC S9(04) COMP.
           10 NI-ED-ZWEIG-VOM      PIC S9(04) COMP.
           10 NI-ED-PROV1          PIC S9(04) COMP.
           10 NI-ED-PROV-DAT1      PIC S9(04) COMP.
           10 NI-ED-PROV2          PIC S9(04) COMP.
           10 NI-ED-PROV-DAT2      PIC S9(04) COMP.

        05  DCL-VIVBA003.
           10 ED-ST-NR             PIC S9(9)   USAGE COMP.
           10 ED-LAST-UPDATE       PIC X(26).
           10 MA-ST-NR             PIC S9(9)   USAGE COMP.
           10 ED-RCK-STZ           PIC S999V99 USAGE COMP-3.
           10 ED-RCK-STZ-DAT       PIC X(10).
           10 ED-BEZ               PIC X(40).
           10 ED-GENERATION        PIC S9(04)  USAGE COMP.
           10 ED-ZWEIG             PIC S9(09)  USAGE COMP.
           10 ED-ZWEIG-VOM         PIC X(10).
           10 ED-PROV1             PIC S999V99 USAGE COMP-3.
           10 ED-PROV-DAT1         PIC X(10).
           10 ED-PROV2             PIC S999V99 USAGE COMP-3.
           10 ED-PROV-DAT2         PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
