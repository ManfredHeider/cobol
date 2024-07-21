      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS VM
      *            Datum      :  11.03.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVTP0003.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/CVTP0003.CPv  $
      *
      *   Rev 2.1   May 15 2003 14:03:00   t13ae13
      *AVB00658: Dimension 7ukunft - Neues Aufstiegssystem 
      *
      *   Rev 2.0   Mar 11 2003 15:31:06   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVTP0003                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBTP0003 (BEN)                                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    02.04.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00658!02.04.03!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *---------------------------------------------------------------*
      * DCLGEN TABLE(TBTP0003)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVTP0003))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBTP0003 TABLE
CCC004*    (  MA_ST_NR           INTEGER              NOT NULL,
CCC004*       BEN_MONAT           DATE                NOT NULL,
CCC004*       BEN_NEU_STF         SMALLINT            NOT NULL,
CCC004*       BEN_ZEITRAUM        SMALLINT            NOT NULL,
CCC004*       BEN_EH_GS_NT_GR     DECIMAL ( 10, 1 )   NOT NULL,
CCC004*       BEN_EH_GS_NT_GES    DECIMAL ( 10, 1 )   NOT NULL,
CCC004*       BEN_ANZ_HPTB_ME     SMALLINT            NOT NULL,
CCC004*       BEN_ANZ_AB_VBA      SMALLINT            NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBTP0003                           *
      ******************************************************************
       01  TBTP0003-GESAMT.
        05  NI-TBTP0003-TAB.
           10  NI-TBTP0003-ELE     PIC S9(04)  COMP   OCCURS  8  TIMES.
        05  NI-TBTP0003            REDEFINES   NI-TBTP0003-TAB.
           10 NI-MA-ST-NR          PIC S9(4) USAGE COMP.
           10 NI-BEN-MONAT         PIC S9(4) USAGE COMP.
           10 NI-BEN-NEU-STF       PIC S9(4) USAGE COMP.
           10 NI-BEN-ZEITRAUM      PIC S9(4) USAGE COMP.
           10 NI-BEN-EH-GS-NT-GR   PIC S9(4) USAGE COMP.
           10 NI-BEN-EH-GS-NT-GES  PIC S9(4) USAGE COMP.
           10 NI-BEN-ANZ-HPTB-ME   PIC S9(4) USAGE COMP.
           10 NI-BEN-ANZ-AB-VBA    PIC S9(4) USAGE COMP.
        05  DCL-TBTP0003.
           10 MA-ST-NR             PIC  S9(09)    COMP.
           10 BEN-MONAT            PIC   X(10).
           10 BEN-NEU-STF          PIC  S9(04)    COMP.
           10 BEN-ZEITRAUM         PIC  S9(04)    COMP.
           10 BEN-EH-GS-NT-GR      PIC  S9(09)V9  COMP-3.
           10 BEN-EH-GS-NT-GES     PIC  S9(09)V9  COMP-3.
           10 BEN-ANZ-HPTB-ME      PIC  S9(04)    COMP.
           10 BEN-ANZ-AB-VBA       PIC  S9(04)    COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
