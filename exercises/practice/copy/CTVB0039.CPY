      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0039.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0039.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:00:00   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CTVB0039                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON TBVB0039 (LF)                                   * 00040001
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: KUHN                                             * 00060000
      *    DATUM:    15.09.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    29.10.94 10:16:41
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0142!15.09.94! 01 ! NEU                       ! KUHN     *  00140000
      *         !        !    !                           !          *  00143000
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(TBVB0039)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0039))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0039 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      LF_MONAT                       DATE NOT NULL,
CCC004*      KNR_PROD_GEB_GES               SMALLINT NOT NULL,
CCC004*      KTY_PROD_GEB_GES               SMALLINT NOT NULL,
CCC004*      LF_BQF_EG                      DECIMAL(5, 2) NOT NULL,
CCC004*      LF_BQF_GS                      DECIMAL(5, 2) NOT NULL,
CCC004*      LF_BQF_GR                      DECIMAL(5, 2) NOT NULL,
CCC004*      LF_DYNQ_EG                     DECIMAL(5, 2) NOT NULL,
CCC004*      LF_DYNQ_GS                     DECIMAL(5, 2) NOT NULL,
CCC004*      LF_DYNQ_GR                     DECIMAL(5, 2) NOT NULL,
CCC004*      LF_NEQF_EG                     DECIMAL(5, 2) NOT NULL,
CCC004*      LF_NEQF_GS                     DECIMAL(5, 2) NOT NULL,
CCC004*      LF_NEQF_GR                     DECIMAL(5, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0039                           *
      ******************************************************************
       01  TBVB0039-GESAMT.
        05  NI-TBVB0039-TAB.
           10 NI-TBVB0039-ELE      PIC S9(04) COMP  OCCURS 13 TIMES.
        05  NI-TBVB0039            REDEFINES  NI-TBVB0039-TAB.
           10 NI-ST-NR             PIC S9(4) USAGE COMP.
           10 NI-LF-MONAT          PIC S9(4) USAGE COMP.
           10 NI-KNR-PROD-GEB-GES  PIC S9(4) USAGE COMP.
           10 NI-KTY-PROD-GEB-GES  PIC S9(4) USAGE COMP.
           10 NI-LF-BQF-EG         PIC S9(4) USAGE COMP.
           10 NI-LF-BQF-GS         PIC S9(4) USAGE COMP.
           10 NI-LF-BQF-GR         PIC S9(4) USAGE COMP.
           10 NI-LF-DYNQ-EG        PIC S9(4) USAGE COMP.
           10 NI-LF-DYNQ-GS        PIC S9(4) USAGE COMP.
           10 NI-LF-DYNQ-GR        PIC S9(4) USAGE COMP.
           10 NI-LF-NEQF-EG        PIC S9(4) USAGE COMP.
           10 NI-LF-NEQF-GS        PIC S9(4) USAGE COMP.
           10 NI-LF-NEQF-GR        PIC S9(4) USAGE COMP.
        05  DCL-TBVB0039.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 LF-MONAT             PIC X(10).
           10 KNR-PROD-GEB-GES     PIC S9(4) USAGE COMP.
           10 KTY-PROD-GEB-GES     PIC S9(4) USAGE COMP.
           10 LF-BQF-EG            PIC S999V99 USAGE COMP-3.
           10 LF-BQF-GS            PIC S999V99 USAGE COMP-3.
           10 LF-BQF-GR            PIC S999V99 USAGE COMP-3.
           10 LF-DYNQ-EG           PIC S999V99 USAGE COMP-3.
           10 LF-DYNQ-GS           PIC S999V99 USAGE COMP-3.
           10 LF-DYNQ-GR           PIC S999V99 USAGE COMP-3.
           10 LF-NEQF-EG           PIC S999V99 USAGE COMP-3.
           10 LF-NEQF-GS           PIC S999V99 USAGE COMP-3.
           10 LF-NEQF-GR           PIC S999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
