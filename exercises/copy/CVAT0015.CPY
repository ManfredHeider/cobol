      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVAT0015.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVAT0015.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:08:02   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CVAT0015                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON VIAT0015 (PSGS)                                 * 00040001
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: KUHN                                             * 00060000
      *    DATUM:    18.11.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    10.01.95 16:36:12
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0142!18.11.94! 01 ! NEU                       ! KUHN     *  00140000
      *         !        !    !                           !          *  00143000
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(VIAT0015)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT0015 TABLE
CCC004*    ( KNR_GRP_SPARTE                 SMALLINT NOT NULL,
CCC004*      KNR_PROV_TYP                   SMALLINT NOT NULL,
CCC004*      KNR_PROD_SPARTE                SMALLINT NOT NULL,
CCC004*      PSGS_DAT_VON                   TIMESTAMP NOT NULL,
CCC004*      PSGS_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PSGS_DAT_BIS                   DATE NOT NULL,
CCC004*      KTY_GRP_SPARTE                 SMALLINT NOT NULL,
CCC004*      KTY_PROV_TYP                   SMALLINT NOT NULL,
CCC004*      KTY_PROD_SPARTE                SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0015                           *
      ******************************************************************
       01  VIAT0015-GESAMT.
        05  NI-VIAT0015-TAB.
           10 NI-VIAT0015-ELE      PIC S9(04) COMP  OCCURS 9 TIMES.
        05  NI-VIAT0015            REDEFINES  NI-VIAT0015-TAB.
           10 NI-KNR-GRP-SPARTE    PIC S9(4) USAGE COMP.
           10 NI-KNR-PROV-TYP      PIC S9(4) USAGE COMP.
           10 NI-KNR-PROD-SPARTE   PIC S9(4) USAGE COMP.
           10 NI-PSGS-DAT-VON      PIC S9(4) USAGE COMP.
           10 NI-PSGS-LAST-UPDATE  PIC S9(4) USAGE COMP.
           10 NI-PSGS-DAT-BIS      PIC S9(4) USAGE COMP.
           10 NI-KTY-GRP-SPARTE    PIC S9(4) USAGE COMP.
           10 NI-KTY-PROV-TYP      PIC S9(4) USAGE COMP.
           10 NI-KTY-PROD-SPARTE   PIC S9(4) USAGE COMP.
        05  DCL-VIAT0015.
           10 KNR-GRP-SPARTE       PIC S9(4) USAGE COMP.
           10 KNR-PROV-TYP         PIC S9(4) USAGE COMP.
           10 KNR-PROD-SPARTE      PIC S9(4) USAGE COMP.
           10 PSGS-DAT-VON         PIC  X(26).
           10 PSGS-LAST-UPDATE     PIC  X(26).
           10 PSGS-DAT-BIS         PIC  X(10).
           10 KTY-GRP-SPARTE       PIC S9(4) USAGE COMP.
           10 KTY-PROV-TYP         PIC S9(4) USAGE COMP.
           10 KTY-PROD-SPARTE      PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
