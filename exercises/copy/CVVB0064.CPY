      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0064.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0064.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:23:24   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CVVB0064                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON VIVB0064 (SLM)                                  * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: SCHMIDT                                          * 00060000
      *    DATUM:    12.09.1995                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    02.10.95 15:15:43
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0226!12.09.95! 01 ! NEU                       ! SCHMIDT  *  00140000
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(VIVB0064)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0064 TABLE
CCC004*    ( ST_NR                          INTEGER        NOT NULL,
CCC004*      SLM_DAT_VON                    DATE           NOT NULL,
CCC004*      KNR_SLM_TYP                    SMALLINT       NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT       NOT NULL,
CCC004*      SLM_LAST_UPDATE                TIMESTAMP      NOT NULL,
CCC004*      KTY_SLM_TYP                    SMALLINT       NOT NULL,
CCC004*      KTY_WAEHRUNG                   SMALLINT       NOT NULL,
CCC004*      SLM_AUSZAHLUNG                 DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0064                           *
      ******************************************************************
       01  VIVB0064-GESAMT.
        05  NI-VIVB0064-TAB.
           10 NI-VIVB0064-ELE      PIC S9(04) COMP  OCCURS 8  TIMES.
        05  NI-VIVB0064            REDEFINES  NI-VIVB0064-TAB.
           10 NI-ST-NR             PIC S9(4)    USAGE COMP.
           10 NI-SLM-DAT-VON       PIC S9(4)    USAGE COMP.
           10 NI-KNR-SLM-TYP       PIC S9(4)    USAGE COMP.
           10 NI-KNR-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 NI-SLM-LAST-UPDATE   PIC S9(4)    USAGE COMP.
           10 NI-KTY-SLM-TYP       PIC S9(4)    USAGE COMP.
           10 NI-KTY-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 NI-SLM-AUSZAHLUNG    PIC S9(4)    USAGE COMP.
        05  DCL-VIVB0064.
           10 ST-NR                PIC S9(9)    USAGE COMP.
           10 SLM-DAT-VON          PIC X(10).
           10 KNR-SLM-TYP          PIC S9(4)    USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 SLM-LAST-UPDATE      PIC X(26).
           10 KTY-SLM-TYP          PIC S9(4)    USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 SLM-AUSZAHLUNG       PIC S9(9)V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
