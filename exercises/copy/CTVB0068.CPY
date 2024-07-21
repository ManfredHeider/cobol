      *---------------------------------------------------------------*
      *    STANDARD F‹R ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0068.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0068.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   May 17 1996 13:31:04   T13EX16
      *AFVB0226: Copy war fehlerhaft
      *
      *   Rev 1.1   30 Jan 1996 11:48:06   P13SP99
      *PVCS-Keywords angepaﬂt
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CTVB0068                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON TBVB0068 (SPFB)                                 * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: J. SCHINDLER                                     * 00060000
      *    DATUM:    10.11.1995                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    02.10.95 15:15:43
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0226!10.11.95! 01 ! NEU                       !SCHINDLER *  00140000
      * AFVB0226!17.05.96! 02 ! COPY WAR FEHLERHAFT       !AUERBACH  *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(TBVB0068)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0068 TABLE
CCC004*    ( ST_NR              INTEGER        NOT NULL,
CCC004*      SPFB_MONAT         DATE           NOT NULL,
CCC004*      SPFB_PFND_NR       SMALLINT       NOT NULL,
CCC004*      KNR_WAEHRUNG       SMALLINT       NOT NULL,
CCC004*      SPFB_LAST_UPDATE   TIMESTAMP      NOT NULL,
CCC004*      KTY_WAEHRUNG       SMALLINT       NOT NULL,
CCC004*      SPFB_RATE_GLAEUB   DECIMAL(11, 2) NOT NULL,
CCC004*      SPFB_ZAHLUNG_MA    DECIMAL(11, 2) NOT NULL,
CCC004*      SPFB_ZINSEN        DECIMAL(11, 2) NOT NULL,
CCC004*      SPFB_PFND_REST     DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0068                           *
      ******************************************************************
       01  TBVB0068-GESAMT.
        05  NI-TBVB0068-TAB.
           10 NI-TBVB0068-ELE      PIC S9(04) COMP  OCCURS 10 TIMES.
        05  NI-TBVB0068            REDEFINES  NI-TBVB0068-TAB.
           10 NI-ST-NR             PIC S9(4)    USAGE COMP.
           10 NI-SPFB-MONAT        PIC S9(4)    USAGE COMP.
           10 NI-SPFB-PFND-NR      PIC S9(4)    USAGE COMP.
           10 NI-KNR-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 NI-SPFB-LAST-UPDATE  PIC S9(4)    USAGE COMP.
           10 NI-KTY-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 NI-SPFB-RATE-GLAEUB  PIC S9(4)    USAGE COMP.
           10 NI-SPFB-ZAHLUNG-MA   PIC S9(4)    USAGE COMP.
           10 NI-SPFB-ZINSEN       PIC S9(4)    USAGE COMP.
           10 NI-SPFB-PFND-REST    PIC S9(4)    USAGE COMP.
        05  DCL-TBVB0068.
           10 ST-NR                PIC S9(9)    USAGE COMP.
           10 SPFB-MONAT           PIC X(10).
           10 SPFB-PFND-NR         PIC S9(4)    USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 SPFB-LAST-UPDATE     PIC X(26).
           10 KTY-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 SPFB-RATE-GLAEUB     PIC S9(9)V99 USAGE COMP-3.
           10 SPFB-ZAHLUNG-MA      PIC S9(9)V99 USAGE COMP-3.
           10 SPFB-ZINSEN          PIC S9(9)V99 USAGE COMP-3.
           10 SPFB-PFND-REST       PIC S9(9)V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
