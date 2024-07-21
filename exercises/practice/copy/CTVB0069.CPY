      * T13EX16   Stand: 20/05/96    08:40:31
      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0069.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0069.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Jun 25 1996 15:42:32   T13EX16
      *AFVBP226: SUSIS-Tabellen erweitert
      *
      *   Rev 1.1   30 Jan 1996 11:47:46   P13SP99
      *PVCS-Keywords angepaßt
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CTVB0069                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON TBVB0069 (SDLB)                                 * 00040000
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
      * AFVB0226!20.05.96! 02 ! DIE SPALTE SDLB_ZAHLUNG_MA!AUERBACH  *  00145000
      *         !        !    ! ENTFÄLLT                  !          *  00145000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(TBVB0069)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0069 TABLE
CCC004*    ( ST_NR              INTEGER        NOT NULL,
CCC004*      SDLB_MONAT         DATE           NOT NULL,
CCC004*      SDLB_DARL_NR       SMALLINT       NOT NULL,
CCC004*      KNR_WAEHRUNG       SMALLINT       NOT NULL,
CCC004*      SDLB_LAST_UPDATE   TIMESTAMP      NOT NULL,
CCC004*      KTY_WAEHRUNG       SMALLINT       NOT NULL,
CCC004*      SDLB_RATE_EINBH    DECIMAL(11, 2) NOT NULL,
200596*      SDLB_ZAHLUNG_MA    DECIMAL(11, 2) NOT NULL,
CCC004*      SDLB_ZINSEN        DECIMAL(11, 2) NOT NULL,
CCC004*      SDLB_DARL_REST     DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0069                           *
      ******************************************************************
       01  TBVB0069-GESAMT.
        05  NI-TBVB0069-TAB.
200596     10 NI-TBVB0069-ELE      PIC S9(04) COMP  OCCURS 09 TIMES.
        05  NI-TBVB0069            REDEFINES  NI-TBVB0069-TAB.
           10 NI-ST-NR             PIC S9(4)    USAGE COMP.
           10 NI-SDLB-MONAT        PIC S9(4)    USAGE COMP.
           10 NI-SDLB-DARL-NR      PIC S9(4)    USAGE COMP.
           10 NI-KNR-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 NI-SDLB-LAST-UPDATE  PIC S9(4)    USAGE COMP.
           10 NI-KTY-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 NI-SDLB-RATE-EINBH   PIC S9(4)    USAGE COMP.
200596*    10 NI-SDLB-ZAHLUNG-MA   PIC S9(4)    USAGE COMP.
           10 NI-SDLB-ZINSEN       PIC S9(4)    USAGE COMP.
           10 NI-SDLB-DARL-BETRAG  PIC S9(4)    USAGE COMP.
        05  DCL-TBVB0069.
           10 ST-NR                PIC S9(9)    USAGE COMP.
           10 SDLB-MONAT           PIC X(10).
           10 SDLB-DARL-NR         PIC S9(4)    USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 SDLB-LAST-UPDATE     PIC X(26).
           10 KTY-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 SDLB-RATE-EINBH      PIC S9(9)V99 USAGE COMP-3.
200596*    10 SDLB-ZAHLUNG-MA      PIC S9(9)V99 USAGE COMP-3.
           10 SDLB-ZINSEN          PIC S9(9)V99 USAGE COMP-3.
           10 SDLB-DARL-BETRAG     PIC S9(9)V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
