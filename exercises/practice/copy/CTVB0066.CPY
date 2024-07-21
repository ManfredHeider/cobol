      * T13EX16   Stand: 20/05/96    08:40:15
      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0066.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0066.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Jun 25 1996 15:40:18   T13EX16
      *AFVBP226: SUSIS-Tabellen erweitert
      *
      *   Rev 1.1   30 Jan 1996 11:48:44   P13SP99
      *PVCS-Keywords angepaßt
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CTVB0066                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON TBVB0066 (SPFS)                                 * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: J. SCHINDLER                                     * 00060000
      *    DATUM:    09.11.1995                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    02.10.95 15:15:43
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0226!09.11.95! 01 ! NEU                       !SCHINDLER *  00140000
      * AFVB0226!20.05.96! 02 ! UM STATUS DER PFÄNDUNG    !AUERBACH  *  00143100
      *         !        !    ! ERWEITERT                 !          *  00144000
      *         !        !    !                           !          *  00145000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(TBVB0066)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0066 TABLE
CCC004*    ( ST_NR              INTEGER        NOT NULL,
CCC004*      SPFS_PFND_NR       SMALLINT       NOT NULL,
CCC004*      KNR_WAEHRUNG       SMALLINT       NOT NULL,
CCC004*      SPFS_LAST_UPDATE   TIMESTAMP      NOT NULL,
CCC004*      KNR_PFND_ART       SMALLINT       NOT NULL,
CCC004*      KTY_WAEHRUNG       SMALLINT       NOT NULL,
CCC004*      KTY_PFND_ART       SMALLINT       NOT NULL,
CCC004*      SPFS_PFND_RANG     SMALLINT       NOT NULL,
CCC004*      KTY_PFAND_STATUS   SMALLINT       NOT NULL,
CCC004*      KNR_PFAND_STATUS   SMALLINT       NOT NULL,
CCC004*      SPFS_FORD_GS       DECIMAL(11, 2) NOT NULL,
CCC004*      SPFS_RATE_UNTERH   DECIMAL(11, 2) NOT NULL,
CCC004*      SPFS_RATE_PFND_V   DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0066                           *
      ******************************************************************
       01  TBVB0066-GESAMT.
        05  NI-TBVB0066-TAB.
200596     10 NI-TBVB0066-ELE      PIC S9(04) COMP  OCCURS 13 TIMES.
        05  NI-TBVB0066            REDEFINES  NI-TBVB0066-TAB.
           10 NI-ST-NR             PIC S9(4)    USAGE COMP.
           10 NI-SPFS-PFND-NR      PIC S9(4)    USAGE COMP.
           10 NI-KNR-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 NI-SPFS-LAST-UPDATE  PIC S9(4)    USAGE COMP.
           10 NI-KNR-PFND-ART      PIC S9(4)    USAGE COMP.
           10 NI-KTY-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 NI-KTY-PFND-ART      PIC S9(4)    USAGE COMP.
           10 NI-SPFS-PFND-RANG    PIC S9(4)    USAGE COMP.
200596     10 NI-KTY-PFAND-STATUS  PIC S9(4)    USAGE COMP.
200596     10 NI-KNR-PFAND-STATUS  PIC S9(4)    USAGE COMP.
           10 NI-SPFS-FORD-GS      PIC S9(4)    USAGE COMP.
           10 NI-SPFS-RATE-UNTERH  PIC S9(4)    USAGE COMP.
           10 NI-SPFS-RATE-PFND-V  PIC S9(4)    USAGE COMP.
        05  DCL-TBVB0066.
           10 ST-NR                PIC S9(9)    USAGE COMP.
           10 SPFS-PFND-NR         PIC S9(4)    USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 SPFS-LAST-UPDATE     PIC X(26).
           10 KNR-PFND-ART         PIC S9(4)    USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 KTY-PFND-ART         PIC S9(4)    USAGE COMP.
           10 SPFS-PFND-RANG       PIC S9(4)    USAGE COMP.
200596     10 KTY-PFAND-STATUS     PIC S9(4)    USAGE COMP.
200596     10 KNR-PFAND-STATUS     PIC S9(4)    USAGE COMP.
           10 SPFS-FORD-GS         PIC S9(9)V99 USAGE COMP-3.
           10 SPFS-RATE-UNTERH     PIC S9(9)V99 USAGE COMP-3.
           10 SPFS-RATE-PFND-V     PIC S9(9)V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
