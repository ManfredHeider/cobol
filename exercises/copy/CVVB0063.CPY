      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0063.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0063.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:23:12   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CVVB0063                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON VIVB0063 (BER)                                  * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: SCHMIDT                                          * 00060000
      *    DATUM:    16.04.1995                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   2    25.08.95 13:13:03
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0180!16.04.95! 01 ! NEU                       ! SCHMIDT  *  00140000
      * AFVB0180!25.08.95! 02 ! SYNTAX-FEHLER BEHOBEN     ! GOECKE   *  00140000
      *         !        !    ! UND NEU IN PRODUKTION     !          *  00143000
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(VIVB0063)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0063 TABLE
CCC004*    ( ST_NR                          INTEGER        NOT NULL,
CCC004*      BER_HAFTUNG                    CHAR(1)        NOT NULL,
CCC004*      BER_L_PROV_BEW                 CHAR(1)        NOT NULL,
CCC004*      BER_STAND_DISK                 DECIMAL(11, 2) NOT NULL,
CCC004*      BER_STAND_RUECK                DECIMAL(11, 2) NOT NULL,
CCC004*      BER_STAND_VORSCH               DECIMAL(11, 2) NOT NULL,
CCC004*      BER_STAND_ABSCHR               DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0063                           *
      ******************************************************************
       01  VIVB0063-GESAMT.
        05  NI-VIVB0063-TAB.
           10 NI-VIVB0063-ELE      PIC S9(04) COMP  OCCURS 7  TIMES.
        05  NI-VIVB0063            REDEFINES  NI-VIVB0063-TAB.
           10 NI-ST-NR             PIC S9(4) USAGE COMP.
           10 NI-BER-HAFTUNG       PIC S9(4) USAGE COMP.
           10 NI-BER-L-PROV-BEW    PIC S9(4) USAGE COMP.
           10 NI-BER-STAND-DISK    PIC S9(4) USAGE COMP.
           10 NI-BER-STAND-RUECK   PIC S9(4) USAGE COMP.
           10 NI-BER-STAND-VORSCH  PIC S9(4) USAGE COMP.
           10 NI-BER-STAND-ABSCHR  PIC S9(4) USAGE COMP.
        05  DCL-VIVB0063.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 BER-HAFTUNG          PIC X(1).
           10 BER-L-PROV-BEW       PIC X(1).
           10 BER-STAND-DISK       PIC S999999999V99 USAGE COMP-3.
           10 BER-STAND-RUECK      PIC S999999999V99 USAGE COMP-3.
           10 BER-STAND-VORSCH     PIC S999999999V99 USAGE COMP-3.
           10 BER-STAND-ABSCHR     PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
