      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0072.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0072.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Apr 30 1996 17:08:00   T13EX16
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTVB0072                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBVB0072 (ZPKT)                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: R. AUERBACH                                      *
      *    DATUM:    12.03.1996                                       *
      *****************************************************************
      *    VERSION   1    02.10.95 15:15:43
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0266!12.03.96! 01 ! NEU                       !AUERBACH  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(TBVB0072)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0072 TABLE
CCC004*    ( KNR_STEUER_INFO    SMALLINT       NOT NULL,
CCC004*      ZPKT_ZEITPUNKT     TIMESTAMP      NOT NULL
CCC004*      ZPKT_LAST_UPDATE   TIMESTAMP      NOT NULL,
CCC004*      KTY_STEUER_INFO    SMALLINT       NOT NULL,
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0072                           *
      ******************************************************************
       01  TBVB0072-GESAMT.
        05  NI-TBVB0072-TAB.
           10 NI-TBVB0072-ELE      PIC S9(04) COMP  OCCURS 4 TIMES.
        05  NI-TBVB0072            REDEFINES  NI-TBVB0072-TAB.
           10 NI-KNR-STEUER-INFO   PIC S9(4)    USAGE COMP.
           10 NI-ZPKT-ZEITPUNKT    PIC S9(4)    USAGE COMP.
           10 NI-ZPKT-LAST-UPDATE  PIC S9(4)    USAGE COMP.
           10 NI-KTY-STEUER-INFO   PIC S9(4)    USAGE COMP.
        05  DCL-TBVB0072.
           10 KNR-STEUER-INFO      PIC S9(4)    USAGE COMP.
           10 ZPKT-ZEITPUNKT       PIC X(26).
           10 ZPKT-LAST-UPDATE     PIC X(26).
           10 KTY-STEUER-INFO      PIC S9(4)    USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
