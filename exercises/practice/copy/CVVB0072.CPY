      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0072.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0072.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Apr 30 1996 17:09:30   T13EX16
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVVB0072                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON VIVB0072 (ZPKT)                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J. SCHINDLER                                     *
      *    DATUM:    12.12.1995                                       *
      *****************************************************************
      *    VERSION   1    02.10.95 15:15:43
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0266!12.12.95! 01 ! NEU                       !SCHINDLER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(VIVB0072)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0072 TABLE
CCC004*    ( KNR_STEUER_INFO    SMALLINT       NOT NULL,
CCC004*      ZPKT_ZEITPUNKT     TIMESTAMP      NOT NULL,
CCC004*      ZPKT_LAST_UPDATE   TIMESTAMP      NOT NULL,
CCC004*      KTY_STEUER_INFO    SMALLINT       NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0072                           *
      ******************************************************************
       01  VIVB0072-GESAMT.
        05  NI-VIVB0072-TAB.
           10 NI-VIVB0072-ELE      PIC S9(04) COMP  OCCURS 4 TIMES.
        05  NI-VIVB0072            REDEFINES  NI-VIVB0072-TAB.
           10 NI-KNR-STEUER-INFO   PIC S9(4)    USAGE COMP.
           10 NI-ZPKT-ZEITPUNKT    PIC S9(4)    USAGE COMP.
           10 NI-ZPKT-LAST-UPDATE  PIC S9(4)    USAGE COMP.
           10 NI-KTY-STEUER-INFO   PIC S9(4)    USAGE COMP.
        05  DCL-VIVB0072.
           10 KNR-STEUER-INFO      PIC S9(4)    USAGE COMP.
           10 ZPKT-ZEITPUNKT       PIC X(26).
           10 ZPKT-LAST-UPDATE     PIC X(26).
           10 KTY-STEUER-INFO      PIC S9(4)    USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
