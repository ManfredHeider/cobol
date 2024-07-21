      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA072.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVBA072.CPv  $
      *
      *   Rev 2.0   Apr 26 1999 13:23:18   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.1   26 Nov 1998 10:04:08   T13AE35
      *ARW00016 - Neue View mit zusaetzlichen Attributen auf ZPKT
      *
      *   Rev 1.0   25 Nov 1998 09:10:36   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVVBA072                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON VIVBA072 (ZPKT)                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J. Hering                                        *
      *    DATUM:    25.11.1998                                       *
      *****************************************************************
      *    VERSION   1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ARW00016!25.11.98! 01 ! NEU                       !Hering    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(VIVBA072)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA072 TABLE
CCC004*    ( KNR_STEUER_INFO    SMALLINT       NOT NULL,
CCC004*      ZPKT_ZEITPUNKT     TIMESTAMP      NOT NULL,
CCC004*      KTY_STEUER_INFO    SMALLINT       NOT NULL,
CCC004*      ZPKT_VERARB_KZ     CHAR(1)        NOT NULL,
CCC004*      ZPKT_LAST_UPDATE   TIMESTAMP      NOT NULL,
CCC004*      ZPKT_KENNUNG       CHAR(8)        NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR VIEW  VIVBA072                           *
      ******************************************************************
       01  VIVBA072-GESAMT.
        05  NI-VIVBA072-TAB.
           10 NI-VIVBA072-ELE      PIC S9(04) COMP  OCCURS 6 TIMES.
        05  NI-VIVBA072            REDEFINES  NI-VIVBA072-TAB.
           10 NI-KNR-STEUER-INFO   PIC S9(4)    USAGE COMP.
           10 NI-ZPKT-ZEITPUNKT    PIC S9(4)    USAGE COMP.
           10 NI-KTY-STEUER-INFO   PIC S9(4)    USAGE COMP.
           10 NI-ZPKT-VERARB-KZ    PIC S9(4)    USAGE COMP.
           10 NI-ZPKT-LAST-UPDATE  PIC S9(4)    USAGE COMP.
           10 NI-ZPKT-KENNUNG      PIC S9(4)    USAGE COMP.
        05  DCL-VIVBA072.
           10 KNR-STEUER-INFO      PIC S9(4)    USAGE COMP.
           10 ZPKT-ZEITPUNKT       PIC X(26).
           10 KTY-STEUER-INFO      PIC S9(4)    USAGE COMP.
           10 ZPKT-VERARB-KZ       PIC X(01).
           10 ZPKT-LAST-UPDATE     PIC X(26).
           10 ZPKT-KENNUNG         PIC X(08).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
