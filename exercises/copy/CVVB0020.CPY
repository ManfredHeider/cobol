      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0020.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0020.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:15:20   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0020)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0020))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0020 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      BES_DAT_VON                    TIMESTAMP NOT NULL,
CCC004*      KTY_BES_GRND                   SMALLINT NOT NULL,
CCC004*      KNR_BES_GRND                   SMALLINT NOT NULL,
CCC004*      BES_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      BES_INFORMATION                CHAR(55) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0020                           *
      ******************************************************************
       01  VIVB0020-GESAMT.
        05  NI-VIVB0020-TAB.
           10  NI-VIVB0020-ELE     PIC S9(04)  COMP   OCCURS  6  TIMES.
        05  NI-VIVB0020            REDEFINES   NI-VIVB0020-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-BES-DAT-VON       PIC S9(04)  COMP.
           10 NI-KTY-BES-GRND      PIC S9(04)  COMP.
           10 NI-KNR-BES-GRND      PIC S9(04)  COMP.
           10 NI-BES-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-BES-INFORMATION   PIC S9(04)  COMP.
        05  DCL-VIVB0020.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 BES-DAT-VON          PIC X(26).
           10 KTY-BES-GRND         PIC S9(4) USAGE COMP.
           10 KNR-BES-GRND         PIC S9(4) USAGE COMP.
           10 BES-LAST-UPDATE      PIC X(26).
           10 BES-INFORMATION      PIC X(55).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
