      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0030.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0030.CPv  $
      *
      *   Rev 2.1   Apr 22 2009 10:56:32   t13ex13
      *ASL00328: Unterprogramme
      *
      *   Rev 2.0   Feb 27 1999 15:56:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:17:28   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0030)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0030))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0030 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      ZV_NR                          CHAR(1) NOT NULL,
CCC004*      ZVZ_DAT_VON                    DATE NOT NULL,
CCC004*      ZVZ_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      ZVZ_DAT_BIS                    DATE NOT NULL,
CCC004*      ZVZ_VBG_AB                     DATE,
CCC004*      ZVZ_VBG_VNR                    CHAR(15),
CCC004*      ZVZ_ABSICHERG                  DECIMAL(11, 2),
CCC004*      ZVZ_DVAG_SUMME                 DECIMAL(11, 2),
CCC004*      ZVZ_VBG_BEITRAG                DECIMAL(11, 2),
CCC004*      ZVZ_CEN_AB                     DATE,
CCC004*      ZVZ_CEN_VNR                    CHAR(15),
CCC004*      ZVZ_CEN_BETR                   DECIMAL(11, 2),
CCC004*      ZVZ_CEN_BEITRAG                DECIMAL(11, 2)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0030                           *
      ******************************************************************
       01  VIVB0030-GESAMT.
        05  NI-VIVB0030-TAB.
           10  NI-VIVB0030-ELE     PIC S9(04)  COMP   OCCURS  14  TIMES.
        05  NI-VIVB0030            REDEFINES   NI-VIVB0030-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-ZV-NR             PIC S9(04)  COMP.
           10 NI-ZVZ-DAT-VON       PIC S9(04)  COMP.
           10 NI-ZVZ-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-ZVZ-DAT-BIS       PIC S9(04)  COMP.
           10 NI-ZVZ-VBG-AB        PIC S9(04)  COMP.
           10 NI-ZVZ-VBG-VNR       PIC S9(04)  COMP.
           10 NI-ZVZ-ABSICHERG     PIC S9(04)  COMP.
           10 NI-ZVZ-DVAG-SUMME    PIC S9(04)  COMP.
           10 NI-ZVZ-VBG-BEITRAG   PIC S9(04)  COMP.
           10 NI-ZVZ-CEN-AB        PIC S9(04)  COMP.
           10 NI-ZVZ-CEN-VNR       PIC S9(04)  COMP.
           10 NI-ZVZ-CEN-BETR      PIC S9(04)  COMP.
           10 NI-ZVZ-CEN-BEITRAG   PIC S9(04)  COMP.
        05  DCL-VIVB0030.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 ZV-NR                PIC X(1).
           10 ZVZ-DAT-VON          PIC X(10).
           10 ZVZ-LAST-UPDATE      PIC X(26).
           10 ZVZ-DAT-BIS          PIC X(10).
           10 ZVZ-VBG-AB           PIC X(10).
           10 ZVZ-VBG-VNR          PIC X(15).
           10 ZVZ-ABSICHERG        PIC S9(9)V99 USAGE COMP-3.
           10 ZVZ-DVAG-SUMME       PIC S9(9)V99 USAGE COMP-3.
           10 ZVZ-VBG-BEITRAG      PIC S9(9)V99 USAGE COMP-3.
           10 ZVZ-CEN-AB           PIC X(10).
           10 ZVZ-CEN-VNR          PIC X(15).
           10 ZVZ-CEN-BETR         PIC S9(9)V99 USAGE COMP-3.
           10 ZVZ-CEN-BEITRAG      PIC S9(9)V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
