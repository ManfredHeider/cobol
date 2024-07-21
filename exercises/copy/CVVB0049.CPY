      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0049.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0049.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:21:02   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0049)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0049))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0049 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      MAT_DAT_VON                    TIMESTAMP NOT NULL,
CCC004*      MAT_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      MAT_DAT_BIS                    DATE NOT NULL,
CCC004*      MAT_KUE_DAT_VOM                DATE,
CCC004*      MAT_EING_KUE_DAT               DATE,
CCC004*      MAT_VORZFREI_DAT               DATE,
CCC004*      KTY_MAT_AUS_GRND               SMALLINT NOT NULL,
CCC004*      KNR_MAT_AUS_GRND               SMALLINT NOT NULL,
CCC004*      MAT_L_RCK_STZ                  DECIMAL(5, 2) NOT NULL,
CCC004*      MAT_ED_PROV1                   DECIMAL(5, 2),
CCC004*      MAT_ED_PROV2                   DECIMAL(5, 2),
CCC004*      MAT_ED_PROV_DAT1               DATE,
CCC004*      MAT_ED_PROV_DAT2               DATE
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0049                           *
      ******************************************************************
       01  VIVB0049-GESAMT.
        05  NI-VIVB0049-TAB.
           10  NI-VIVB0049-ELE     PIC S9(04)  COMP   OCCURS  14 TIMES.
        05  NI-VIVB0049            REDEFINES   NI-VIVB0049-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-MAT-DAT-VON       PIC S9(04)  COMP.
           10 NI-MAT-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-MAT-DAT-BIS       PIC S9(04)  COMP.
           10 NI-MAT-KUE-DAT-VOM   PIC S9(04)  COMP.
           10 NI-MAT-EING-KUE-DAT  PIC S9(04)  COMP.
           10 NI-MAT-VORZFREI-DAT  PIC S9(04)  COMP.
           10 NI-KTY-MAT-AUS-GRND  PIC S9(04)  COMP.
           10 NI-KNR-MAT-AUS-GRND  PIC S9(04)  COMP.
           10 NI-MAT-L-RCK-STZ     PIC S9(04)  COMP.
           10 NI-MAT-ED-PROV1      PIC S9(04)  COMP.
           10 NI-MAT-ED-PROV2      PIC S9(04)  COMP.
           10 NI-MAT-ED-PROV-DAT1  PIC S9(04)  COMP.
           10 NI-MAT-ED-PROV-DAT2  PIC S9(04)  COMP.
        05  DCL-VIVB0049.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 MAT-DAT-VON          PIC X(26).
           10 MAT-LAST-UPDATE      PIC X(26).
           10 MAT-DAT-BIS          PIC X(10).
           10 MAT-KUE-DAT-VOM      PIC X(10).
           10 MAT-EING-KUE-DAT     PIC X(10).
           10 MAT-VORZFREI-DAT     PIC X(10).
           10 KTY-MAT-AUS-GRND     PIC S9(4) USAGE COMP.
           10 KNR-MAT-AUS-GRND     PIC S9(4) USAGE COMP.
           10 MAT-L-RCK-STZ        PIC S999V99 USAGE COMP-3.
           10 MAT-ED-PROV1         PIC S999V99 USAGE COMP-3.
           10 MAT-ED-PROV2         PIC S999V99 USAGE COMP-3.
           10 MAT-ED-PROV-DAT1     PIC X(10).
           10 MAT-ED-PROV-DAT2     PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
