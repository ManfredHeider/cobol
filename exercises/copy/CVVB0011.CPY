      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0011.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0011.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:13:30   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0011)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0011))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0011 TABLE
CCC004*    ( BVH_BETR_NR                    INTEGER NOT NULL,
CCC004*      BVH_UNTST_NR                   INTEGER NOT NULL,
CCC004*      BVH_DAT_VON                    TIMESTAMP NOT NULL,
CCC004*      BVH_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      BVH_DAT_BIS                    DATE NOT NULL,
CCC004*      KTY_BETR_TYP                   SMALLINT NOT NULL,
CCC004*      KNR_BETR_TYP                   SMALLINT NOT NULL,
CCC004*      KTY_UNTST_TYP                  SMALLINT NOT NULL,
CCC004*      KNR_UNTST_TYP                  SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0011                           *
      ******************************************************************
       01  VIVB0011-GESAMT.
        05  NI-VIVB0011-TAB.
           10  NI-VIVB0011-ELE     PIC S9(04)  COMP   OCCURS  9  TIMES.
        05  NI-VIVB0011            REDEFINES   NI-VIVB0011-TAB.
           10 NI-BVH-BETR-NR       PIC S9(04)  COMP.
           10 NI-BVH-UNTST-NR      PIC S9(04)  COMP.
           10 NI-BVH-DAT-VON       PIC S9(04)  COMP.
           10 NI-BVH-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-BVH-DAT-BIS       PIC S9(04)  COMP.
           10 NI-KTY-BETR-TYP      PIC S9(04)  COMP.
           10 NI-KNR-BETR-TYP      PIC S9(04)  COMP.
           10 NI-KTY-UNTST-TYP     PIC S9(04)  COMP.
           10 NI-KNR-UNTST-TYP     PIC S9(04)  COMP.
        05  DCL-VIVB0011.
           10 BVH-BETR-NR          PIC S9(9) USAGE COMP.
           10 BVH-UNTST-NR         PIC S9(9) USAGE COMP.
           10 BVH-DAT-VON          PIC X(26).
           10 BVH-LAST-UPDATE      PIC X(26).
           10 BVH-DAT-BIS          PIC X(10).
           10 KTY-BETR-TYP         PIC S9(4) USAGE COMP.
           10 KNR-BETR-TYP         PIC S9(4) USAGE COMP.
           10 KTY-UNTST-TYP        PIC S9(4) USAGE COMP.
           10 KNR-UNTST-TYP        PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
