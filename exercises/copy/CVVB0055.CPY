      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0055.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0055.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:22:22   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0055)                                         *
      *        LIBRARY(T130003.CE010000.SRCDCL(CVVB0055))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0055 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      MAST_DAT_VON                   TIMESTAMP NOT NULL,
CCC004*      KTY_MAST_TYP                   SMALLINT NOT NULL,
CCC004*      KNR_MAST_TYP                   SMALLINT NOT NULL,
CCC004*      MAST_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      MAST_DAT_BIS                   DATE NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0055                           *
      ******************************************************************
       01  VIVB0055-GESAMT.
        05  NI-VIVB0055-TAB.
           10  NI-VIVB0055-ELE     PIC S9(04)  COMP   OCCURS   6 TIMES.
        05  NI-VIVB0055            REDEFINES   NI-VIVB0055-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-MAST-DAT-VON      PIC S9(04)  COMP.
           10 NI-KTY-MAST-TYP      PIC S9(04)  COMP.
           10 NI-KNR-MAST-TYP      PIC S9(04)  COMP.
           10 NI-MAST-LAST-UPDATE  PIC S9(04)  COMP.
           10 NI-MAST-DAT-BIS      PIC S9(04)  COMP.
        05  DCL-VIVB0055.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 MAST-DAT-VON         PIC X(26).
           10 KTY-MAST-TYP         PIC S9(4) USAGE COMP.
           10 KNR-MAST-TYP         PIC S9(4) USAGE COMP.
           10 MAST-LAST-UPDATE     PIC X(26).
           10 MAST-DAT-BIS         PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
