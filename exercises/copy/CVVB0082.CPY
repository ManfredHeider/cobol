      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0082.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\CVVB0082.CPv  $
      *
      *   Rev 2.3   31 May 2001 10:41:48   t13ae27
      *neu
      *
      *   Rev 2.2   18 May 2001 15:46:22   t13ae30
      * 
      *
      *   Rev 2.1   17 May 2001 10:55:38   t13ae30
      *AAS00274 - PIN für VB-Portal
      *
      *   Rev 2.0   20 Feb 2001 13:22:22   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(T13AES.VIVB0082)                                  *
      *        LIBRARY(T130005.AE010000.SRCDCL(CVVB0082))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0082 TABLE
CCC004*    (MA_ST_NR                         INTEGER NOT NULL,
CCC004*     LIST_MONAT                       DATE,
CCC004*     KTY_WB_TYP                       SMALLINT NOT NULL,
CCC004*     KNR_WB_TYP                       SMALLINT NOT NULL,
CCC004*     KTY_WERTUNGS_TYP                 SMALLINT NOT NULL,
CCC004*     KNR_WERTUNGS_TYP                 SMALLINT NOT NULL,
CCC004*     LIST_STUF_BEZ                    CHAR(10),
CCC004*     LIST_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*     DIR_NR                           SMALLINT NOT NULL,
CCC004*     LIST_NAME                        CHAR(25),
CCC004*     LIST_VORNAME                     CHAR(20),
CCC004*     LIST_RANG                        INTEGER NOT NULL,
CCC004*     LIST_GEWINNER_KZ                 CHAR(1) NOT NULL,
CCC004*     LIST_EH_1                        DECIMAL(09, 1),
CCC004*     LIST_EH_2                        DECIMAL(09, 1),
CCC004*     LIST_EH_3                        DECIMAL(09, 1),
CCC004*     LIST_EH_4                        DECIMAL(09, 1),
CCC004*     LIST_EH_5                        DECIMAL(09, 1),
CCC004*     LIST_SUM_EH                      DECIMAL(09, 1),
CCC004*     KTY_REGION                       SMALLINT NOT NULL,
CCC004*     KNR_REGION                       SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AES.VIVB0082                    *
      ******************************************************************
       01  VIVB0082-GESAMT.
        05  NI-VIVB0082-TAB.
           10 NI-VIVB0082-ELE      PIC S9(04) BINARY OCCURS 6 TIMES.
        05  NI-VIVB0082            REDEFINES   NI-VIVB0082-TAB.
           10 NI-LIST-EH-1               PIC S9(04)  BINARY.
           10 NI-LIST-EH-2               PIC S9(04)  BINARY.
           10 NI-LIST-EH-3               PIC S9(04)  BINARY.
           10 NI-LIST-EH-4               PIC S9(04)  BINARY.
           10 NI-LIST-EH-5               PIC S9(04)  BINARY.
           10 NI-LIST-SUM-EH             PIC S9(04)  BINARY.
        05 DCL-VIVB0082.
           10 MA-ST-NR                   PIC S9(9)   USAGE COMP.
           10 LIST-MONAT                 PIC X(10).
           10 KTY-WB-TYP                 PIC S9(4)   USAGE COMP.
           10 KNR-WB-TYP                 PIC S9(4)   USAGE COMP.
           10 KTY-WERTUNGS-TYP           PIC S9(4)   USAGE COMP.
           10 KNR-WERTUNGS-TYP           PIC S9(4)   USAGE COMP.
           10 LIST-STUF-BEZ              PIC X(10).
           10 LIST-LAST-UPDATE           PIC X(26).
           10 DIR-NR                     PIC S9(4)   USAGE COMP.
           10 LIST-NAME                  PIC X(25).
           10 LIST-VORNAME               PIC X(20).
           10 LIST-RANG                  PIC S9(9)   USAGE COMP.
           10 LIST-GEWINNER-KZ           PIC X(01).
           10 LIST-EH-1                  PIC S9(9)V9 COMP-3.
           10 LIST-EH-2                  PIC S9(9)V9 COMP-3.
           10 LIST-EH-3                  PIC S9(9)V9 COMP-3.
           10 LIST-EH-4                  PIC S9(9)V9 COMP-3.
           10 LIST-EH-5                  PIC S9(9)V9 COMP-3.
           10 LIST-SUM-EH                PIC S9(9)V9 COMP-3.
           10 KTY-REGION                 PIC S9(4)   USAGE COMP.
           10 KNR-REGION                 PIC S9(4)   USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 21      *
      ******************************************************************
