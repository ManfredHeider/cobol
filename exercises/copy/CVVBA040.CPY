      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   cvvba040.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\cvvba040.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Nov 22 1996 08:27:32   T13AE24
      *Version bei erster Prod.-Übergabe vergessen
      *
      *   Rev 1.0   18 Nov 1996 13:13:52   P13SP17
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    VERSION
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0299!21.11.96!    ! NEU                       ! SCHMIDT  *  00140000
      *         !        !    !                           !          *  00143000
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(VIVBA040)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA040))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA040 TABLE
CCC004*    ( KL_ST_NR                       INTEGER NOT NULL,
CCC004*      KL_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      KL_DAT_VON                     DATE NOT NULL,
CCC004*      KL_DAT_BIS                     DATE,
CCC004*      KL_PROV_STZ_AP                 DECIMAL(5, 2) NOT NULL,
CCC004*      KL_PROV_STZ_GP                 DECIMAL(5, 2) NOT NULL,
CCC004*      KL_PROZ_STZ                    DECIMAL(5, 2) NOT NULL,
CCC004*      KL_RCK_STZ_DAT                 DATE NOT NULL,
CCC004*      KL_RCK_STZ                     DECIMAL(5, 2) NOT NULL,
CCC004*      MA_ST_NR                       INTEGER,
CCC004*      KNR_KL_TYP                     SMALLINT NOT NULL,
CCC004*      KTY_KL_TYP                     SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA040                           *
      ******************************************************************
       01  VIVBA040-GESAMT.
        05  NI-VIVBA040-TAB.
           10  NI-VIVBA040-ELE     PIC S9(04)  COMP   OCCURS 12  TIMES.
        05  NI-VIVBA040            REDEFINES   NI-VIVBA040-TAB.
           10 NI-KL-ST-NR          PIC S9(04)  COMP.
           10 NI-KL-LAST-UPDATE    PIC S9(04)  COMP.
           10 NI-KL-DAT-VON        PIC S9(04)  COMP.
           10 NI-KL-DAT-BIS        PIC S9(04)  COMP.
           10 NI-KL-PROV-STZ-AP    PIC S9(04)  COMP.
           10 NI-KL-PROV-STZ-GP    PIC S9(04)  COMP.
           10 NI-KL-PROZ-STZ       PIC S9(04)  COMP.
           10 NI-KL-RCK-STZ-DAT    PIC S9(04)  COMP.
           10 NI-KL-RCK-STZ        PIC S9(04)  COMP.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-KNR-KL-TYP        PIC S9(04)  COMP.
           10 NI-KTY-KL-TYP        PIC S9(04)  COMP.
        05  DCL-VIVBA040.
           10 KL-ST-NR             PIC S9(9) USAGE COMP.
           10 KL-LAST-UPDATE       PIC X(26).
           10 KL-DAT-VON           PIC X(10).
           10 KL-DAT-BIS           PIC X(10).
           10 KL-PROV-STZ-AP       PIC S999V99 USAGE COMP-3.
           10 KL-PROV-STZ-GP       PIC S999V99 USAGE COMP-3.
           10 KL-PROZ-STZ          PIC S999V99 USAGE COMP-3.
           10 KL-RCK-STZ-DAT       PIC X(10).
           10 KL-RCK-STZ           PIC S999V99 USAGE COMP-3.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 KNR-KL-TYP           PIC S9(4) USAGE COMP.
           10 KTY-KL-TYP           PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
