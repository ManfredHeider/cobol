      *----------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   CVVB0078.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\CVVB0078.CPv  $
      *
      *   Rev 2.1   08 Mar 2001 10:01:36   T13AE27
      *neu
      *
      *   Rev 2.0   20 Feb 2001 13:22:18   p13sp16
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(T13AES.VIVB0078)                                  *
      *        LIBRARY(T130005.AE010000.SRCDCL(CVVB0078))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0078 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      KTY_WB_TYP                     SMALLINT NOT NULL,
CCC004*      KNR_WB_TYP                     SMALLINT NOT NULL,
CCC004*      KTY_WERTUNGS_TYP               SMALLINT NOT NULL,
CCC004*      KNR_WERTUNGS_TYP               SMALLINT NOT NULL,
CCC004*      KAND_WERTUNGSSTUFE             SMALLINT NOT NULL,
CCC004*      KAND_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      KAND_TAETIG_BIS                DATE NOT NULL,
CCC004*      KAND_DRUCK_KZ                  SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AES.VIVB0078                    *
      ******************************************************************
       01  VIVB0078-GESAMT.
        05  NI-VIVB0078-TAB.
           10 NI-VIVB0078-ELE      PIC S9(04) BINARY OCCURS  9  TIMES.
        05  NI-VIVB0078            REDEFINES   NI-VIVB0078-TAB.
           10 NI-MA-ST-NR               PIC S9(04)       BINARY.
           10 NI-KTY-WB-TYP             PIC S9(04)       BINARY.
           10 NI-KNR-WB-TYP             PIC S9(04)       BINARY.
           10 NI-KTY-WERTUNGS-TYP       PIC S9(04)       BINARY.
           10 NI-KNR-WERTUNGS-TYP       PIC S9(04)       BINARY.
           10 NI-KAND-WERTUNGSSTUFE     PIC S9(04)       BINARY.
           10 NI-KAND-LAST-UPDATE       PIC S9(04)       BINARY.
           10 NI-KAND-TAETIG-BIS        PIC S9(04)       BINARY.
           10 NI-KAND-DRUCK-KZ          PIC S9(04)       BINARY.
        05 DCL-VIVB0078.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 KTY-WB-TYP           PIC S9(4) USAGE COMP.
           10 KNR-WB-TYP           PIC S9(4) USAGE COMP.
           10 KTY-WERTUNGS-TYP     PIC S9(4) USAGE COMP.
           10 KNR-WERTUNGS-TYP     PIC S9(4) USAGE COMP.
           10 KAND-WERTUNGSSTUFE   PIC S9(4) USAGE COMP.
           10 KAND-LAST-UPDATE     PIC X(26).
           10 KAND-TAETIG-BIS      PIC X(10).
           10 KAND-DRUCK-KZ        PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
