      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.06.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA079.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/CVVBA079.CPv  $
      *
      *   Rev 2.1   Jul 25 2003 08:43:46   t13ae27
      *Erweiterung wegen OLB-2003 
      *
      *   Rev 2.0   Jun 26 2003 11:58:38   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(T13AES.VIVBA079)                                  *
      *        LIBRARY(T130005.AE010000.SRCDCL(CVVBA079))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA079 TABLE
CCC004*    ( MA_ST_NR                       INTEGER        NOT NULL,
CCC004*      ERG_MONAT                      DATE           NOT NULL,
CCC004*      KTY_WB_TYP                     SMALLINT       NOT NULL,
CCC004*      KNR_WB_TYP                     SMALLINT       NOT NULL,
CCC004*      KTY_WERTUNGS_TYP               SMALLINT       NOT NULL,
CCC004*      KNR_WERTUNGS_TYP               SMALLINT       NOT NULL,
CCC004*      ERG_WERTUNGSSTUFE              SMALLINT       NOT NULL,
CCC004*      ERG_LAST_UPDATE                TIMESTAMP      NOT NULL,
CCC004*      ERG_EQF_1                      DECIMAL(5, 2)  NOT NULL,
CCC004*      ERG_EQF_2                      DECIMAL(5, 2)  NOT NULL,
CCC004*      ERG_SUM_EH                     DECIMAL(10, 1) NOT NULL,
CCC004*      ERG_EH_1                       DECIMAL(10, 1),
CCC004*      ERG_EH_2                       DECIMAL(10, 1),
CCC004*      ERG_EH_3                       DECIMAL(10, 1),
CCC004*      ERG_EH_4                       DECIMAL(10, 1),
CCC004*      ERG_EH_5                       DECIMAL(10, 1),
CCC004*      ERG_SUM_BETR                   DECIMAL(11, 2) NOT NULL,
CCC004*      ERG_BETR_1                     DECIMAL(11, 2),
CCC004*      ERG_BETR_2                     DECIMAL(11, 2),
CCC004*      ERG_BETR_3                     DECIMAL(11, 2),
CCC004*      ERG_BETR_4                     DECIMAL(11, 2),
CCC004*      ERG_BETR_5                     DECIMAL(11, 2),
CCC004*      ERG_PROZSATZ                   DECIMAL(7, 2),
CCC004*      ERG_BEMERKUNG                  CHAR(20)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AES.VIVBA079                    *
      ******************************************************************
       01  VIVBA079-GESAMT.
        05  NI-VIVBA079-TAB.
           10 NI-VIVBA079-ELE   PIC S9(04)  BINARY OCCURS  24 TIMES.
        05  NI-VIVBA079         REDEFINES   NI-VIVBA079-TAB.
           10 NI-MA-ST-NR           PIC S9(04)       BINARY.
           10 NI-ERG-MONAT          PIC S9(04)       BINARY.
           10 NI-KTY-WB-TYP         PIC S9(04)       BINARY.
           10 NI-KNR-WB-TYP         PIC S9(04)       BINARY.
           10 NI-KTY-WERTUNGS-TYP   PIC S9(04)       BINARY.
           10 NI-KNR-WERTUNGS-TYP   PIC S9(04)       BINARY.
           10 NI-ERG-WERTUNGSSTUFE  PIC S9(04)       BINARY.
           10 NI-ERG-LAST-UPDATE    PIC S9(04)       BINARY.
           10 NI-ERG-EQF-1          PIC S9(04)       BINARY.
           10 NI-ERG-EQF-2          PIC S9(04)       BINARY.
           10 NI-ERG-SUM-EH         PIC S9(04)       BINARY.
           10 NI-ERG-EH-1           PIC S9(04)       BINARY.
           10 NI-ERG-EH-2           PIC S9(04)       BINARY.
           10 NI-ERG-EH-3           PIC S9(04)       BINARY.
           10 NI-ERG-EH-4           PIC S9(04)       BINARY.
           10 NI-ERG-EH-5           PIC S9(04)       BINARY.
           10 NI-ERG-SUM-BETR       PIC S9(04)       BINARY.
           10 NI-ERG-BETR-1         PIC S9(04)       BINARY.
           10 NI-ERG-BETR-2         PIC S9(04)       BINARY.
           10 NI-ERG-BETR-3         PIC S9(04)       BINARY.
           10 NI-ERG-BETR-4         PIC S9(04)       BINARY.
           10 NI-ERG-BETR-5         PIC S9(04)       BINARY.
           10 NI-ERG-PROZSATZ       PIC S9(04)       BINARY.
           10 NI-ERG-BEMERKUNG      PIC S9(04)       BINARY.

      *
        05 DCL-VIVBA079.
           10 MA-ST-NR             PIC S9(9)      USAGE COMP.
           10 ERG-MONAT            PIC X(10).
           10 KTY-WB-TYP           PIC S9(4)      USAGE COMP.
           10 KNR-WB-TYP           PIC S9(4)      USAGE COMP.
           10 KTY-WERTUNGS-TYP     PIC S9(4)      USAGE COMP.
           10 KNR-WERTUNGS-TYP     PIC S9(4)      USAGE COMP.
           10 ERG-WERTUNGSSTUFE    PIC S9(4)      USAGE COMP.
           10 ERG-LAST-UPDATE      PIC X(26).
           10 ERG-EQF-1            PIC S9(3)V9(2) USAGE COMP-3.
           10 ERG-EQF-2            PIC S9(3)V9(2) USAGE COMP-3.
           10 ERG-SUM-EH           PIC S9(9)V9(1) USAGE COMP-3.
           10 ERG-EH-1             PIC S9(9)V9(1) USAGE COMP-3.
           10 ERG-EH-2             PIC S9(9)V9(1) USAGE COMP-3.
           10 ERG-EH-3             PIC S9(9)V9(1) USAGE COMP-3.
           10 ERG-EH-4             PIC S9(9)V9(1) USAGE COMP-3.
           10 ERG-EH-5             PIC S9(9)V9(1) USAGE COMP-3.
           10 ERG-SUM-BETR         PIC S9(9)V9(2) USAGE COMP-3.
           10 ERG-BETR-1           PIC S9(9)V9(2) USAGE COMP-3.
           10 ERG-BETR-2           PIC S9(9)V9(2) USAGE COMP-3.
           10 ERG-BETR-3           PIC S9(9)V9(2) USAGE COMP-3.
           10 ERG-BETR-4           PIC S9(9)V9(2) USAGE COMP-3.
           10 ERG-BETR-5           PIC S9(9)V9(2) USAGE COMP-3.
           10 ERG-PROZSATZ         PIC S9(5)V9(2) USAGE COMP-3.
           10 ERG-BEMERKUNG        PIC X(20).

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 24      *
      ******************************************************************


