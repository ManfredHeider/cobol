      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.06.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA082.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/CVVBA082.CPv  $
      *
      *   Rev 2.1   Jul 25 2003 08:45:28   t13ae27
      *Erweiterung wegen OLB-2003
      *
      *   Rev 2.0   Jun 26 2003 11:58:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(T13AES.VIVBA082)                                  *
      *        LIBRARY(T130005.AE010000.SRCDCL(CVVBA082))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA082 TABLE
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
CCC004*     KNR_REGION                       SMALLINT NOT NULL,
CCC004*     LIST_BETRAG_1                    DECIMAL(11, 2),
CCC004*     LIST_BETRAG_2                    DECIMAL(11, 2),
CCC004*     LIST_BETRAG_3                    DECIMAL(11, 2),
CCC004*     LIST_BETRAG_4                    DECIMAL(11, 2),
CCC004*     LIST_BETRAG_5                    DECIMAL(11, 2),
CCC004*     LIST_SUM_BETRAG                  DECIMAL(11, 2),
CCC004*     LIST_PROZSATZ                    DECIMAL(7, 2),
CCC004*     LIST_BEMERKUNG                   CHAR(20)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AES.VIVBA082                    *
      ******************************************************************
       01  VIVBA082-GESAMT.
        05  NI-VIVBA082-TAB.
           10 NI-VIVBA082-ELE      PIC S9(04) BINARY OCCURS 29 TIMES.
        05  NI-VIVBA082            REDEFINES   NI-VIVBA082-TAB.
           10 NI-MA-ST-NR                PIC S9(04)  BINARY.
           10 NI-LIST-MONAT              PIC S9(04)  BINARY.
           10 NI-KTY-WB-TYP              PIC S9(04)  BINARY.
           10 NI-KNR-WB-TYP              PIC S9(04)  BINARY.
           10 NI-KTY-WERTUNGS-TYP        PIC S9(04)  BINARY.
           10 NI-KNR-WERTUNGS-TYP        PIC S9(04)  BINARY.
           10 NI-LIST-STUF-BEZ           PIC S9(04)  BINARY.
           10 NI-LIST-LAST-UPDATE        PIC S9(04)  BINARY.
           10 NI-DIR-NR                  PIC S9(04)  BINARY.
           10 NI-LIST-NAME               PIC S9(04)  BINARY.
           10 NI-LIST-VORNAME            PIC S9(04)  BINARY.
           10 NI-LIST-RANG               PIC S9(04)  BINARY.
           10 NI-LIST-GEWINNER-KZ        PIC S9(04)  BINARY.
           10 NI-LIST-EH-1               PIC S9(04)  BINARY.
           10 NI-LIST-EH-2               PIC S9(04)  BINARY.
           10 NI-LIST-EH-3               PIC S9(04)  BINARY.
           10 NI-LIST-EH-4               PIC S9(04)  BINARY.
           10 NI-LIST-EH-5               PIC S9(04)  BINARY.
           10 NI-LIST-SUM-EH             PIC S9(04)  BINARY.
           10 NI-KTY-REGION              PIC S9(04)  BINARY.
           10 NI-KNR-REGION              PIC S9(04)  BINARY.
           10 NI-LIST-BETRAG-1           PIC S9(04)  BINARY.
           10 NI-LIST-BETRAG-2           PIC S9(04)  BINARY.
           10 NI-LIST-BETRAG-3           PIC S9(04)  BINARY.
           10 NI-LIST-BETRAG-4           PIC S9(04)  BINARY.
           10 NI-LIST-BETRAG-5           PIC S9(04)  BINARY.
           10 NI-LIST-SUM-BETRAG         PIC S9(04)  BINARY.
           10 NI-LIST-PROZSATZ           PIC S9(04)  BINARY.
           10 NI-LIST-BEMERKUNG          PIC S9(04)  BINARY.
      *
        05 DCL-VIVBA082.
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
           10 LIST-BETRAG-1              PIC S9(9)V99 COMP-3.
           10 LIST-BETRAG-2              PIC S9(9)V99 COMP-3.
           10 LIST-BETRAG-3              PIC S9(9)V99 COMP-3.
           10 LIST-BETRAG-4              PIC S9(9)V99 COMP-3.
           10 LIST-BETRAG-5              PIC S9(9)V99 COMP-3.
           10 LIST-SUM-BETRAG            PIC S9(9)V99 COMP-3.
           10 LIST-PROZSATZ              PIC S9(5)V9(2) USAGE COMP-3.
           10 LIST-BEMERKUNG             PIC X(20).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 29      *
      ******************************************************************
