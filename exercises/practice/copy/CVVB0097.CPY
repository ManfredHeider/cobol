      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0097.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0097.CPv  $
      *
      *   Rev 2.1   Mar 27 2006 08:18:26   T13AE27
      *AAS00602: Jahres-Wettbewerb 2006 DVAG Wien
      *
      *   Rev 2.0   Mar 03 2006 14:44:30   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * DCLGEN TABLE(T13AES.VIVB0097)                                  *
      *        LIBRARY(T130005.AE010000.SRCDCL(CVVB0097))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0097 TABLE
CCC004*    ( WBW01_MA_ST_NR                 INTEGER NOT NULL,
CCC004*      WBW01_MONAT                    DATE NOT NULL,
CCC004*      WBW01_VERTRAG_NR               CHAR(15),
CCC004*      WBW01_GES_NR                   SMALLINT NOT NULL,
CCC004*      WBW01_PRODUKT_NR               SMALLINT NOT NULL,
CCC004*      WBW01_STORNO_SCHL              CHAR(01)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AES.VIVB0097                    *
      ******************************************************************
       01  VIVB0097-GESAMT.
        05  NI-VIVB0097-TAB.
           10 NI-VIVB0097-ELE      PIC S9(04) BINARY OCCURS 06  TIMES.
        05  NI-VIVB0097            REDEFINES   NI-VIVB0097-TAB.
           10 NI-WBW01-MA-ST-NR         PIC S9(04)       BINARY.
           10 NI-WBW01-MONAT            PIC S9(04)       BINARY.
           10 NI-WBW01-VERTRAG-NR       PIC S9(04)       BINARY.
           10 NI-WBW01-GES-NR           PIC S9(04)       BINARY.
           10 NI-WBW01-PRODUKT-NR       PIC S9(04)       BINARY.
           10 NI-WBW01-STORNO-SCHL      PIC S9(04)       BINARY.
        05 DCL-VIVB0097.
           10 WBW01-MA-ST-NR            PIC S9(9)        BINARY.
           10 WBW01-MONAT               PIC X(010).
           10 WBW01-VERTRAG-NR          PIC X(015).
           10 WBW01-GES-NR              PIC S9(4)        BINARY.
           10 WBW01-PRODUKT-NR          PIC S9(4)        BINARY.
           10 WBW01-STORNO-SCHL         PIC X(001).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 06      *
      ******************************************************************
