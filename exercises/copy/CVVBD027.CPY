      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBD027.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBD027.CPv  $
      *
      *   Rev 2.1   May 22 2014 10:10:22   t13ae13
      *ASL00509: FAP Dialogänderung Vb 31
      *
      *   Rev 2.0   Jan 21 2014 11:32:02   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVBD027)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBD027))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBD027 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      FAPZ_DAT_VON                   DATE NOT NULL,
CCC004*      FAPZ_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      FAPZ_VERS_NR                   DECIMAL(15, 0) NOT NULL,
CCC004*      FAPZ_STF                       CHAR(1) NOT NULL,
CCC004*      FAPZ_AMLVERS_SUM               DECIMAL(11, 2),
CCC004*      FAPZ_NEU_KZ                    CHAR(1),
CCC004*      FAPZ_BEITRAG                   DEC(11, 2),
CCC004*      FAPZ_SELBSTBET                 DEC(11, 2),
CCC004*      FAPZ_DVAG_STF                  CHAR(1),
CCC004*      FAPZ_UV_KZ                     CHAR(1),
CCC004*      FAPZ_AMLUV_SUM                 DEC(11, 2),
CCC004*      FAPZ_DVAGUV_SUM                DEC(11, 2),
CCC004*      FAPZ_DVAGVER_SUM               DEC(11, 2),
CCC004*      FAPZ_BEITRAG_UV                DEC(11, 2),
CCC004*      FAPZ_SELBSTB_UV                DEC(11, 2),
CCC004*      FAPZ_TEILABL_KZ                CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBD027                           *
      ******************************************************************
       01  VIVBD027-GESAMT.
        05  NI-VIVBD027-TAB.
           10  NI-VIVBD027-ELE     PIC S9(04)  COMP   OCCURS  17 TIMES.
        05  NI-VIVBD027            REDEFINES   NI-VIVBD027-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-FAPZ-DAT-VON      PIC S9(04)  COMP.
           10 NI-FAPZ-LAST-UPDATE  PIC S9(04)  COMP.
           10 NI-FAPZ-VERS-NR      PIC S9(04)  COMP.
           10 NI-FAPZ-STF          PIC S9(04)  COMP.
           10 NI-FAPZ-AMLVERS-SUM  PIC S9(04)  COMP.
           10 NI-FAPZ-NEU-KZ       PIC S9(04)  COMP.
           10 NI-FAPZ-BEITRAG      PIC S9(04)  COMP.
           10 NI-FAPZ-SELBSTBET    PIC S9(04)  COMP.
           10 NI-FAPZ-DVAG-STF     PIC S9(04)  COMP.
           10 NI-FAPZ-UV-KZ        PIC S9(04)  COMP.
           10 NI-FAPZ-AMLUV-SUM    PIC S9(04)  COMP.
           10 NI-FAPZ-DVAGUV-SUM   PIC S9(04)  COMP.
           10 NI-FAPZ-DVAGVER-SUM  PIC S9(04)  COMP.
           10 NI-FAPZ-BEITRAG-UV   PIC S9(04)  COMP.
           10 NI-FAPZ-SELBSTB-UV   PIC S9(04)  COMP.
           10 NI-FAPZ-TEILABL-KZ   PIC S9(04)  COMP.
        05  DCL-VIVBD027.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 FAPZ-DAT-VON         PIC X(10).
           10 FAPZ-LAST-UPDATE     PIC X(26).
           10 FAPZ-VERS-NR         PIC S999999999999999V USAGE COMP-3.
           10 FAPZ-STF             PIC X(1).
           10 FAPZ-AMLVERS-SUM     PIC S999999999V99 USAGE COMP-3.
           10 FAPZ-NEU-KZ          PIC X(1).
           10 FAPZ-BEITRAG         PIC S999999999V99 USAGE COMP-3.
           10 FAPZ-SELBSTBET       PIC S999999999V99 USAGE COMP-3.
           10 FAPZ-DVAG-STF        PIC X(1).
           10 FAPZ-UV-KZ           PIC X(1).
           10 FAPZ-AMLUV-SUM       PIC S999999999V99 USAGE COMP-3.
           10 FAPZ-DVAGUV-SUM      PIC S999999999V99 USAGE COMP-3.
           10 FAPZ-DVAGVER-SUM     PIC S999999999V99 USAGE COMP-3.
           10 FAPZ-BEITRAG-UV      PIC S999999999V99 USAGE COMP-3.
           10 FAPZ-SELBSTB-UV      PIC S999999999V99 USAGE COMP-3.
           10 FAPZ-TEILABL-KZ      PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************
