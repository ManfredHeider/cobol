      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.06.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBB027.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBB027.CPv  $
      *
      *   Rev 2.1   Aug 19 2008 11:33:42   t13ex13
      *ASL00304: Familien-Absicherungsplan
      *
      *   Rev 2.0   Jun 27 2008 12:44:54   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVBB027)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBB027))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBB027 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      FAPZ_DAT_VON                   DATE NOT NULL,
CCC004*      FAPZ_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      FAPZ_VERS_NR                   DECIMAL(15, 0) NOT NULL,
CCC004*      FAPZ_STF                       CHAR(1) NOT NULL,
CCC004*      FAPZ_AMLVERS_SUM               DECIMAL(11, 2),
CCC004*      FAPZ_NEU_KZ                    CHAR(1),
CCC004*      FAPZ_BEITRAG                   DEC(11, 2)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBB027                           *
      ******************************************************************
       01  VIVBB027-GESAMT.
        05  NI-VIVBB027-TAB.
           10  NI-VIVBB027-ELE     PIC S9(04)  COMP   OCCURS  8  TIMES.
        05  NI-VIVBB027            REDEFINES   NI-VIVBB027-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-FAPZ-DAT-VON      PIC S9(04)  COMP.
           10 NI-FAPZ-LAST-UPDATE  PIC S9(04)  COMP.
           10 NI-FAPZ-VERS-NR      PIC S9(04)  COMP.
           10 NI-FAPZ-STF          PIC S9(04)  COMP.
           10 NI-FAPZ-AMLVERS-SUM  PIC S9(04)  COMP.
           10 NI-FAPZ-NEU-KZ       PIC S9(04)  COMP.
           10 NI-FAPZ-BEITRAG      PIC S9(04)  COMP.
        05  DCL-VIVBB027.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 FAPZ-DAT-VON         PIC X(10).
           10 FAPZ-LAST-UPDATE     PIC X(26).
           10 FAPZ-VERS-NR         PIC S999999999999999V USAGE COMP-3.
           10 FAPZ-STF             PIC X(1).
           10 FAPZ-AMLVERS-SUM     PIC S999999999V99 USAGE COMP-3.
           10 FAPZ-NEU-KZ          PIC X(1).
           10 FAPZ-BEITRAG         PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
