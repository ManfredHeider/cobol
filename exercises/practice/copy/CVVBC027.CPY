      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBC027.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBC027.CPv  $
      *
      *   Rev 2.1   Dec 18 2013 08:24:42   t13ae13
      *ASL00490: Familienabsicherungsplan alle Vertriebsgesellschaf
      *ten
      *
      *   Rev 2.0   Nov 11 2013 13:47:28   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVBC027)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBC027))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBC027 TABLE
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
CCC004*      FAPZ_UV_KZ                     CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBC027                           *
      ******************************************************************
       01  VIVBC027-GESAMT.
        05  NI-VIVBC027-TAB.
           10  NI-VIVBC027-ELE     PIC S9(04)  COMP   OCCURS  11 TIMES.
        05  NI-VIVBC027            REDEFINES   NI-VIVBC027-TAB.
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
        05  DCL-VIVBC027.
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
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
