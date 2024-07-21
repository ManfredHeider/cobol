      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.07.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA223.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA223.CPv  $
      *
      *   Rev 2.1   Aug 10 2017 17:03:56   t13ae13
      *ASL00567: LUCY2 für Eliteclubbonus SL13 (swl) 
      *
      *   Rev 2.0   Jul 12 2017 10:53:00   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: WALB                                             *
      *    DATUM:    12.07.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00567!12.07.17!2.1 ! NEU                       ! WALB      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA223) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA223)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBA223 TABLE
CCC004*    ( ECBA_ST_NR                  INTEGER NOT NULL
CCC004*    , ECBA_ANSPR_VOM              DATE NOT NULL
CCC004*    , KNR_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KTY_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , ECBA_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , ECBA_PART_EH                DECIMAL(10, 1) NOT NULL
CCC004*    , ECBA_FAKTOR                 DECIMAL(05, 2) NOT NULL
CCC004*    , ECBA_ANSPR_BETRAG           DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_ANSPR_STATUS            SMALLINT NOT NULL
CCC004*    , KNR_ANSPR_STATUS            SMALLINT NOT NULL
CCC004*    , ECBA_PART_EH_UPFL           DECIMAL(10, 1) NOT NULL
CCC004*    , ECBA_EH_UFREI_RECH          DECIMAL(10, 1) NOT NULL
CCC004*    , ECBA_EH_UPFL_RECH           DECIMAL(10, 1) NOT NULL
CCC004*    , ECBA_BETRAG_UPFL            DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA223                           *
      *****************************************************************

       01  VIVBA223-GESAMT.
        05 NI-VIVBA223-TAB.
           10 NI-VIVBA223-ELE      PIC S9(04)  BINARY OCCURS  16.
        05 NI-VIVBA223             REDEFINES   NI-VIVBA223-TAB.
           10 NI-ECBA-ST-NR               PIC S9(04)       BINARY.
           10 NI-ECBA-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-ECBA-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-ECBA-PART-EH             PIC S9(04)       BINARY.
           10 NI-ECBA-FAKTOR              PIC S9(04)       BINARY.
           10 NI-ECBA-ANSPR-BETRAG        PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-ECBA-PART-EH-UPFL        PIC S9(04)       BINARY.
           10 NI-ECBA-EH-UFREI-RECH       PIC S9(04)       BINARY.
           10 NI-ECBA-EH-UPFL-RECH        PIC S9(04)       BINARY.
           10 NI-ECBA-BETRAG-UPFL         PIC S9(04)       BINARY.

        05 DCL-VIVBA223.
           10 ECBA-ST-NR                  PIC S9(09)       BINARY.
           10 ECBA-ANSPR-VOM              PIC  X(10).
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 ECBA-LAST-UPDATE            PIC  X(26).
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 ECBA-PART-EH                PIC S9(09)V9(01) COMP-3.
           10 ECBA-FAKTOR                 PIC S9(03)V9(02) COMP-3.
           10 ECBA-ANSPR-BETRAG           PIC S9(09)V9(02) COMP-3.
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 ECBA-PART-EH-UPFL           PIC S9(09)V9(01) COMP-3.
           10 ECBA-EH-UFREI-RECH          PIC S9(09)V9(01) COMP-3.
           10 ECBA-EH-UPFL-RECH           PIC S9(09)V9(01) COMP-3.
           10 ECBA-BETRAG-UPFL            PIC S9(09)V9(02) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  16                    *
      *****************************************************************

