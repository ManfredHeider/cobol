      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.04.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0225.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVVB0225.CPv  $
      *
      *   Rev 2.2   May 02 2017 15:54:58   t13ex18
      *ASL00569: Förderprovision Zugriffsmodul für TBVB0225
      *
      *   Rev 2.1   May 02 2017 13:55:02   t13ex18
      *ASL00569: Förderprovision Zugriffsmodul für TBVB0225
      *
      *   Rev 2.0   Apr 28 2017 10:56:04   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: G.BRUDER                                         *
      *    DATUM:    28.04.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00569!28.04.17!2.1 ! NEU                       ! G.BRUDER  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0225) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0225)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0225 TABLE
CCC004*    ( FPRA_ST_NR                  INTEGER NOT NULL
CCC004*    , FPRA_ANSPR_VOM              DATE NOT NULL
CCC004*    , KNR_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KTY_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , FPRA_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , FPRA_ANSP_BET_FREI          DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_ANSPR_STATUS            SMALLINT NOT NULL
CCC004*    , KNR_ANSPR_STATUS            SMALLINT NOT NULL
CCC004*    , FPRA_ANSP_BET_UPFL          DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0225                           *
      *****************************************************************
 
       01  VIVB0225-GESAMT.
        05 NI-VIVB0225-TAB.
           10 NI-VIVB0225-ELE      PIC S9(04)  BINARY OCCURS  11.
        05 NI-VIVB0225             REDEFINES   NI-VIVB0225-TAB.
           10 NI-FPRA-ST-NR               PIC S9(04)       BINARY.
           10 NI-FPRA-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-FPRA-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-FPRA-ANSP-BET-FREI       PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-FPRA-ANSP-BET-UPFL       PIC S9(04)       BINARY.
 
        05 DCL-VIVB0225.
           10 FPRA-ST-NR                  PIC S9(09)       BINARY.
           10 FPRA-ANSPR-VOM              PIC  X(10).
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 FPRA-LAST-UPDATE            PIC  X(26).
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 FPRA-ANSP-BET-FREI          PIC S9(09)V9(02) COMP-3.
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 FPRA-ANSP-BET-UPFL          PIC S9(09)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************
 

