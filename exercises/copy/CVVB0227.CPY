      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.05.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0227.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVVB0227.CPv  $
      *
      *   Rev 2.2   May 11 2017 15:05:36   t13ex18
      *ASL00569: Förderprovision Zugriffsmodul für TBVB0227
      *
      *   Rev 2.1   May 02 2017 16:03:20   t13ex18
      *ASL00569: Förderprovision Zugriffsmodul für TBVB0227
      *
      *   Rev 2.0   May 02 2017 10:24:48   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: G.BRUDER                                         *
      *    DATUM:    02.05.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00569!02.05.17!2.1 ! NEU                       ! G.BRUDER  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0227) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0227)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0227 TABLE
CCC004*    ( FPRM_BETR_ST_NR             INTEGER NOT NULL
CCC004*    , FPRM_PART_ST_NR             INTEGER NOT NULL
CCC004*    , FPRM_STICHTAG               DATE NOT NULL
CCC004*    , KNR_ABZUG                   SMALLINT NOT NULL
CCC004*    , KTY_ABZUG                   SMALLINT NOT NULL
CCC004*    , FPRM_ANSPR_VOM              DATE NOT NULL
CCC004*    , FPRM_BETR_UMSATZ            DECIMAL(10, 1) NOT NULL
CCC004*    , FPRM_BETR_UMS_MAX           DECIMAL(10, 1) NOT NULL
CCC004*    , FPRM_PART_UMSATZ            DECIMAL(10, 1) NOT NULL
CCC004*    , FPRM_PART_ABZUG             DECIMAL(10, 1) NOT NULL
CCC004*    , FPRM_LAST_UPDATE            TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0227                           *
      *****************************************************************
 
       01  VIVB0227-GESAMT.
        05 NI-VIVB0227-TAB.
           10 NI-VIVB0227-ELE      PIC S9(04)  BINARY OCCURS  11.
        05 NI-VIVB0227             REDEFINES   NI-VIVB0227-TAB.
           10 NI-FPRM-BETR-ST-NR          PIC S9(04)       BINARY.
           10 NI-FPRM-PART-ST-NR          PIC S9(04)       BINARY.
           10 NI-FPRM-STICHTAG            PIC S9(04)       BINARY.
           10 NI-KNR-ABZUG                PIC S9(04)       BINARY.
           10 NI-KTY-ABZUG                PIC S9(04)       BINARY.
           10 NI-FPRM-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-FPRM-BETR-UMSATZ         PIC S9(04)       BINARY.
           10 NI-FPRM-BETR-UMS-MAX        PIC S9(04)       BINARY.
           10 NI-FPRM-PART-UMSATZ         PIC S9(04)       BINARY.
           10 NI-FPRM-PART-ABZUG          PIC S9(04)       BINARY.
           10 NI-FPRM-LAST-UPDATE         PIC S9(04)       BINARY.
 
        05 DCL-VIVB0227.
           10 FPRM-BETR-ST-NR             PIC S9(09)       BINARY.
           10 FPRM-PART-ST-NR             PIC S9(09)       BINARY.
           10 FPRM-STICHTAG               PIC  X(10).
           10 KNR-ABZUG                   PIC S9(04)       BINARY.
           10 KTY-ABZUG                   PIC S9(04)       BINARY.
           10 FPRM-ANSPR-VOM              PIC  X(10).
           10 FPRM-BETR-UMSATZ            PIC S9(09)V9(01) COMP-3.
           10 FPRM-BETR-UMS-MAX           PIC S9(09)V9(01) COMP-3.
           10 FPRM-PART-UMSATZ            PIC S9(09)V9(01) COMP-3.
           10 FPRM-PART-ABZUG             PIC S9(09)V9(01) COMP-3.
           10 FPRM-LAST-UPDATE            PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************
 

