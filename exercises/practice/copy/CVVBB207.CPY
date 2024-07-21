      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.03.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBB207.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBB207.CPv  $
      *
      *   Rev 2.1   Apr 17 2013 17:07:00   t13ae36
      *AAS01058: Allfinanz Wettbewerbe 2013: Erfolge im Eig u. Gr.-
      *Geschäft ohne HBO
      *
      *   Rev 2.0   Mar 19 2013 09:04:04   t13ae36
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *****************************************************************
      *    ERSTELLT: FRANKE                                           *
      *    DATUM:    12.03.2013                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AAS01058!12.03.13!2.1 ! NEU aus CCVBA207          ! FRANKE    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBB207) LZA                                    *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBB207)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBB207 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , LZA_MONAT                   DATE NOT NULL
CCC004*    , LZA_BEITRAG_AKT             DECIMAL(11, 2) NOT NULL
CCC004*    , LZA_BEITRAG_ALT             DECIMAL(11, 2) NOT NULL
CCC004*    , LZA_BESTAND_HIN             DECIMAL(11, 2) NOT NULL
CCC004*    , LZA_BESTAND_WEG             DECIMAL(11, 2) NOT NULL
CCC004*    , LZA_BESTHIN_1MON            DECIMAL(11, 2)
CCC004*    , LZA_BESTWEG_1MON            DECIMAL(11, 2)
CCC004*    , LZA_BEITRGRP_AKT            DECIMAL(11, 2)
CCC004*    , LZA_BEITRGRP_ALT            DECIMAL(11, 2)
CCC004*    , LZA_BEITREIG_AKT            DECIMAL(11, 2)
CCC004*    , LZA_EH_EIG_PUR              DECIMAL(10, 1)
CCC004*    , LZA_BEIG_PUR_AKT            DECIMAL(11, 2)
CCC004*    , LZA_BEIG_PUR_ALT            DECIMAL(11, 2)
CCC004*    , LZA_BGRP_PUR_AKT            DECIMAL(11, 2)
CCC004*    , LZA_BGRP_PUR_ALT            DECIMAL(11, 2)
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBB207                           *
      *****************************************************************

       01  VIVBB207-GESAMT.
        05 NI-VIVBB207-TAB.
           10 NI-VIVBB207-ELE      PIC S9(04)  BINARY OCCURS  16.
        05 NI-VIVBB207             REDEFINES   NI-VIVBB207-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-LZA-MONAT                PIC S9(04)       BINARY.
           10 NI-LZA-BEITRAG-AKT          PIC S9(04)       BINARY.
           10 NI-LZA-BEITRAG-ALT          PIC S9(04)       BINARY.
           10 NI-LZA-BESTAND-HIN          PIC S9(04)       BINARY.
           10 NI-LZA-BESTAND-WEG          PIC S9(04)       BINARY.
           10 NI-LZA-BESTHIN-1MON         PIC S9(04)       BINARY.
           10 NI-LZA-BESTWEG-1MON         PIC S9(04)       BINARY.
           10 NI-LZA-BEITRGRP-AKT         PIC S9(04)       BINARY.
           10 NI-LZA-BEITRGRP-ALT         PIC S9(04)       BINARY.
           10 NI-LZA-BEITREIG-AKT         PIC S9(04)       BINARY.
           10 NI-LZA-EH-EIG-PUR           PIC S9(04)       BINARY.
           10 NI-LZA-BEIG-PUR-AKT         PIC S9(04)       BINARY.
           10 NI-LZA-BEIG-PUR-ALT         PIC S9(04)       BINARY.
           10 NI-LZA-BGRP-PUR-AKT         PIC S9(04)       BINARY.
           10 NI-LZA-BGRP-PUR-ALT         PIC S9(04)       BINARY.

        05 DCL-VIVBB207.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 LZA-MONAT                   PIC  X(10).
           10 LZA-BEITRAG-AKT             PIC S9(09)V9(02) COMP-3.
           10 LZA-BEITRAG-ALT             PIC S9(09)V9(02) COMP-3.
           10 LZA-BESTAND-HIN             PIC S9(09)V9(02) COMP-3.
           10 LZA-BESTAND-WEG             PIC S9(09)V9(02) COMP-3.
           10 LZA-BESTHIN-1MON            PIC S9(09)V9(02) COMP-3.
           10 LZA-BESTWEG-1MON            PIC S9(09)V9(02) COMP-3.
           10 LZA-BEITRGRP-AKT            PIC S9(09)V9(02) COMP-3.
           10 LZA-BEITRGRP-ALT            PIC S9(09)V9(02) COMP-3.
           10 LZA-BEITREIG-AKT            PIC S9(09)V9(02) COMP-3.
           10 LZA-EH-EIG-PUR              PIC S9(09)V9(01) COMP-3.
           10 LZA-BEIG-PUR-AKT            PIC S9(09)V9(02) COMP-3.
           10 LZA-BEIG-PUR-ALT            PIC S9(09)V9(02) COMP-3.
           10 LZA-BGRP-PUR-AKT            PIC S9(09)V9(02) COMP-3.
           10 LZA-BGRP-PUR-ALT            PIC S9(09)V9(02) COMP-3.
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  16                    *
      *****************************************************************
      *---------------------------------------------------------------*