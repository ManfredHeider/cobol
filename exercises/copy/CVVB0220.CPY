      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.08.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $WORKFILE:   CVVB0220.CPY  $
      *    $Revision:   2.3  $
      *
      *    $LOG:   L:/MF/CPY/CVVB0220.CPV  $
      *
      *   Rev 2.1   Feb 17 2016 08:47:12   t13ae14
      *ASL00530: SL12 - Dialog Ausbilder-Zuschuss
      *
      *   Rev 2.0   Aug 11 2015 13:00:38   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: -                                                *
      *    DATUM:    11.08.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * -       !11.08.15!2.1 ! NEU                       ! -         *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0220) AZERF                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0220)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0220 TABLE
CCC004*    ( AZERF_PART_VBNR             INTEGER NOT NULL
CCC004*    , AZERF_BETR_VBNR             INTEGER NOT NULL
CCC004*    , DIR_NR                      SMALLINT NOT NULL
CCC004*    , MS_NR                       SMALLINT NOT NULL
CCC004*    , AZERF_ANSPR_VOM             DATE NOT NULL
CCC004*    , AZERF_ANSPR_SEIT            DATE NOT NULL
CCC004*    , AZERF_ANSPR_BIS             DATE
CCC004*    , AZERF_EBENE                 SMALLINT NOT NULL
CCC004*    , AZERF_VERT_FAKTOR           SMALLINT NOT NULL
CCC004*    , AZERF_BETR_FAKTOR           DECIMAL(11, 2) NOT NULL
CCC004*    , AZERF_ANZAHL_MON            SMALLINT NOT NULL
CCC004*    , AZERF_BETRAG                DECIMAL(11, 2) NOT NULL
CCC004*    , AZERF_EH_GRENZE             DECIMAL(10, 1) NOT NULL
CCC004*    , AZERF_EH_EIGEN              DECIMAL(10, 1) NOT NULL
CCC004*    , AZERF_EH_EIGEN_MON          DECIMAL(10, 1) NOT NULL
CCC004*    , AZERF_EQF_EIGEN             DECIMAL(05, 2) NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , AZERF_LAST_UPDATE           TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0220                           *
      *****************************************************************
 
       01  VIVB0220-GESAMT.
        05 NI-VIVB0220-TAB.
           10 NI-VIVB0220-ELE      PIC S9(04)  BINARY OCCURS  19.
        05 NI-VIVB0220             REDEFINES   NI-VIVB0220-TAB.
           10 NI-AZERF-PART-VBNR          PIC S9(04)       BINARY.
           10 NI-AZERF-BETR-VBNR          PIC S9(04)       BINARY.
           10 NI-DIR-NR                   PIC S9(04)       BINARY.
           10 NI-MS-NR                    PIC S9(04)       BINARY.
           10 NI-AZERF-ANSPR-VOM          PIC S9(04)       BINARY.
           10 NI-AZERF-ANSPR-SEIT         PIC S9(04)       BINARY.
           10 NI-AZERF-ANSPR-BIS          PIC S9(04)       BINARY.
           10 NI-AZERF-EBENE              PIC S9(04)       BINARY.
           10 NI-AZERF-VERT-FAKTOR        PIC S9(04)       BINARY.
           10 NI-AZERF-BETR-FAKTOR        PIC S9(04)       BINARY.
           10 NI-AZERF-ANZAHL-MON         PIC S9(04)       BINARY.
           10 NI-AZERF-BETRAG             PIC S9(04)       BINARY.
           10 NI-AZERF-EH-GRENZE          PIC S9(04)       BINARY.
           10 NI-AZERF-EH-EIGEN           PIC S9(04)       BINARY.
           10 NI-AZERF-EH-EIGEN-MON       PIC S9(04)       BINARY.
           10 NI-AZERF-EQF-EIGEN          PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-AZERF-LAST-UPDATE        PIC S9(04)       BINARY.
 
        05 DCL-VIVB0220.
           10 AZERF-PART-VBNR             PIC S9(09)       BINARY.
           10 AZERF-BETR-VBNR             PIC S9(09)       BINARY.
           10 DIR-NR                      PIC S9(04)       BINARY.
           10 MS-NR                       PIC S9(04)       BINARY.
           10 AZERF-ANSPR-VOM             PIC  X(10).
           10 AZERF-ANSPR-SEIT            PIC  X(10).
           10 AZERF-ANSPR-BIS             PIC  X(10).
           10 AZERF-EBENE                 PIC S9(04)       BINARY.
           10 AZERF-VERT-FAKTOR           PIC S9(04)       BINARY.
           10 AZERF-BETR-FAKTOR           PIC S9(09)V9(02) COMP-3.
           10 AZERF-ANZAHL-MON            PIC S9(04)       BINARY.
           10 AZERF-BETRAG                PIC S9(09)V9(02) COMP-3.
           10 AZERF-EH-GRENZE             PIC S9(09)V9(01) COMP-3.
           10 AZERF-EH-EIGEN              PIC S9(09)V9(01) COMP-3.
           10 AZERF-EH-EIGEN-MON          PIC S9(09)V9(01) COMP-3.
           10 AZERF-EQF-EIGEN             PIC S9(03)V9(02) COMP-3.
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 AZERF-LAST-UPDATE           PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  19                    *
      *****************************************************************
 

