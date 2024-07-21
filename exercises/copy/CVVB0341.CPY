      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0341.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0341.CPv  $
      *
      *   Rev 2.1   Jun 17 2020 09:42:04   t13ex71
      *ARW00147: Offensive 2020 - Jetzt erst recht: Neukunden/aktiv
      *ierte Einvertragskunden - Neue Sonderprov. (VB-System)(+) - 
      *copy und PI/PO-Bereich aus GENERCBL (BPR)
      *
      *   Rev 2.0   Jun 09 2020 12:43:40   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    09.06.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * VLA-712 !09.06.20!2.1 ! NEU                       ! B.PRETSCH *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0341) OFAUS                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0341)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0341 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , OFAUS_ANSPR_VOM             DATE NOT NULL
CCC004*    , KNR_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KTY_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , OFAUS_LAST_UPDATE           TIMESTAMP NOT NULL
CCC004*    , OFAUS_ANSPR_BETRAG          DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_ANSPR_STATUS            SMALLINT
CCC004*    , KNR_ANSPR_STATUS            SMALLINT
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0341                          *
      *****************************************************************
 
       01  VIVB0341-GESAMT.
        05 NI-VIVB0341-TAB.
           10 NI-VIVB0341-ELE      PIC S9(04)  BINARY OCCURS   8.
        05 NI-VIVB0341             REDEFINES   NI-VIVB0341-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-OFAUS-ANSPR-VOM          PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-OFAUS-LAST-UPDATE        PIC S9(04)       BINARY.
           10 NI-OFAUS-ANSPR-BETRAG       PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
 
        05 DCL-VIVB0341.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 OFAUS-ANSPR-VOM             PIC  X(10).
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 OFAUS-LAST-UPDATE           PIC  X(26).
           10 OFAUS-ANSPR-BETRAG          PIC S9(09)V9(02) COMP-3.
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************
 
