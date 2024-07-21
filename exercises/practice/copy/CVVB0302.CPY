      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0302.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVVB0302.CPv  $
      *
      *   Rev 2.2   Mar 26 2009 14:14:02   t13ex43
      *AAS00812: PGM - D-Chancenprofil
      *
      *   Rev 2.1   Mar 26 2009 09:21:16   t13ex43
      *AAS00812: PGM - D-Chancenprofil
      *
      *   Rev 2.0   Mar 25 2009 08:43:58   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    25.03.2009                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00812!25.03.09!2.1 ! NEU                       ! KLEEMEIER *
260309* AAS00812!26.03.09!2.2 ! KORRERKTUR FELDNAMEN      ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0302) DETAILNP                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0302)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVB0302 TABLE
CCC004*    ( DETAILNP_BETR               INTEGER NOT NULL,
CCC004*      DETAILNP_MONAT              DATE NOT NULL,
CCC004*      DETAILNP_UNTST              INTEGER NOT NULL,
CCC004*      KNR_WB_TYP                  SMALLINT NOT NULL,
CCC004*      KNR_WERTUNGS_TYP            SMALLINT NOT NULL,
CCC004*      KTY_WB_TYP                  SMALLINT NOT NULL,
CCC004*      KTY_WERTUNGS_TYP            SMALLINT NOT NULL,
260309****   DETAILNP_LAST_UPD           TIMESTAMP NOT NULL,
CCC004*      DETAILNP_LAST_UP            TIMESTAMP NOT NULL,
CCC004*      DETAILNP_EH                 DECIMAL(10, 1) NOT NULL,
CCC004*      DETAILNP_EBENE              CHAR(01) NOT NULL,
CCC004*      DETAILNP_BIS                DATE,
260309****   PARTEH_BETR_TYP             CHAR(01)
CCC004*      DETAILNP_BTR_TYP            CHAR(01)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0302
      ******************************************************************
       01  VIVB0302-GESAMT.
        05  NI-VIVB0302-TAB.
           10 NI-VIVB0302-ELE      PIC S9(04) BINARY OCCURS 12 TIMES.
        05  NI-VIVB0302            REDEFINES   NI-VIVB0302-TAB.
           10 NI-DETAILNP-BETR            PIC S9(04)       BINARY.
           10 NI-DETAILNP-MONAT           PIC S9(04)       BINARY.
           10 NI-DETAILNP-UNTST           PIC S9(04)       BINARY.
           10 NI-KNR-WB-TYP               PIC S9(04)       BINARY.
           10 NI-KNR-WERTUNGS-TYP         PIC S9(04)       BINARY.
           10 NI-KTY-WB-TYP               PIC S9(04)       BINARY.
           10 NI-KTY-WERTUNGS-TYP         PIC S9(04)       BINARY.
260309**** 10 NI-DETAILNP-LAST-UPD        PIC S9(04)       BINARY.
260309     10 NI-DETAILNP-LAST-UP         PIC S9(04)       BINARY.
           10 NI-DETAILNP-EH              PIC S9(04)       BINARY.
           10 NI-DETAILNP-EBENE           PIC S9(04)       BINARY.
           10 NI-DETAILNP-BIS             PIC S9(04)       BINARY.
260309**** 10 NI-PARTEH-BETR-TYP          PIC S9(04)       BINARY.
260309     10 NI-DETAILNP-BTR-TYP         PIC S9(04)       BINARY.

        05  DCL-VIVB0302.
           10 DETAILNP-BETR               PIC S9(09)       BINARY.
           10 DETAILNP-MONAT              PIC  X(10).
           10 DETAILNP-UNTST              PIC S9(09)       BINARY.
           10 KNR-WB-TYP                  PIC S9(04)       BINARY.
           10 KNR-WERTUNGS-TYP            PIC S9(04)       BINARY.
           10 KTY-WB-TYP                  PIC S9(04)       BINARY.
           10 KTY-WERTUNGS-TYP            PIC S9(04)       BINARY.
260309**** 10 DETAILNP-LAST-UPD           PIC  X(26).
260309     10 DETAILNP-LAST-UP            PIC  X(26).
           10 DETAILNP-EH                 PIC S9(09)V9(01) COMP-3.
           10 DETAILNP-EBENE              PIC  X(01).
           10 DETAILNP-BIS                PIC  X(10).
260309**** 10 PARTEH-BETR-TYP             PIC  X(01).
260309     10 DETAILNP-BTR-TYP            PIC  X(01).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
