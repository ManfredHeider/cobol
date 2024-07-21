      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.09.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0138.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0138.CPv  $
      *
      *   Rev 2.1   Dec 21 2020 17:35:14   t13ae30
      *AVB01642: Anpassung PMVD0010
      *
      *   Rev 2.0   Sep 09 2020 13:24:04   T13AE30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: T13AE30                                          *
      *    DATUM:    09.09.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * VLA-989 !09.09.20!2.1 ! NEU                       ! T13AE30   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0138) PEHDAI                                 *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0138)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0138 TABLE
CCC004*    ( PEHDAI_VS_ZEITRAUM          SMALLINT NOT NULL
CCC004*    , PEHDAI_BETR                 INTEGER NOT NULL
CCC004*    , PEHDAI_UNTST                INTEGER NOT NULL
CCC004*    , PEHDAI_DAT_VON              DATE NOT NULL
CCC004*    , PEHDAI_DAT_BIS              DATE NOT NULL
CCC004*    , PEHDAI_ERSTELL_DAT          DATE NOT NULL
CCC004*    , PEHDAI_EH_DAISY             DECIMAL(10, 1) NOT NULL
CCC004*    , PEHDAI_UNTST_WERT           CHAR(01) NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0138                          *
      *****************************************************************
 
       01  VIVB0138-GESAMT.
        05 NI-VIVB0138-TAB.
           10 NI-VIVB0138-ELE      PIC S9(04)  BINARY OCCURS   8.
        05 NI-VIVB0138             REDEFINES   NI-VIVB0138-TAB.
           10 NI-PEHDAI-VS-ZEITRAUM       PIC S9(04)       BINARY.
           10 NI-PEHDAI-BETR              PIC S9(04)       BINARY.
           10 NI-PEHDAI-UNTST             PIC S9(04)       BINARY.
           10 NI-PEHDAI-DAT-VON           PIC S9(04)       BINARY.
           10 NI-PEHDAI-DAT-BIS           PIC S9(04)       BINARY.
           10 NI-PEHDAI-ERSTELL-DAT       PIC S9(04)       BINARY.
           10 NI-PEHDAI-EH-DAISY          PIC S9(04)       BINARY.
           10 NI-PEHDAI-UNTST-WERT        PIC S9(04)       BINARY.
 
        05 DCL-VIVB0138.
           10 PEHDAI-VS-ZEITRAUM          PIC S9(04)       BINARY.
           10 PEHDAI-BETR                 PIC S9(09)       BINARY.
           10 PEHDAI-UNTST                PIC S9(09)       BINARY.
           10 PEHDAI-DAT-VON              PIC  X(10).
           10 PEHDAI-DAT-BIS              PIC  X(10).
           10 PEHDAI-ERSTELL-DAT          PIC  X(10).
           10 PEHDAI-EH-DAISY             PIC S9(09)V9(01) COMP-3.
           10 PEHDAI-UNTST-WERT           PIC  X(01).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************

