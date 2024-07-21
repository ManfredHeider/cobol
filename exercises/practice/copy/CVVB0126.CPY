      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0126.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CVVB0126.CPv  $
      *
      *   Rev 2.4   Dec 03 2018 10:57:28   t13ex18
      *AVB01456: Nachfolgeregelungen Zugriffsmodul Tabellen TBVB0126
      *
      *   Rev 2.2   Aug 16 2018 12:52:40   t13ex18
      *AVB01456: Nachfolgeregelungen Tabellen NRB, NRP, NRPF
      *
      *   Rev 2.1   02 Aug 2018 16:15:32   t13ae13
      *AVB01456: Nachfolgeregelungen Tabellen NRB, NRP, NRPF
      *
      *   Rev 2.0   Dec 01 2017 12:58:50   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: WALB                                             *
      *    DATUM:    04.12.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01456!04.12.17!2.1 ! NEU                       ! WALB      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0126) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0126)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0126 TABLE
CCC004*    ( NRB_ABGEB_NR                INTEGER NOT NULL
CCC004*    , NRB_AUFNEHM_NR              INTEGER NOT NULL
CCC004*    , KTY_NR_TYP                  SMALLINT NOT NULL
CCC004*    , KNR_NR_TYP                  SMALLINT NOT NULL
CCC004*    , NRB_PROJ_NR                 CHAR(10)
CCC004*    , NRB_DAT_VON                 DATE NOT NULL
CCC004*    , NRB_DAT_BIS                 DATE NOT NULL
CCC004*    , NRB_LAST_UPDATE             TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0126                           *
      *****************************************************************
 
       01  VIVB0126-GESAMT.
        05 NI-VIVB0126-TAB.
           10 NI-VIVB0126-ELE      PIC S9(04)  BINARY OCCURS   8.
        05 NI-VIVB0126             REDEFINES   NI-VIVB0126-TAB.
           10 NI-NRB-ABGEB-NR             PIC S9(04)       BINARY.
           10 NI-NRB-AUFNEHM-NR           PIC S9(04)       BINARY.
           10 NI-KTY-NR-TYP               PIC S9(04)       BINARY.
           10 NI-KNR-NR-TYP               PIC S9(04)       BINARY.
           10 NI-NRB-PROJ-NR              PIC S9(04)       BINARY.
           10 NI-NRB-DAT-VON              PIC S9(04)       BINARY.
           10 NI-NRB-DAT-BIS              PIC S9(04)       BINARY.
           10 NI-NRB-LAST-UPDATE          PIC S9(04)       BINARY.
 
        05 DCL-VIVB0126.
           10 NRB-ABGEB-NR                PIC S9(09)       BINARY.
           10 NRB-AUFNEHM-NR              PIC S9(09)       BINARY.
           10 KTY-NR-TYP                  PIC S9(04)       BINARY.
           10 KNR-NR-TYP                  PIC S9(04)       BINARY.
           10 NRB-PROJ-NR                 PIC  X(10).
           10 NRB-DAT-VON                 PIC  X(10).
           10 NRB-DAT-BIS                 PIC  X(10).
           10 NRB-LAST-UPDATE             PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************
 
 
 

