      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.04.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0131.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0131.CPv  $
      *
      *   Rev 2.1   Dec 03 2018 13:57:26   t13ex18
      *AVB01456: Nachfolgeregelungen Zugriffsmodul für TBVB0131 (GBU)
      *
      *   Rev 2.0   Apr 13 2018 12:07:44   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: G. BRUDER                                        *
      *    DATUM:    13.04.2018                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01456!13.04.18!2.1 ! NEU                       ! G. BRUDER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0131) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0131)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0131 TABLE
CCC004*    ( NRDF_ABGEB_NR               INTEGER NOT NULL
CCC004*    , NRDF_AUFNEHM_NR             INTEGER NOT NULL
CCC004*    , KTY_NR_TYP                  SMALLINT NOT NULL
CCC004*    , KNR_NR_TYP                  SMALLINT NOT NULL
CCC004*    , NRDF_DAT_VON                DATE NOT NULL
CCC004*    , NRDF_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , NRDF_UEBG_ZTPKT             DATE NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0131                           *
      *****************************************************************
 
       01  VIVB0131-GESAMT.
        05 NI-VIVB0131-TAB.
           10 NI-VIVB0131-ELE      PIC S9(04)  BINARY OCCURS   7.
        05 NI-VIVB0131             REDEFINES   NI-VIVB0131-TAB.
           10 NI-NRDF-ABGEB-NR            PIC S9(04)       BINARY.
           10 NI-NRDF-AUFNEHM-NR          PIC S9(04)       BINARY.
           10 NI-KTY-NR-TYP               PIC S9(04)       BINARY.
           10 NI-KNR-NR-TYP               PIC S9(04)       BINARY.
           10 NI-NRDF-DAT-VON             PIC S9(04)       BINARY.
           10 NI-NRDF-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-NRDF-UEBG-ZTPKT          PIC S9(04)       BINARY.
 
        05 DCL-VIVB0131.
           10 NRDF-ABGEB-NR               PIC S9(09)       BINARY.
           10 NRDF-AUFNEHM-NR             PIC S9(09)       BINARY.
           10 KTY-NR-TYP                  PIC S9(04)       BINARY.
           10 KNR-NR-TYP                  PIC S9(04)       BINARY.
           10 NRDF-DAT-VON                PIC  X(10).
           10 NRDF-LAST-UPDATE            PIC  X(26).
           10 NRDF-UEBG-ZTPKT             PIC  X(10).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   7                    *
      *****************************************************************

