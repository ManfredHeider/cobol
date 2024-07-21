      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.04.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0226.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0226.CPv  $
      *
      *   Rev 2.1   May 02 2017 15:52:02   t13ex18
      *ASL00569: Förderprovision Zugriffsmodul für TBVB0226
      *
      *   Rev 2.0   Apr 28 2017 17:25:44   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: G. BRUDER                                        *
      *    DATUM:    02.05.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00569!02.05.17!2.1 ! NEU                       ! G. BRUDER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0226) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0226)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0226 TABLE
CCC004*    ( FPRNERF_ST_NR               INTEGER NOT NULL
CCC004*    , FPRNERF_STICHTAG            DATE NOT NULL
CCC004*    , FPRNERF_LAST_UPD            TIMESTAMP NOT NULL
CCC004*    , KNR_NERF_GRUND              SMALLINT NOT NULL
CCC004*    , KTY_NERF_GRUND              SMALLINT NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0226                           *
      *****************************************************************
 
       01  VIVB0226-GESAMT.
        05 NI-VIVB0226-TAB.
           10 NI-VIVB0226-ELE      PIC S9(04)  BINARY OCCURS   5.
        05 NI-VIVB0226             REDEFINES   NI-VIVB0226-TAB.
           10 NI-FPRNERF-ST-NR            PIC S9(04)       BINARY.
           10 NI-FPRNERF-STICHTAG         PIC S9(04)       BINARY.
           10 NI-FPRNERF-LAST-UPD         PIC S9(04)       BINARY.
           10 NI-KNR-NERF-GRUND           PIC S9(04)       BINARY.
           10 NI-KTY-NERF-GRUND           PIC S9(04)       BINARY.
 
        05 DCL-VIVB0226.
           10 FPRNERF-ST-NR               PIC S9(09)       BINARY.
           10 FPRNERF-STICHTAG            PIC  X(10).
           10 FPRNERF-LAST-UPD            PIC  X(26).
           10 KNR-NERF-GRUND              PIC S9(04)       BINARY.
           10 KTY-NERF-GRUND              PIC S9(04)       BINARY.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   5                    *
      *****************************************************************

