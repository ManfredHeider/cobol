      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  26.03.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBB006.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBB006.CPv  $
      *
      *   Rev 2.1   Mar 27 2020 12:15:42   t13ex18
      *AVB01617: Neue Regionaliserte Direktionsbereiche für Allfina
      *nz 1 und 2 / Anpassung VB22 (VB-System)(+)
      *
      *   Rev 2.0   Mar 26 2020 17:13:30   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: G. BRUDER                                        *
      *    DATUM:    26.03.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01617!26.03.20!2.1 ! NEU                       ! G. BRUDER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBB006) DIR                                    *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBB006)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVBB006 TABLE
CCC004*    ( DIR_NR                      SMALLINT NOT NULL
CCC004*    , DIR_LAST_UPDATE             TIMESTAMP NOT NULL
CCC004*    , KTY_DIR_TYP                 SMALLINT NOT NULL
CCC004*    , KNR_DIR_TYP                 SMALLINT NOT NULL
CCC004*    , KTY_REGION                  SMALLINT NOT NULL
CCC004*    , KNR_REGION                  SMALLINT NOT NULL
CCC004*    , KTY_RESERV_TYP              SMALLINT NOT NULL
CCC004*    , KNR_RESERV_TYP              SMALLINT NOT NULL
CCC004*    , MA_ST_NR                    INTEGER NOT NULL
CCC004*    , DIR_NAME                    CHAR(40) NOT NULL
CCC004*    , DIR_INFORMATION             CHAR(150) NOT NULL
CCC004*    , KTY_VERTRBER                SMALLINT NOT NULL
CCC004*    , KNR_VERTRBER                SMALLINT NOT NULL
CCC004*    , KTY_REGDB                   SMALLINT NOT NULL
CCC004*    , KNR_REGDB                   SMALLINT NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVBB006                          *
      *****************************************************************
 
       01  VIVBB006-GESAMT.
        05 NI-VIVBB006-TAB.
           10 NI-VIVBB006-ELE      PIC S9(04)  BINARY OCCURS  15.
        05 NI-VIVBB006             REDEFINES   NI-VIVBB006-TAB.
           10 NI-DIR-NR                   PIC S9(04)       BINARY.
           10 NI-DIR-LAST-UPDATE          PIC S9(04)       BINARY.
           10 NI-KTY-DIR-TYP              PIC S9(04)       BINARY.
           10 NI-KNR-DIR-TYP              PIC S9(04)       BINARY.
           10 NI-KTY-REGION               PIC S9(04)       BINARY.
           10 NI-KNR-REGION               PIC S9(04)       BINARY.
           10 NI-KTY-RESERV-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-RESERV-TYP           PIC S9(04)       BINARY.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-DIR-NAME                 PIC S9(04)       BINARY.
           10 NI-DIR-INFORMATION          PIC S9(04)       BINARY.
           10 NI-KTY-VERTRBER             PIC S9(04)       BINARY.
           10 NI-KNR-VERTRBER             PIC S9(04)       BINARY.
           10 NI-KTY-REGDB                PIC S9(04)       BINARY.
           10 NI-KNR-REGDB                PIC S9(04)       BINARY.
 
        05 DCL-VIVBB006.
           10 DIR-NR                      PIC S9(04)       BINARY.
           10 DIR-LAST-UPDATE             PIC  X(26).
           10 KTY-DIR-TYP                 PIC S9(04)       BINARY.
           10 KNR-DIR-TYP                 PIC S9(04)       BINARY.
           10 KTY-REGION                  PIC S9(04)       BINARY.
           10 KNR-REGION                  PIC S9(04)       BINARY.
           10 KTY-RESERV-TYP              PIC S9(04)       BINARY.
           10 KNR-RESERV-TYP              PIC S9(04)       BINARY.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 DIR-NAME                    PIC  X(40).
           10 DIR-INFORMATION             PIC  X(150).
           10 KTY-VERTRBER                PIC S9(04)       BINARY.
           10 KNR-VERTRBER                PIC S9(04)       BINARY.
           10 KTY-REGDB                   PIC S9(04)       BINARY.
           10 KNR-REGDB                   PIC S9(04)       BINARY.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  15                    *
      *****************************************************************

