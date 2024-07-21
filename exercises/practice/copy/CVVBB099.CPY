      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.04.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBB099.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBB099.CPv  $
      *
      *   Rev 2.1   May 09 2011 10:01:04   t13ex43
      *AVB01168: PGM - Pflege Orga-Betreuer (VB47)
      *
      *   Rev 2.0   Apr 28 2011 14:56:42   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    02.05.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01168!02.05.11!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBB099) NAMZ                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBB099)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBB099 TABLE
CCC004*    ( KTY_NAMZ_TYP                SMALLINT NOT NULL
CCC004*    , KNR_NAMZ_TYP                SMALLINT NOT NULL
CCC004*    , NAMZ_LFD                    SMALLINT NOT NULL
CCC004*    , KTY_ANREDE                  SMALLINT NOT NULL
CCC004*    , KNR_ANREDE                  SMALLINT NOT NULL
CCC004*    , NAMZ_TITEL_AKAD             CHAR(10)
CCC004*    , NAMZ_NAME                   CHAR(40) NOT NULL
CCC004*    , NAMZ_VORNAME                CHAR(20) NOT NULL
CCC004*    , KTY_NAMZ_FUNK               SMALLINT NOT NULL
CCC004*    , KNR_NAMZ_FUNK               SMALLINT NOT NULL
CCC004*    , NAMZ_DAT_VON                TIMESTAMP NOT NULL
CCC004*    , NAMZ_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , NAMZ_DAT_BIS                DATE NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBB099                           *
      *****************************************************************

       01  VIVBB099-GESAMT.
        05 NI-VIVBB099-TAB.
           10 NI-VIVBB099-ELE      PIC S9(04)  BINARY OCCURS  13.
        05 NI-VIVBB099             REDEFINES   NI-VIVBB099-TAB.
           10 NI-KTY-NAMZ-TYP             PIC S9(04)       BINARY.
           10 NI-KNR-NAMZ-TYP             PIC S9(04)       BINARY.
           10 NI-NAMZ-LFD                 PIC S9(04)       BINARY.
           10 NI-KTY-ANREDE               PIC S9(04)       BINARY.
           10 NI-KNR-ANREDE               PIC S9(04)       BINARY.
           10 NI-NAMZ-TITEL-AKAD          PIC S9(04)       BINARY.
           10 NI-NAMZ-NAME                PIC S9(04)       BINARY.
           10 NI-NAMZ-VORNAME             PIC S9(04)       BINARY.
           10 NI-KTY-NAMZ-FUNK            PIC S9(04)       BINARY.
           10 NI-KNR-NAMZ-FUNK            PIC S9(04)       BINARY.
           10 NI-NAMZ-DAT-VON             PIC S9(04)       BINARY.
           10 NI-NAMZ-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-NAMZ-DAT-BIS             PIC S9(04)       BINARY.

        05 DCL-VIVBB099.
           10 KTY-NAMZ-TYP                PIC S9(04)       BINARY.
           10 KNR-NAMZ-TYP                PIC S9(04)       BINARY.
           10 NAMZ-LFD                    PIC S9(04)       BINARY.
           10 KTY-ANREDE                  PIC S9(04)       BINARY.
           10 KNR-ANREDE                  PIC S9(04)       BINARY.
           10 NAMZ-TITEL-AKAD             PIC  X(10).
           10 NAMZ-NAME                   PIC  X(40).
           10 NAMZ-VORNAME                PIC  X(20).
           10 KTY-NAMZ-FUNK               PIC S9(04)       BINARY.
           10 KNR-NAMZ-FUNK               PIC S9(04)       BINARY.
           10 NAMZ-DAT-VON                PIC  X(26).
           10 NAMZ-LAST-UPDATE            PIC  X(26).
           10 NAMZ-DAT-BIS                PIC  X(10).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  13                    *
      *****************************************************************
