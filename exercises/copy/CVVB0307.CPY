      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.06.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0307.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0307.CPv  $
      *
      *   Rev 2.1   Jun 22 2009 09:27:00   t13ex43
      *AAS00815: PGM2 - Neukundenwettbewerb
      *
      *   Rev 2.0   Jun 04 2009 11:11:36   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    12.06.2009                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AAS00815!12.06.09!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0307) NKLOS                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0307)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0307 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , NKLOS_DAT_VON               DATE NOT NULL
CCC004*    , NKLOS_LOSNR                 INTEGER NOT NULL
CCC004*    , NKLOS_LAST_UPDAT            TIMESTAMP NOT NULL
CCC004*    , NKLOS_KZ                    SMALLINT NOT NULL
CCC004*    , NKLOS_GUELTIG               CHAR(01) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0307                           *
      *****************************************************************

       01  VIVB0307-GESAMT.
        05 NI-VIVB0307-TAB.
           10 NI-VIVB0307-ELE      PIC S9(04)  BINARY OCCURS   6.
        05 NI-VIVB0307             REDEFINES   NI-VIVB0307-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-NKLOS-DAT-VON            PIC S9(04)       BINARY.
           10 NI-NKLOS-LOSNR              PIC S9(04)       BINARY.
           10 NI-NKLOS-LAST-UPDAT         PIC S9(04)       BINARY.
           10 NI-NKLOS-KZ                 PIC S9(04)       BINARY.
           10 NI-NKLOS-GUELTIG            PIC S9(04)       BINARY.

        05 DCL-VIVB0307.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 NKLOS-DAT-VON               PIC  X(10).
           10 NKLOS-LOSNR                 PIC S9(09)       BINARY.
           10 NKLOS-LAST-UPDAT            PIC  X(26).
           10 NKLOS-KZ                    PIC S9(04)       BINARY.
           10 NKLOS-GUELTIG               PIC  X(01).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   6                    *
      *****************************************************************
