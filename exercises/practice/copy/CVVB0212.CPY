      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0212.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0212.CPv  $
      *
      *   Rev 2.1   Dec 12 2011 14:26:42   t13ex13
      *ASL00399: MAP für FVD
      *
      *   Rev 2.0   Nov 14 2011 16:08:38   t13ex13
      *Archiverstellung
      *
      *****************************************************************
      *    ERSTELLT: K.OBER                                           *
      *    DATUM:    12.08.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00399!11.11.11!2.1 ! NEU                       !GUTFLEISCH *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0212) MAPFVD                                 *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0212)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0212 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL
CCC004*    , MAPFVD_REF_VB                  INTEGER NOT NULL
CCC004*    , MAPFVD_TURNUS                  INTEGER NOT NULL
CCC004*    , MAPFVD_LFDNR                   SMALLINT NOT NULL
CCC004*    , MAPFVD_REF_KZ                  CHAR(1) NOT NULL
CCC004*    , MAPFVD_BETRAG                  DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_WAEHRUNG                   SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                   SMALLINT NOT NULL
CCC004*    , KNR_ANSPR_GRUND                SMALLINT NOT NULL
CCC004*    , KTY_ANSPR_GRUND                SMALLINT NOT NULL
CCC004*    , MAPFVD_EH                      DECIMAL(10, 1) NOT NULL
CCC004*    , KTY_ANSPR_STATUS               SMALLINT NOT NULL
CCC004*    , KNR_ANSPR_STATUS               SMALLINT NOT NULL
CCC004*    , MAPFVD_LAST_UPD                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0212                           *
      *****************************************************************

       01  VIVB0212-GESAMT.
        05 NI-VIVB0212-TAB.
           10 NI-VIVB0212-ELE      PIC S9(04)  BINARY OCCURS  14.
        05 NI-VIVB0212             REDEFINES   NI-VIVB0212-TAB.
           10 NI-MA-ST-NR           PIC S9(04)       BINARY.
           10 NI-MAPFVD-REF-VB      PIC S9(04)       BINARY.
           10 NI-MAPFVD-TURNUS      PIC S9(04)       BINARY.
           10 NI-MAPFVD-LFDNR       PIC S9(04)       BINARY.
           10 NI-MAPFVD-REF-KZ      PIC S9(04)       BINARY.
           10 NI-MAPFVD-BETRAG      PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG       PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG       PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
           10 NI-MAPFVD-EH          PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
           10 NI-MAPFVD-LAST-UPD    PIC S9(04)       BINARY.
        05 DCL-VIVB0212.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 MAPFVD-REF-VB        PIC S9(9) USAGE COMP.
           10 MAPFVD-TURNUS        PIC S9(9) USAGE COMP.
           10 MAPFVD-LFDNR         PIC S9(4) USAGE COMP.
           10 MAPFVD-REF-KZ        PIC X(1).
           10 MAPFVD-BETRAG        PIC S9(9)V9(2) USAGE COMP-3.
           10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KNR-ANSPR-GRUND      PIC S9(4) USAGE COMP.
           10 KTY-ANSPR-GRUND      PIC S9(4) USAGE COMP.
           10 MAPFVD-EH            PIC S9(9)V9(1) USAGE COMP-3.
           10 KTY-ANSPR-STATUS     PIC S9(4) USAGE COMP.
           10 KNR-ANSPR-STATUS     PIC S9(4) USAGE COMP.
           10 MAPFVD-LAST-UPD      PIC X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  14                    *
      *****************************************************************

      *---------------------------------------------------------------*
