      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.01.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0208.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CVVB0208.CPv  $
      *
      *   Rev 2.4   Mar 01 2011 13:50:52   t13ae14
      *ASL00366: neuer BOZ-DVAG-Wien incl. 40%-Regel -> Schreibmodu
      *l, DCLGEN, Copy-Member
      *
      *   Rev 2.3   Feb 24 2011 13:18:42   t13ae14
      *ASL00366: neuer BOZ-DVAG-Wien incl. 40%-Regel -> Schreibmodu
      *l, DCLGEN, Copy-Member
      *
      *   Rev 2.2   Feb 16 2011 13:21:22   t13ae14
      *ASL00366: neuer BOZ-DVAG-Wien incl. 40%-Regel -> Schreibmodu
      *l, DCLGEN, Copy-Member
      *
      *   Rev 2.1   Jan 31 2011 11:00:48   t13ae14
      *ASL00366: neuer BOZ-DVAG-Wien incl. 40%-Regel -> Schreibmodule
      *
      *   Rev 2.0   Jan 31 2011 10:57:18   t13ae14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: K.OBER                                           *
      *    DATUM:    01.02.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00366!01.02.11!2.1 ! NEU                       ! K.OBER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0208) BOZM                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0208)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0208 TABLE
CCC004*    ( BETR_ST_NR                  INTEGER NOT NULL
CCC004*    , PART_ST_NR                  INTEGER NOT NULL
CCC004*    , STICHTAG                    DATE NOT NULL
CCC004*    , KNR_ABZUG                   SMALLINT NOT NULL
CCC004*    , KTY_ABZUG                   SMALLINT NOT NULL
CCC004*    , BOZM_ANSPR_VOM              DATE NOT NULL
CCC004*    , BETR_UMSATZ                 DECIMAL(10, 1) NOT NULL
CCC004*    , BETR_UMSATZ_MAX             DECIMAL(10, 1) NOT NULL
CCC004*    , PART_UMSATZ                 DECIMAL(10, 1) NOT NULL
CCC004*    , PART_ABZUG                  DECIMAL(10, 1) NOT NULL
CCC004*    , BOZM_LAST_UPDATE            TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0208                           *
      *****************************************************************

       01  VIVB0208-GESAMT.
        05 NI-VIVB0208-TAB.
           10 NI-VIVB0208-ELE      PIC S9(04)  BINARY OCCURS  11.
        05 NI-VIVB0208             REDEFINES   NI-VIVB0208-TAB.
           10 NI-BETR-ST-NR               PIC S9(04)       BINARY.
           10 NI-PART-ST-NR               PIC S9(04)       BINARY.
           10 NI-STICHTAG                 PIC S9(04)       BINARY.
           10 NI-KNR-ABZUG                PIC S9(04)       BINARY.
           10 NI-KTY-ABZUG                PIC S9(04)       BINARY.
           10 NI-BOZM-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-BETR-UMSATZ              PIC S9(04)       BINARY.
           10 NI-BETR-UMSATZ-MAX          PIC S9(04)       BINARY.
           10 NI-PART-UMSATZ              PIC S9(04)       BINARY.
           10 NI-PART-ABZUG               PIC S9(04)       BINARY.
           10 NI-BOZM-LAST-UPDATE         PIC S9(04)       BINARY.

        05 DCL-VIVB0208.
           10 BETR-ST-NR                  PIC S9(09)       BINARY.
           10 PART-ST-NR                  PIC S9(09)       BINARY.
           10 STICHTAG                    PIC  X(10).
           10 KNR-ABZUG                   PIC S9(04)       BINARY.
           10 KTY-ABZUG                   PIC S9(04)       BINARY.
           10 BOZM-ANSPR-VOM              PIC  X(10).
           10 BETR-UMSATZ                 PIC S9(09)V9(01) COMP-3.
           10 BETR-UMSATZ-MAX             PIC S9(09)V9(01) COMP-3.
           10 PART-UMSATZ                 PIC S9(09)V9(01) COMP-3.
           10 PART-ABZUG                  PIC S9(09)V9(01) COMP-3.
           10 BOZM-LAST-UPDATE            PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************

