      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0222.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0222.CPv  $
      *
      *   Rev 2.1   Mar 02 2015 12:42:48   t13ex71
      *ASL00530: Änderung Sonderleistungen 2015 / Wachstums-Agenda 
      *Zugriffsmodule Tabelle AZA (TBVB0222)
      *
      *   Rev 2.0   Mar 02 2015 10:43:38   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    27.02.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00530!27.02.15!2.1 ! NEU                       ! B.PRETSCH *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0222) AZA                                    *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0222)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0222 TABLE
CCC004*    ( AZA_BETR_VBNR               INTEGER NOT NULL
CCC004*    , AZA_ANSPR_MONAT             DATE NOT NULL
CCC004*    , AZA_BETRAG                  DECIMAL(11, 2) NOT NULL
CCC004*    , KNR_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KTY_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KNR_ANSPR_STATUS            SMALLINT NOT NULL
CCC004*    , KTY_ANSPR_STATUS            SMALLINT
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT
CCC004*    , AZA_LAST_UPDATE             TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0222                           *
      *****************************************************************
 
       01  VIVB0222-GESAMT.
        05 NI-VIVB0222-TAB.
           10 NI-VIVB0222-ELE      PIC S9(04)  BINARY OCCURS  10.
        05 NI-VIVB0222             REDEFINES   NI-VIVB0222-TAB.
           10 NI-AZA-BETR-VBNR            PIC S9(04)       BINARY.
           10 NI-AZA-ANSPR-MONAT          PIC S9(04)       BINARY.
           10 NI-AZA-BETRAG               PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-AZA-LAST-UPDATE          PIC S9(04)       BINARY.
 
        05 DCL-VIVB0222.
           10 AZA-BETR-VBNR               PIC S9(09)       BINARY.
           10 AZA-ANSPR-MONAT             PIC  X(10).
           10 AZA-BETRAG                  PIC S9(09)V9(02) COMP-3.
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 AZA-LAST-UPDATE             PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  10                    *
      *****************************************************************

