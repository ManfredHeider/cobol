      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.07.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA222.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA222.CPv  $
      *
      *   Rev 2.1   26 Jul 2017 18:22:22   t13ex71
      *ASL00567: LUCY2 für Ausbilderbonus - AZA (BPR)
      *
      *   Rev 2.0   25 Jul 2017 19:08:52   t13ex71
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
      * ASL00567!26.07.17!2.1 ! NEU                       ! B.PRETSCH *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA222) AZA                                    *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA222)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVBA222 TABLE
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
CCC004*    , AZA_BETRAG_UPFL             DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA222                           *
      *****************************************************************
 
       01  VIVBA222-GESAMT.
        05 NI-VIVBA222-TAB.
           10 NI-VIVBA222-ELE      PIC S9(04)  BINARY OCCURS  11.
        05 NI-VIVBA222             REDEFINES   NI-VIVBA222-TAB.
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
           10 NI-AZA-BETRAG-UPFL          PIC S9(04)       BINARY.
 
        05 DCL-VIVBA222.
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
           10 AZA-BETRAG-UPFL             PIC S9(09)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************
 

