      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.07.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0203.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0203.CPv  $
      *
      *   Rev 2.1   Jul 15 2010 14:11:24   t13ae14
      *ASL00352: BOZ DVAG Schreibmodule
      *
      *   Rev 2.0   Jul 15 2010 14:07:20   t13ae14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0203                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0203 (BOZBNEU )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: OBER                                             *
      *    DATUM:    15.07.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00352!15.07.10!2.1 ! NEU                       ! OBER      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0203                           *
      *****************************************************************

        05 WB-NI-VIVB0203-TAB.
           10 WB-NI-VIVB0203-ELE   PIC S9(04)  BINARY OCCURS  23.
        05 WB-NI-VIVB0203          REDEFINES   WB-NI-VIVB0203-TAB.
           10 WB-NI-ST-NR                 PIC S9(04)       BINARY.
           10 WB-NI-STICHTAG              PIC S9(04)       BINARY.
           10 WB-NI-ANSPR-MONAT           PIC S9(04)       BINARY.
           10 WB-NI-ANSPR-VOM             PIC S9(04)       BINARY.
           10 WB-NI-BOZBNEU-LAST-UPD      PIC S9(04)       BINARY.
           10 WB-NI-UMS-12-EIG            PIC S9(04)       BINARY.
           10 WB-NI-UMS-12-GRU            PIC S9(04)       BINARY.
           10 WB-NI-UMS-12-GES            PIC S9(04)       BINARY.
           10 WB-NI-EQF-12-GRU            PIC S9(04)       BINARY.
           10 WB-NI-UMS-03-GRU            PIC S9(04)       BINARY.
           10 WB-NI-EH-PRO-ABZUG          PIC S9(04)       BINARY.
           10 WB-NI-EH-NACH-PRO-ABZUG     PIC S9(04)       BINARY.
           10 WB-NI-BOZ-BASIS-BETRAG      PIC S9(04)       BINARY.
           10 WB-NI-UMS-03-PART           PIC S9(04)       BINARY.
           10 WB-NI-BOZ-AUFBAU-BETRAG     PIC S9(04)       BINARY.
           10 WB-NI-BOZ-ZW-SUMME          PIC S9(04)       BINARY.
           10 WB-NI-EQF-ABZUG             PIC S9(04)       BINARY.
           10 WB-NI-BOZ-BETRAG            PIC S9(04)       BINARY.
           10 WB-NI-BOZ-FAKTOR-BASIS      PIC S9(04)       BINARY.
           10 WB-NI-BOZ-FAKTOR-AUFBAU     PIC S9(04)       BINARY.
           10 WB-NI-BOZ-ANZ-MONATE        PIC S9(04)       BINARY.
           10 WB-NI-BOZ-NEU-AUSZAHLUNG    PIC S9(04)       BINARY.
           10 WB-NI-BOZ-NACH-DIR          PIC S9(04)       BINARY.

        05 WB-VIVB0203.
           10 WB-ST-NR                    PIC S9(09)       BINARY.
           10 WB-STICHTAG                 PIC  X(10).
           10 WB-ANSPR-MONAT              PIC  X(10).
           10 WB-ANSPR-VOM                PIC  X(10).
           10 WB-BOZBNEU-LAST-UPD         PIC  X(26).
           10 WB-UMS-12-EIG               PIC S9(09)V9(01) COMP-3.
           10 WB-UMS-12-GRU               PIC S9(09)V9(01) COMP-3.
           10 WB-UMS-12-GES               PIC S9(09)V9(01) COMP-3.
           10 WB-EQF-12-GRU               PIC S9(03)V9(02) COMP-3.
           10 WB-UMS-03-GRU               PIC S9(09)V9(01) COMP-3.
           10 WB-EH-PRO-ABZUG             PIC S9(09)V9(01) COMP-3.
           10 WB-EH-NACH-PRO-ABZUG        PIC S9(09)V9(01) COMP-3.
           10 WB-BOZ-BASIS-BETRAG         PIC S9(09)V9(02) COMP-3.
           10 WB-UMS-03-PART              PIC S9(09)V9(01) COMP-3.
           10 WB-BOZ-AUFBAU-BETRAG        PIC S9(09)V9(02) COMP-3.
           10 WB-BOZ-ZW-SUMME             PIC S9(09)V9(02) COMP-3.
           10 WB-EQF-ABZUG                PIC S9(09)V9(02) COMP-3.
           10 WB-BOZ-BETRAG               PIC S9(09)V9(02) COMP-3.
           10 WB-BOZ-FAKTOR-BASIS         PIC S9(03)V9(02) COMP-3.
           10 WB-BOZ-FAKTOR-AUFBAU        PIC S9(03)V9(02) COMP-3.
           10 WB-BOZ-ANZ-MONATE           PIC S9(04)       BINARY.
           10 WB-BOZ-NEU-AUSZAHLUNG       PIC  X(01).
           10 WB-BOZ-NACH-DIR             PIC  X(01).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  23                    *
      *****************************************************************
