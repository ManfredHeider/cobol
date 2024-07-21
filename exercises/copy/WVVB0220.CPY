      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.04.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0220.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0220.CPv  $
      *
      *   Rev 2.1   Feb 17 2016 09:40:52   t13ae14
      *ASL00530: SL12 - Dialog Ausbilder-Zuschuss
      *
      *   Rev 2.0   Apr 01 2015 14:38:50   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0220                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0220 (AZERF   )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GUTFLEISCH                                       *
      *    DATUM:    11.08.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00530!11.08.15!2.1 ! NEU                       ! GUTFLEISCH*
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0220                           *
      *****************************************************************
 
        05 WB-NI-VIVB0220-TAB.
           10 WB-NI-VIVB0220-ELE   PIC S9(04)  BINARY OCCURS  19.
        05 WB-NI-VIVB0220          REDEFINES   WB-NI-VIVB0220-TAB.
           10 WB-NI-AZERF-PART-VBNR       PIC S9(04)       BINARY.
           10 WB-NI-AZERF-BETR-VBNR       PIC S9(04)       BINARY.
           10 WB-NI-DIR-NR                PIC S9(04)       BINARY.
           10 WB-NI-MS-NR                 PIC S9(04)       BINARY.
           10 WB-NI-AZERF-ANSPR-VOM       PIC S9(04)       BINARY.
           10 WB-NI-AZERF-ANSPR-SEIT      PIC S9(04)       BINARY.
           10 WB-NI-AZERF-ANSPR-BIS       PIC S9(04)       BINARY.
           10 WB-NI-AZERF-EBENE           PIC S9(04)       BINARY.
           10 WB-NI-AZERF-VERT-FAKTOR     PIC S9(04)       BINARY.
           10 WB-NI-AZERF-BETR-FAKTOR     PIC S9(04)       BINARY.
           10 WB-NI-AZERF-ANZAHL-MON      PIC S9(04)       BINARY.
           10 WB-NI-AZERF-BETRAG          PIC S9(04)       BINARY.
           10 WB-NI-AZERF-EH-GRENZE       PIC S9(04)       BINARY.
           10 WB-NI-AZERF-EH-EIGEN        PIC S9(04)       BINARY.
           10 WB-NI-AZERF-EH-EIGEN-MON    PIC S9(04)       BINARY.
           10 WB-NI-AZERF-EQF-EIGEN       PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-AZERF-LAST-UPDATE     PIC S9(04)       BINARY.
 
        05 WB-VIVB0220.
           10 WB-AZERF-PART-VBNR          PIC S9(09)       BINARY.
           10 WB-AZERF-BETR-VBNR          PIC S9(09)       BINARY.
           10 WB-DIR-NR                   PIC S9(04)       BINARY.
           10 WB-MS-NR                    PIC S9(04)       BINARY.
           10 WB-AZERF-ANSPR-VOM          PIC  X(10).
           10 WB-AZERF-ANSPR-SEIT         PIC  X(10).
           10 WB-AZERF-ANSPR-BIS          PIC  X(10).
           10 WB-AZERF-EBENE              PIC S9(04)       BINARY.
           10 WB-AZERF-VERT-FAKTOR        PIC S9(04)       BINARY.
           10 WB-AZERF-BETR-FAKTOR        PIC S9(09)V9(02) COMP-3.
           10 WB-AZERF-ANZAHL-MON         PIC S9(04)       BINARY.
           10 WB-AZERF-BETRAG             PIC S9(09)V9(02) COMP-3.
           10 WB-AZERF-EH-GRENZE          PIC S9(09)V9(01) COMP-3.
           10 WB-AZERF-EH-EIGEN           PIC S9(09)V9(01) COMP-3.
           10 WB-AZERF-EH-EIGEN-MON       PIC S9(09)V9(01) COMP-3.
           10 WB-AZERF-EQF-EIGEN          PIC S9(03)V9(02) COMP-3.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-AZERF-LAST-UPDATE        PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  19                    *
      *****************************************************************
 

