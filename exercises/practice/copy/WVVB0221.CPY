      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.08.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0221.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0221.CPv  $
      *
      *   Rev 2.1   Feb 17 2016 09:40:58   t13ae14
      *ASL00530: SL12 - Dialog Ausbilder-Zuschuss
      *
      *   Rev 2.0   Aug 11 2015 13:00:48   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0221                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0221 (AZNERF  )                        *
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
      * COBOL DEKLARATION FÜR VIEW VIVB0221                           *
      *****************************************************************
 
        05 WB-NI-VIVB0221-TAB.
           10 WB-NI-VIVB0221-ELE   PIC S9(04)  BINARY OCCURS  21.
        05 WB-NI-VIVB0221          REDEFINES   WB-NI-VIVB0221-TAB.
           10 WB-NI-AZNERF-PART-VBNR      PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-BETR-VBNR      PIC S9(04)       BINARY.
           10 WB-NI-DIR-NR                PIC S9(04)       BINARY.
           10 WB-NI-MS-NR                 PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-ANSPR-VOM      PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-ANSPR-SEIT     PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-ANSPR-BIS      PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-EBENE          PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-VERT-FAKTOR    PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-BETR-FAKTOR    PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-ANZAHL-MON     PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-BETRAG         PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-EH-GRENZE      PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-EH-EIGEN       PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-EH-EIGEN-MO    PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-EQF-EIGEN      PIC S9(04)       BINARY.
           10 WB-NI-KNR-NERF-GRUND        PIC S9(04)       BINARY.
           10 WB-NI-KTY-NERF-GRUND        PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-LAST-UPDATE    PIC S9(04)       BINARY.
 
        05 WB-VIVB0221.
           10 WB-AZNERF-PART-VBNR         PIC S9(09)       BINARY.
           10 WB-AZNERF-BETR-VBNR         PIC S9(09)       BINARY.
           10 WB-DIR-NR                   PIC S9(04)       BINARY.
           10 WB-MS-NR                    PIC S9(04)       BINARY.
           10 WB-AZNERF-ANSPR-VOM         PIC  X(10).
           10 WB-AZNERF-ANSPR-SEIT        PIC  X(10).
           10 WB-AZNERF-ANSPR-BIS         PIC  X(10).
           10 WB-AZNERF-EBENE             PIC S9(04)       BINARY.
           10 WB-AZNERF-VERT-FAKTOR       PIC S9(04)       BINARY.
           10 WB-AZNERF-BETR-FAKTOR       PIC S9(09)V9(02) COMP-3.
           10 WB-AZNERF-ANZAHL-MON        PIC S9(04)       BINARY.
           10 WB-AZNERF-BETRAG            PIC S9(09)V9(02) COMP-3.
           10 WB-AZNERF-EH-GRENZE         PIC S9(09)V9(01) COMP-3.
           10 WB-AZNERF-EH-EIGEN          PIC S9(09)V9(01) COMP-3.
           10 WB-AZNERF-EH-EIGEN-MO       PIC S9(09)V9(01) COMP-3.
           10 WB-AZNERF-EQF-EIGEN         PIC S9(03)V9(02) COMP-3.
           10 WB-KNR-NERF-GRUND           PIC S9(04)       BINARY.
           10 WB-KTY-NERF-GRUND           PIC S9(04)       BINARY.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-AZNERF-LAST-UPDATE       PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  21                    *
      *****************************************************************
 

