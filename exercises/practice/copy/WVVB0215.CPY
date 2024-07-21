      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.12.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0215.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/WVVB0215.CPv  $
      *
      *   Rev 2.6   Feb 01 2012 15:43:16   t13ae14
      *ASL00420: Partnerbonus - DLGEN, Lese OZP, Schreibmodule
      *
      *   Rev 2.5   Jan 16 2012 14:53:46   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.4   Jan 16 2012 14:40:26   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.3   Jan 16 2012 14:31:40   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.2   Jan 16 2012 14:16:58   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.1   Jan 16 2012 14:07:24   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.0   Dec 19 2011 11:21:02   t13ae14
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: WVVB0215                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0215 (PABONERF)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: K.OBER                                           *
      *    DATUM:    19.12.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00420!19.12.11!2.1 ! NEU                       ! K.OBER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0215                           *
      *****************************************************************

        05 WB-NI-VIVB0215-TAB.
           10 WB-NI-VIVB0215-ELE   PIC S9(04)  BINARY OCCURS   9.
        05 WB-NI-VIVB0215          REDEFINES   WB-NI-VIVB0215-TAB.
           10 WB-NI-ST-NR                 PIC S9(04)       BINARY.
           10 WB-NI-PABONERF-UNTST        PIC S9(04)       BINARY.
           10 WB-NI-STICHTAG              PIC S9(04)       BINARY.
           10 WB-NI-PABON-LAST-UPD        PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-PABON-ANSPR-BETR      PIC S9(04)       BINARY.
           10 WB-NI-KNR-NERF-GRUND        PIC S9(04)       BINARY.
           10 WB-NI-KTY-NERF-GRUND        PIC S9(04)       BINARY.

        05 WB-VIVB0215.
           10 WB-ST-NR                    PIC S9(09)       BINARY.
           10 WB-PABONERF-UNTST           PIC S9(09)       BINARY.
           10 WB-STICHTAG                 PIC  X(10).
           10 WB-PABON-LAST-UPD           PIC  X(26).
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-PABON-ANSPR-BETR         PIC S9(09)V9(02) COMP-3.
           10 WB-KNR-NERF-GRUND           PIC S9(04)       BINARY.
           10 WB-KTY-NERF-GRUND           PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   9                    *
      *****************************************************************
