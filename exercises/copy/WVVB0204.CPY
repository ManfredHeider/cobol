      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.07.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0204.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0204.CPv  $
      *
      *   Rev 2.1   Jul 15 2010 14:43:18   t13ae14
      *ASL00352: BOZ DVAG Schreibmodule
      *
      *   Rev 2.0   Jul 15 2010 14:38:36   t13ae14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0204                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0204 (BOZNERF )                        *
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
      * COBOL DEKLARATION FÜR VIEW VIVB0204                           *
      *****************************************************************

        05 WB-NI-VIVB0204-TAB.
           10 WB-NI-VIVB0204-ELE   PIC S9(04)  BINARY OCCURS   5.
        05 WB-NI-VIVB0204          REDEFINES   WB-NI-VIVB0204-TAB.
           10 WB-NI-ST-NR                 PIC S9(04)       BINARY.
           10 WB-NI-STICHTAG              PIC S9(04)       BINARY.
           10 WB-NI-BOZNERF-LAST-UPD      PIC S9(04)       BINARY.
           10 WB-NI-KNR-NERF-GRUND        PIC S9(04)       BINARY.
           10 WB-NI-KTY-NERF-GRUND        PIC S9(04)       BINARY.

        05 WB-VIVB0204.
           10 WB-ST-NR                    PIC S9(09)       BINARY.
           10 WB-STICHTAG                 PIC  X(10).
           10 WB-BOZNERF-LAST-UPD         PIC  X(26).
           10 WB-KNR-NERF-GRUND           PIC S9(04)       BINARY.
           10 WB-KTY-NERF-GRUND           PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   5                    *
      *****************************************************************
