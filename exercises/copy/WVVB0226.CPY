      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.04.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0226.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0226.CPv  $
      *
      *   Rev 2.1   May 02 2017 15:52:10   t13ex18
      *ASL00569: Förderprovision Zugriffsmodul für TBVB0226
      *
      *   Rev 2.0   Apr 28 2017 17:25:52   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0226                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0226 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G. BRUDER                                        *
      *    DATUM:    02.05.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00569!02.05.17!2.1 ! NEU                       ! G. BRUDER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0226                           *
      *****************************************************************
 
        05 WB-NI-VIVB0226-TAB.
           10 WB-NI-VIVB0226-ELE   PIC S9(04)  BINARY OCCURS   5.
        05 WB-NI-VIVB0226          REDEFINES   WB-NI-VIVB0226-TAB.
           10 WB-NI-FPRNERF-ST-NR         PIC S9(04)       BINARY.
           10 WB-NI-FPRNERF-STICHTAG      PIC S9(04)       BINARY.
           10 WB-NI-FPRNERF-LAST-UPD      PIC S9(04)       BINARY.
           10 WB-NI-KNR-NERF-GRUND        PIC S9(04)       BINARY.
           10 WB-NI-KTY-NERF-GRUND        PIC S9(04)       BINARY.
 
        05 WB-VIVB0226.
           10 WB-FPRNERF-ST-NR            PIC S9(09)       BINARY.
           10 WB-FPRNERF-STICHTAG         PIC  X(10).
           10 WB-FPRNERF-LAST-UPD         PIC  X(26).
           10 WB-KNR-NERF-GRUND           PIC S9(04)       BINARY.
           10 WB-KTY-NERF-GRUND           PIC S9(04)       BINARY.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   5                    *
      *****************************************************************

