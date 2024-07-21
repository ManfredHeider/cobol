      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  07.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1219.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1219.CPv  $
      *
      *   Rev 2.1   Jun 28 2019 12:33:44   t13ae56
      *AVB01570: PGM - Gutscheincode
      *
      *   Rev 2.0   Jun 07 2019 15:10:18   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1219                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0132                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    25.06.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VBL-998 !25.06.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1219 - ERMITTELN SATZ AUS TABELLE GUTSC       *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1219.
          05 PI-DRVB1219-DATEN.
             10 PI-DRVB1219-GUTSC-ST-NR        PIC S9(09)       BINARY.
             10 PI-DRVB1219-KNR-GUTSC-TYP      PIC S9(04)       BINARY.
             10 PI-DRVB1219-GUTSC-GUELT-AB     PIC  X(10).
 
       01 PO-DRVB1219.
         03 PO-DRVB1219-RC                     PIC  X(03).
             88 PO-DRVB1219-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1219-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1219-DATEN.
          05 PO-DRVB1219-NI-VIVB0132.
             10 NI-DRVB1219-GUTSC-ST-NR        PIC S9(04)       BINARY.
             10 NI-DRVB1219-KTY-GUTSC-TYP      PIC S9(04)       BINARY.
             10 NI-DRVB1219-KNR-GUTSC-TYP      PIC S9(04)       BINARY.
             10 NI-DRVB1219-GUTSC-CODE         PIC S9(04)       BINARY.
             10 NI-DRVB1219-GUTSC-GUELT-AB     PIC S9(04)       BINARY.
             10 NI-DRVB1219-GUTSC-GUELT-BIS    PIC S9(04)       BINARY.
             10 NI-DRVB1219-GUTSC-MELD-DAT     PIC S9(04)       BINARY.
             10 NI-DRVB1219-GUTSC-LAST-UPD     PIC S9(04)       BINARY.
          05 PO-DRVB1219-DCL-VIVB0132.
             10 PO-DRVB1219-GUTSC-ST-NR        PIC S9(09)       BINARY.
             10 PO-DRVB1219-KTY-GUTSC-TYP      PIC S9(04)       BINARY.
             10 PO-DRVB1219-KNR-GUTSC-TYP      PIC S9(04)       BINARY.
             10 PO-DRVB1219-GUTSC-CODE         PIC  X(20).
             10 PO-DRVB1219-GUTSC-GUELT-AB     PIC  X(10).
             10 PO-DRVB1219-GUTSC-GUELT-BIS    PIC  X(10).
             10 PO-DRVB1219-GUTSC-MELD-DAT     PIC  X(10).
             10 PO-DRVB1219-GUTSC-LAST-UPD     PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1219   --------------------------*
      *---------------------------------------------------------------*
