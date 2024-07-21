      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.09.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1234.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1234.CPv  $
      *
      *   Rev 2.1   13 Jan 2020 09:27:08   t13ae56
      *AVB01581: PGM - Paket 2 - Dialoge SL16 + SL17
      *
      *   Rev 2.0   Sep 20 2019 14:06:34   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1234                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0021                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    20.09.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VBL-819 !20.09.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1234 - ERMITTELN BV-SATZ FÜR BV-TYP           *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1234.
          05 PI-DRVB1234-DATEN.
             10 PI-DRVB1234-ST-NR              PIC S9(09)       BINARY.
             10 PI-DRVB1234-KNR-BV-TYP         PIC S9(04)       BINARY.
 
       01 PO-DRVB1234.
         03 PO-DRVB1234-RC                     PIC  X(03).
             88 PO-DRVB1234-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1234-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1234-DATEN.
          05 PO-DRVB1234-NI-VIVBA021.
             10 NI-DRVB1234-ST-NR              PIC S9(04)       BINARY.
             10 NI-DRVB1234-KTY-BV-TYP         PIC S9(04)       BINARY.
             10 NI-DRVB1234-KNR-BV-TYP         PIC S9(04)       BINARY.
             10 NI-DRVB1234-BV-DAT-VON         PIC S9(04)       BINARY.
             10 NI-DRVB1234-BV-LAST-UPDATE     PIC S9(04)       BINARY.
             10 NI-DRVB1234-BV-DAT-BIS         PIC S9(04)       BINARY.
             10 NI-DRVB1234-KTY-SPERRE         PIC S9(04)       BINARY.
             10 NI-DRVB1234-KNR-SPERRE         PIC S9(04)       BINARY.
             10 NI-DRVB1234-BV-KTO-NR          PIC S9(04)       BINARY.
             10 NI-DRVB1234-BV-BLZ             PIC S9(04)       BINARY.
             10 NI-DRVB1234-BV-BANKNAME        PIC S9(04)       BINARY.
             10 NI-DRVB1234-BV-KTO-INHABER     PIC S9(04)       BINARY.
             10 NI-DRVB1234-BV-IBAN            PIC S9(04)       BINARY.
             10 NI-DRVB1234-BV-BIC             PIC S9(04)       BINARY.
          05 PO-DRVB1234-DCL-VIVBA021.
             10 PO-DRVB1234-ST-NR              PIC S9(09)       BINARY.
             10 PO-DRVB1234-KTY-BV-TYP         PIC S9(04)       BINARY.
             10 PO-DRVB1234-KNR-BV-TYP         PIC S9(04)       BINARY.
             10 PO-DRVB1234-BV-DAT-VON         PIC  X(26).
             10 PO-DRVB1234-BV-LAST-UPDATE     PIC  X(26).
             10 PO-DRVB1234-BV-DAT-BIS         PIC  X(10).
             10 PO-DRVB1234-KTY-SPERRE         PIC S9(04)       BINARY.
             10 PO-DRVB1234-KNR-SPERRE         PIC S9(04)       BINARY.
             10 PO-DRVB1234-BV-KTO-NR          PIC  X(20).
             10 PO-DRVB1234-BV-BLZ             PIC  X(11).
             10 PO-DRVB1234-BV-BANKNAME        PIC  X(40).
             10 PO-DRVB1234-BV-KTO-INHABER     PIC  X(40).
             10 PO-DRVB1234-BV-IBAN            PIC  X(34).
             10 PO-DRVB1234-BV-BIC             PIC  X(11).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1234   --------------------------*
      *---------------------------------------------------------------*
