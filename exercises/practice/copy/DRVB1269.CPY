      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  06.11.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1269.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1269.CPv  $
      *
      *   Rev 2.1   Nov 07 2019 11:46:02   u130213
      *AVB01580: Einführung neues ZUSTRUK-Verfahren
      *
      *   Rev 2.0   Nov 06 2019 11:57:48   u130213
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1269                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0329                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: U130213                                          *
      *    DATUM:    06.11.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01580!06.11.19!2.1 ! NEU                       ! U130213   *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1269 - ERMITTELN SATZ AUS TABELLE SWABZ       *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1269.
          05 PI-DRVB1269-DATEN.
             10 PI-DRVB1269-SWABZ-BETR-NR      PIC S9(09)       BINARY.
             10 PI-DRVB1269-SWABZ-MONAT        PIC  X(10).
             10 PI-DRVB1269-KNR-UST-TYP        PIC S9(04)       BINARY.
             10 PI-DRVB1269-KTY-WB-TYP         PIC S9(04)       BINARY.
             10 PI-DRVB1269-KNR-WB-TYP         PIC S9(04)       BINARY.
 
       01 PO-DRVB1269.
         03 PO-DRVB1269-RC                     PIC  X(03).
             88 PO-DRVB1269-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1269-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1269-DATEN.
          05 PO-DRVB1269-NI-VIVB0329.
             10 NI-DRVB1269-SWABZ-BETR-NR      PIC S9(04)       BINARY.
             10 NI-DRVB1269-SWABZ-MONAT        PIC S9(04)       BINARY.
             10 NI-DRVB1269-KTY-UST-TYP        PIC S9(04)       BINARY.
             10 NI-DRVB1269-KNR-UST-TYP        PIC S9(04)       BINARY.
             10 NI-DRVB1269-KTY-WB-TYP         PIC S9(04)       BINARY.
             10 NI-DRVB1269-KNR-WB-TYP         PIC S9(04)       BINARY.
             10 NI-DRVB1269-SWABZ-EH-ABZUG     PIC S9(04)       BINARY.
             10 NI-DRVB1269-SWABZ-LAST-UPDATE  PIC S9(04)       BINARY.
          05 PO-DRVB1269-DCL-VIVB0329.
             10 PO-DRVB1269-SWABZ-BETR-NR      PIC S9(09)       BINARY.
             10 PO-DRVB1269-SWABZ-MONAT        PIC  X(10).
             10 PO-DRVB1269-KTY-UST-TYP        PIC S9(04)       BINARY.
             10 PO-DRVB1269-KNR-UST-TYP        PIC S9(04)       BINARY.
             10 PO-DRVB1269-KTY-WB-TYP         PIC S9(04)       BINARY.
             10 PO-DRVB1269-KNR-WB-TYP         PIC S9(04)       BINARY.
             10 PO-DRVB1269-SWABZ-EH-ABZUG     PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1269-SWABZ-LAST-UPDATE  PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1269   --------------------------*
      *---------------------------------------------------------------*
