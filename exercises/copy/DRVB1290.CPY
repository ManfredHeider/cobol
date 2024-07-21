      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.09.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1290.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1290.CPv  $
      *
      *   Rev 2.1   Dec 21 2020 17:34:34   t13ae30
      *AVB01642: Anpassung PMVD0010
      *
      *   Rev 2.0   Sep 09 2020 13:23:32   T13AE30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1290                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0138                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T13AE30                                          *
      *    DATUM:    09.09.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA3989 !09.09.20!2.1 ! NEU                       ! T13AE30   *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1290 - ERMITTELN SATZ AUS TABELLE PEHDAI      *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1290.
          05 PI-DRVB1290-DATEN.
             10 PI-DRVB1290-PEHDAI-VS-ZEITRAUM PIC S9(04)       BINARY.
             10 PI-DRVB1290-PEHDAI-BETR        PIC S9(09)       BINARY.
             10 PI-DRVB1290-PEHDAI-UNTST       PIC S9(09)       BINARY.
 
       01 PO-DRVB1290.
         03 PO-DRVB1290-RC                     PIC  X(03).
             88 PO-DRVB1290-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1290-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1290-DATEN.
          05 PO-DRVB1290-NI-VIVB0138.
             10 NI-DRVB1290-PEHDAI-VS-ZEITRAUM PIC S9(04)       BINARY.
             10 NI-DRVB1290-PEHDAI-BETR        PIC S9(04)       BINARY.
             10 NI-DRVB1290-PEHDAI-UNTST       PIC S9(04)       BINARY.
             10 NI-DRVB1290-PEHDAI-DAT-VON     PIC S9(04)       BINARY.
             10 NI-DRVB1290-PEHDAI-DAT-BIS     PIC S9(04)       BINARY.
             10 NI-DRVB1290-PEHDAI-ERSTELL-DAT PIC S9(04)       BINARY.
             10 NI-DRVB1290-PEHDAI-EH-DAISY    PIC S9(04)       BINARY.
             10 NI-DRVB1290-PEHDAI-UNTST-WERT  PIC S9(04)       BINARY.
          05 PO-DRVB1290-DCL-VIVB0138.
             10 PO-DRVB1290-PEHDAI-VS-ZEITRAUM PIC S9(04)       BINARY.
             10 PO-DRVB1290-PEHDAI-BETR        PIC S9(09)       BINARY.
             10 PO-DRVB1290-PEHDAI-UNTST       PIC S9(09)       BINARY.
             10 PO-DRVB1290-PEHDAI-DAT-VON     PIC  X(10).
             10 PO-DRVB1290-PEHDAI-DAT-BIS     PIC  X(10).
             10 PO-DRVB1290-PEHDAI-ERSTELL-DAT PIC  X(10).
             10 PO-DRVB1290-PEHDAI-EH-DAISY    PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1290-PEHDAI-UNTST-WERT  PIC  X(01).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1290   --------------------------*
      *---------------------------------------------------------------*

