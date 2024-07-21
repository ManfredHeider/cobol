      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.10.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0030.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVD0030.CPv  $
      *
      *   Rev 2.1   Dec 21 2020 17:48:00   t13ae30
      *AVB01642: Anpassung PMVD0010
      *
      *   Rev 2.0   Oct 22 2020 14:35:46   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      * SATZAUFBAU ZUR TABELLE PEHDAI - TBVB0138
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * VLA-988 !12.10.20!2.0 ! NEU                       ! T13AE30  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       03  DCL-TBVB0138.
           05 WB-PEHDAI-VS-ZEITRAUM       PIC S9(4)        BINARY.
           05 WB-PEHDAI-BETR              PIC S9(09)       BINARY.
           05 WB-PEHDAI-UNTST             PIC S9(09)       BINARY.
           05 WB-PEHDAI-DAT-VON           PIC  X(10).
           05 WB-PEHDAI-DAT-BIS           PIC  X(10).
           05 WB-PEHDAI-ERSTELL-DAT       PIC  X(10).
           05 WB-PEHDAI-EH-DAISY          PIC S9(09)V9(01) COMP-3.
           05 WB-PEHDAI-UNTST-WERT        PIC  X(01).

