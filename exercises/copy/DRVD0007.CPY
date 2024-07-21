      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0007.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVD0007.CPv  $
      *
      *   Rev 2.1   Nov 23 2004 16:48:16   t13ex49
      *AKI00422: VBI-DWH Schnittstelle KI-Kunden- und Vertragsbestand
      *
      *   Rev 2.0   Jun 28 2004 15:09:20   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00422!16.06.04!    ! Neu                       ! P. Pohl   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Daten für DWH-Schnittstelle - verschiedene Kennzeichen aus KI-
      * Tabellen
      *
      * Satzlänge 25 Byte
      *
       01 DRVD0007.
           05  DRVD0007-VERTRAGSID                PIC X(13).
           05  FILLER                             PIC X(02).
           05  DRVD0007-AML-DYN-ERH               PIC X(01).
           05  DRVD0007-AML-Beitragsfrei          PIC X(01).
           05  DRVD0007-VWL                       PIC X(01).
           05  FILLER                             PIC X(07).

      *----------- Ende DRVD0007 ---------------------------

