      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.09.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0016.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVD0016.CPv  $
      *
      *   Rev 2.1   Nov 23 2004 16:49:02   t13ex49
      *AKI00422: VBI-DWH Schnittstelle KI-Kunden- und Vertragsbestand
      *
      *   Rev 2.0   Sep 21 2004 09:03:40   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00422!21.09.04!    ! Neu                       ! P. Pohl   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Daten über betreute Kunden
      *
      * Satzlänge 21 Byte
      *
       01 DRVD0016 .

           05  DRVD0016-KD-NR                   PIC S9(9) COMP.
           05  DRVD0016-KD-ID                   PIC X(13).
           05  DRVD0016-VB-NR                   PIC S9(9) COMP.

      *----------- Ende DRVD0016  ---------------------------

