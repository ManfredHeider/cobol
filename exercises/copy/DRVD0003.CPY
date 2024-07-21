      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.04.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0003.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVD0003.CPv  $
      *
      *   Rev 2.1   Nov 23 2004 16:47:48   t13ex49
      *AKI00422: VBI-DWH Schnittstelle KI-Kunden- und Vertragsbestand
      *
      *   Rev 2.0   Apr 28 2004 09:28:38   t13ex49
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
      * UNLOAD aus KUNDENINTVBBEZ und KUNDENINTERESSENT
      *
      * Satzlänge 025 Byte
      *
       01 DRVD0003.
           05  DRVD0003-KDPERSONENID              PIC X(13).
           05  DRVD0003-KIKUNDENSTATUSID          PIC X(01).
           05  DRVD0003-KIKUNDENNUMMER            PIC X(10).
           05  FILLER                             PIC X(01).

      *----------- Ende DRVD0003 ---------------------------
      *
