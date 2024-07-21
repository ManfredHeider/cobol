      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0006.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVD0006.CPv  $
      *
      *   Rev 2.1   Nov 23 2004 16:48:08   t13ex49
      *AKI00422: VBI-DWH Schnittstelle KI-Kunden- und Vertragsbestand
      *
      *   Rev 2.0   Jun 25 2004 10:23:26   t13ex49
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
      * Anzahl versicherter Personen bei Central-Verträgen
      *
      * Satzlänge 20 Byte
      *
       01 DRVD0006.
           05  DRVD0006-VERTRAGSID                PIC X(13).
           05  FILLER                             PIC X(04).
           05  DRVD0006-ANZVP                     PIC 9(03).

      *----------- Ende DRVD0006 ---------------------------

