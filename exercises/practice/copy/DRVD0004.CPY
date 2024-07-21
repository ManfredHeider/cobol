      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.04.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0004.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVD0004.CPv  $
      *
      *   Rev 2.1   Nov 23 2004 16:47:56   t13ex49
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
      * Daten über Betreuungsverhältnis
      *
      * Satzlänge 31 Byte
      *
       01 DRVD0004.
           05  DRVD0004-VBPERSONENID              PIC X(13).
           05  DRVD0004-VNPERSONENID              PIC X(13).
           05  FILLER                             PIC X(02).
           05  DRVD0004-ANZ-Betreuer              PIC X(03).

      *----------- Ende DRVD0004 ---------------------------
      *

