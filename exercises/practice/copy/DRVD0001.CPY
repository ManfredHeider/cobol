      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.04.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0001.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVD0001.CPv  $
      *
      *   Rev 2.1   Nov 23 2004 16:47:28   t13ex49
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
      * UNLOAD aus MITARBEITER und AUSSENDIENSTMA
      *
      * Satzlänge 106 Byte
      *
       01 DRVD0001.
           05  DRVD0001-MITARBPERSONENID          PIC X(13).
           05  FILLER                             PIC X(03).
           05  DRVD0001-MITARBEITERNR             PIC X(07).
           05  DRVD0001-MITARBEITERNR-NI          PIC X(01).
           05  DRVD0001-ADMAAUSTRITTSDATUM        PIC X(10).
           05  DRVD0001-ADMAAUSTRITTSDATUM-NI     PIC X(01).
           05  FILLER                             PIC X(07).
           05  DRVD0001-ADMSTRUKTURSTUFE          PIC X(03).
           05  DRVD0001-ADMSTRUKTURSTUFE-NI       PIC X(01).
           05  DRVD0001-MITARBVORGESPERSID        PIC X(13).
           05  DRVD0001-MITARBVORGESPERSID-NI     PIC X(01).
           05  DRVD0001-ADMAAUSTRITTSDA-V      PIC X(10).
           05  DRVD0001-ADMAAUSTRITTSDA-V-NI   PIC X(01).
           05  FILLER                             PIC X(07).
           05  DRVD0001-ADMSTRUKTURSTUFE-V        PIC X(03).
           05  DRVD0001-ADMSTRUKTURSTUFE-V-NI     PIC X(01).
           05  DRVD0001-MITARBVORGESPERSID-2      PIC X(13).
      **** 05  DRVD0001-MITARBVORGESPERSID-2-NI   PIC X(01).
           05  FILLER                             PIC X(03).
           05  DRVD0001-MITARBEITERNR-V           PIC X(07).
           05  DRVD0001-MITARBEITERNR-V-NI        PIC X(01).

      *----------- Ende DRVD0001 ---------------------------
      *

