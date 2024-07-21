      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0011.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVD0011.CPv  $
      *
      *   Rev 2.1   Nov 23 2004 16:48:42   t13ex49
      *AKI00422: VBI-DWH Schnittstelle KI-Kunden- und Vertragsbestand
      *
      *   Rev 2.0   Jun 28 2004 15:09:56   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00422!01.10.04!    ! Neu                       ! P. Pohl   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Daten für DWH-Schnittstelle - Altkunden
      *
      * Satzlänge  Byte
      *
       01 UNLAKD-SATZ.
           05  DRVD0011-KDPERSONENID              PIC X(13).
           05  DRVD0011-KIKUNDENSTATUSID          PIC X(01).
           05  DRVD0011-KIKUNDENNUMMER            PIC X(10).
           05  FILLER                             PIC X(01).
           05  DRVD0011-MITARBPERSONENID          PIC X(13).
           05  FILLER                             PIC X(03).
           05  DRVD0011-MITARBEITERNR             PIC X(07).
           05  DRVD0011-MITARBEITERNR-NI          PIC X(01).
           05  DRVD0011-ADMAAUSTRITTSDATUM        PIC X(10).
           05  DRVD0011-ADMAAUSTRITTSDATUM-NI     PIC X(01).
           05  FILLER                             PIC X(07).
           05  DRVD0011-ADMSTRUKTURSTUFE          PIC X(03).
           05  DRVD0011-ADMSTRUKTURSTUFE-NI       PIC X(01).
           05  DRVD0011-MITARBVORGESPERSID        PIC X(13).
           05  DRVD0011-MITARBVORGESPERSID-NI     PIC X(01).
           05  DRVD0011-ADMAAUSTRITTSDA-V      PIC X(10).
           05  DRVD0011-ADMAAUSTRITTSDA-V-NI   PIC X(01).
           05  FILLER                             PIC X(07).
           05  DRVD0011-ADMSTRUKTURSTUFE-V        PIC X(03).
           05  DRVD0011-ADMSTRUKTURSTUFE-V-NI     PIC X(01).
           05  DRVD0011-MITARBVORGESPERSID-2      PIC X(13).
           05  FILLER                             PIC X(03).
           05  DRVD0011-MITARBEITERNR-V           PIC X(07).
           05  DRVD0011-MITARBEITERNR-V-NI        PIC X(01).

      *----------- Ende DRVD0011 ---------------------------

