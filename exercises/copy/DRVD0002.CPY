      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.04.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0002.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVD0002.CPv  $
      *
      *   Rev 2.3   Jun 05 2012 11:07:56   t13ex19
      *AKI01124: Erweiterung VBI-SST STVD0060
      *
      *   Rev 2.2   Jun 05 2012 09:47:56   t13ex19
      *AKI01124: Erweiterung VBI-SST STVD0060
      *
      *   Rev 2.1   Nov 23 2004 16:47:42   t13ex49
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
      * AKI01124!27.02.12!    ! erw. um Vertragsablauf    ! B. Stoldt *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * UNLOAD aus Vertrag und VertragRolleBez
      *
      * Satzlänge 106 Byte
      *
       01 DRVD0002.
           05  DRVD0002-KDPERSONENID              PIC X(13).
           05  DRVD0002-VBPERSONENID              PIC X(13).
           05  DRVD0002-VERTRAGSID                PIC X(13).
           05  DRVD0002-VRBKOMMBETRKZ             PIC X(01).
           05  FILLER                             PIC X(03).
           05  DRVD0002-VERPARTGESELLSCHID        PIC 99.
           05  DRVD0002-VERVERTRAGSNR             PIC X(15).
           05  DRVD0002-VERVERTRAGSNR-NI          PIC X(01).
           05  FILLER                             PIC X(03).
           05  DRVD0002-VERVERTRZUSTANDID         PIC X(02).
           05  DRVD0002-VERVERTRZUSTANDID-NI      PIC X(01).
           05  DRVD0002-VERVERTRAGSBEGINN         PIC X(10).
           05  DRVD0002-VERVERTRAGSBEGINN-NI      PIC X(01).
           05  DRVD0002-VEREIGENVERMKZ            PIC X(01).
           05  FILLER                             PIC X(03).
           05  DRVD0002-VERABSCHLUSSVBNR          PIC X(07).
           05  DRVD0002-VERABSCHLUSSVBNR-NI       PIC X(01).
           05  FILLER                             PIC X(01).
           05  DRVD0002-VERGESAMTBEITRAG          PIC S9(9)V99 COMP-3.
           05  DRVD0002-VERGESAMTBEITRAG-NI       PIC X(01).
           05  DRVD0002-VERGESBEITRWKZ            PIC X(01).
           05  DRVD0002-VERZAHLUNGSWEISEID        PIC S9(4) COMP.
           05  DRVD0002-VERZAHLUNGSWEISEID-NI     PIC X(01).
           05  DRVD0002-KVBKUNDEBETREUUNG         PIC X(01).
           05  DRVD0002-KVBKUNDEBETREUUNG-NI      PIC X(01).
           05  DRVD0002-VERVERTRAGSABLAUF         PIC X(10).
           05  DRVD0002-VERVERTRAGSABLAUF-NI      PIC X(01).


      *----------- Ende DRVD0002 ---------------------------
      *

