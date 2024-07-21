      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0010.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVD0010.CPv  $
      *
      *   Rev 2.2   Feb 09 2012 13:36:02   t13ex19
      *AKI01118: VBI-SST - Anpassung Länge Rücknahmepreis
      *
      *   Rev 2.1   Nov 23 2004 16:48:36   t13ex49
      *AKI00422: VBI-DWH Schnittstelle KI-Kunden- und Vertragsbestand
      *
      *   Rev 2.0   Jun 28 2004 15:09:46   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00422!16.06.04!    ! Neu                       ! P. Pohl   *
      *         !        !    !                           !           *
      * --------------------------------------------------------------*
      * AKI01118!01.02.12!    ! RNPRUECKNAHMERBETR VERLÄN-! BST       *
      *         !        !    ! GERT                      !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Daten für DWH-Schnittstelle - Unload Rücknahmepreis
      *
      * Satzlänge 16 Byte
      *
       01 DRVD0010.
           05  DRVD0010-RNPFONDID                PIC S9(4) COMP.
           05  DRVD0010-RNPRUECKNAHMEBETR        PIC S9(7)V9(2) COMP-3.
           05  FILLER                            PIC X(01).

      *----------- Ende DRVD0010 ---------------------------

