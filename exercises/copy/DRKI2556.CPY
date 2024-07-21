      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.01.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2556.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2556.CPv  $
      *
      *   Rev 2.2   Jan 31 2012 09:26:06   t13ex15
      *AKI01113: AS-Club AM-Ver neues Produkt P.U.R.
      *
      *   Rev 2.1   Jan 30 2012 07:55:18   t13ex15
      *AKI01113: AS-Club AM-Ver neues Produkt P.U.R.
      *
      *   Rev 2.0   Jan 26 2012 11:21:06   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01113!26.01.12!    ! Neu                       ! SWZ       *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Daten für Unload Rücknahmepreis mit Fondsinformationen
      * für AS-Club und VBI
      *
      * Satzlänge 10 Byte
      *

       01 DRKI2556.
           05  DRKI2556-RNPFONDID                PIC S9(4) COMP.
           05  DRKI2556-RNPRUECKNAHMEBETR        PIC S9(7)V9(2) COMP-3.
           05  FILLER                            PIC X(01).
           05  DRKI2556-FONDGESNR                PIC S9(4) COMP.
           05  FILLER                            PIC X(01).

      *----------- Ende DRKI2556 ---------------------------

