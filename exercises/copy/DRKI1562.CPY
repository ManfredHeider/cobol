      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1562.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1562.CPv  $
      *
      *   Rev 2.1   Oct 13 2003 12:04:20   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 2.0   May 19 2003 09:59:32   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * --------------------------------------------------------------*
      * AKI00114!15.07.03!    ! Migration Adressarten     ! P. Pohl   *
      *         !        !    ! Satzlänge 88 Byte         !           *
      *         !        !    !                           !           *
      *****************************************************************
       01 DRKI1562.
           05  PERS-SATZART                PIC X(01).
      *        1 = Personen (keine Haushalte und keine Mitarbeiter)
      *        2 = Personen mit einer    Hauptadresse
      *        3 = Personen mit mehreren Hauptadressen
           05  PERS-PERSONENID             PIC X(13).
           05  PERS-PERSONKUNDENART        PIC X(01).
           05  PERS-PERSONANZNAME          PIC X(30).
           05  FILLER                      PIC X(01).
           05  PERS-PERSONANZVORNAME       PIC X(30).
           05  FILLER                      PIC X(01).
           05  PERS-PERSONGEBGRUENDDAT     PIC X(10).
           05  FILLER                      PIC X(01).


