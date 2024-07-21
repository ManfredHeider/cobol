      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1564.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1564.CPv  $
      *
      *   Rev 2.1   Oct 13 2003 12:04:24   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 2.0   May 19 2003 09:59:42   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * --------------------------------------------------------------*
      * AKI00114!17.07.03!    ! Migration Adressarten     ! P. Pohl   *
      *         !        !    ! Satzlänge 121 BYTE        !           *
      *         !        !    !                           !           *
      *****************************************************************
       01 DRKI1564.
            05  KORR-PERSONID                PIC X(13).
            05  KORR-PERSONIDHEX             PIC X(26).
      *         Personen-ID der natürlichen Person
            05  KORR-ADRESSEID               PIC X(13).
            05  KORR-ADRESSEIDHEX            PIC X(26).
      *         Adresse-ID der Hauptadresse der PERSON
            05  KORR-PERSHHID                PIC X(13).
            05  KORR-PERSHHIDHEX             PIC X(26).
      *         Personen-ID des Haushaltes
            05  KORR-AENDERUNGS-KZ           PIC X(01).
      *         I = Input / U = UPDATE / D = DELETE
            05  KORR-ADRESS-ART-ALT          PIC X(01).
            05  KORR-ADRESS-ART-NEU          PIC X(01).
            05  KORR-ART                     PIC X(01).

      *---------------------------------------------------------------*

