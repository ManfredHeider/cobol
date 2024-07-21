      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.12.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0042.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAS0042.CPv  $
      *
      *   Rev 2.2   Jan 13 2016 14:09:24   t13ex57
      *AVB01376: Stammdatenerfassung §34i - UBO
      *
      *   Rev 2.1   Jan 07 2016 09:05:10   t13ex57
      *AVB01376: Stammdatenerfassung §34i - UBO
      *
      *   Rev 2.0   Dec 09 2015 15:55:02   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AVB01376!09.12.15!    ! Neu                       ! U. Boll   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Eingabedatei der von VBI gelieferten VB 34i Daten zur
      * Übernahme in Tabelle MA
      *
      * Satzlänge 200
      *
      *
       01 DRAS0042.
           05  DRAS0042-VBNR                      PIC 9(07).
           05  DRAS0042-34I-AMT                   PIC X(40).
           05  DRAS0042-34I-STRASSE               PIC X(40).
           05  DRAS0042-34I-PLZ                   PIC X(09).
           05  DRAS0042-34I-ORT                   PIC X(40).
           05  FILLER                             PIC X(64).
      *----------- Ende DRAS0042 ---------------------------


