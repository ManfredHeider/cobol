      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0038.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAS0038.CPv  $
      *
      *   Rev 2.1   Feb 04 2014 09:48:02   t13ex57
      *AVB01296: Datenänderung von VB über VBI
      *
      *   Rev 2.0   Jan 13 2014 15:49:18   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
130114* AVB01296!13.01.14!    ! neue Datei 34F Daten      ! U. Boll   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Eingabedatei der von VBI gelieferten VB 34F Daten zur
      * Übernahme in Tabelle MA
      *
      * Satzlänge 200
      *
      *
       01 DRAS0038.
           05  DRAS0038-VBNR                      PIC 9(07).
           05  DRAS0038-34F-AMT                   PIC X(40).
           05  DRAS0038-34F-STRASSE               PIC X(40).
           05  DRAS0038-34F-PLZ                   PIC X(09).
           05  DRAS0038-34F-ORT                   PIC X(40).
           05  FILLER                             PIC X(64).
      *----------- Ende DRAS0038 ---------------------------
