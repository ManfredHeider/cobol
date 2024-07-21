      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.04.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0022.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAS0022.CPv  $
      *
      *   Rev 2.1   May 29 2008 09:06:36   t13ex57
      *AVB00875: Verlängerung der Adressfelder im VB-Datenbestand -
      * UBO
      *
      *   Rev 2.0   Apr 08 2008 10:36:16   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAS00875!08.04.08!    ! Neu                       ! U. Boll   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Eingabedatei der von VBI gelieferten VB 34C Daten zur
      * Übernahme in Tabelle MA
      *
      * Satzlänge 200
      *
      *
       01 DRAS0022.
           05  DRAS0022-VBNR                      PIC 9(07).
           05  DRAS0022-34C-AMT                   PIC X(40).
           05  DRAS0022-34C-STRASSE               PIC X(40).
           05  DRAS0022-34C-PLZ                   PIC X(09).
           05  DRAS0022-34C-ORT                   PIC X(40).
           05  FILLER                             PIC X(64).
      *----------- Ende DRAS0022 ---------------------------
