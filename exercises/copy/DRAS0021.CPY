      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0021.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAS0021.CPv  $
      *
      *   Rev 2.1   May 29 2008 09:06:22   t13ex57
      *AVB00875: Verlängerung der Adressfelder im VB-Datenbestand -
      * UBO
      *
      *   Rev 2.0   15 Feb 2007 12:24:44   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAS00875!15.02.07!    ! Neu                       ! U. Boll   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Eingabedatei der von VBI gelieferten VB Adressdaten zur
      * Übernahme in Tabelle MAAN
      *
      * Satzlänge 200
      *
      *
       01 DRAS0021.
           05  DRAS0021-VBNR                      PIC 9(07).
           05  DRAS0021-ADRESSESTRHAUSNUM         PIC X(40).
           05  DRAS0021-ADRESSEPLZ                PIC X(09).
           05  DRAS0021-ADRESSEORT                PIC X(40).
           05  DRAS0021-ADRESS-TYP                PIC X(01).
           05  DRAS0021-LAND                      PIC X(02).
           05  DRAS0021-ZUSTELL                   PIC X(01).
           05  DRAS0021-TELEFON                   PIC X(26).
           05  DRAS0021-TELEFAX                   PIC X(26).
           05  DRAS0021-LOESCH-KZ                 PIC X(01).
           05  FILLER                             PIC X(47).
      *----------- Ende DRAS0021 ---------------------------
      *
