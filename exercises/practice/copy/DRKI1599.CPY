      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.10.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1599.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1599.CPv  $
      *
      *   Rev 2.1   Oct 28 2003 15:54:10   t13ex49
      *UPDATE ADRESSESUCHSTRASSE und ADRESSEGEPRUEFT
      *
      *   Rev 2.0   Oct 23 2003 12:16:48   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * --------------------------------------------------------------*
      * FKI00789!23.10.03!    ! Korrektur / Prüfung       ! P. Pohl   *
      *         !        !    ! AdresseSuchStrasse        !           *
      *         !        !    ! Satzlänge 214 Byte        !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
       01 DRKI1599.
           05  ADRESSEID                   PIC X(13).
           05  ADRESSELANDID               PIC X(03).
           05  ADRESSEORT                  PIC X(30).
           05  NI-ADRESSEORT               PIC X(01).
           05  ADRESSEPLZ                  PIC X(15).
           05  NI-ADRESSEPLZ               PIC X(01).
           05  ADRESSESTRHAUSNUM           PIC X(45).
           05  NI-ADRESSESTRHAUSNUM        PIC X(01).
           05  ADRESSEISTVTADR             PIC X(01).
           05  ADRESSESUCHSTRASSE          PIC X(45).
           05  FILLER                      PIC X(01).
           05  ADRESSEGEPRUEFT             PIC X(01).
           05  FILLER                      PIC X(01).
           05  LETZTEAENDERUNG             PIC X(26).
           05  FILLER                      PIC X(01).
           05  ADRESSEID-HEX               PIC X(26).
           05  FILLER                      PIC X(01).
           05  FILLER                      PIC X(01).
           05  ADRESSEUPDATE               PIC X(01).
      *------------------ Ende DRKI1599 -----------------------

