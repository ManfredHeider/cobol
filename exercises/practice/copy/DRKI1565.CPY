      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1565.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1565.CPv  $
      *
      *   Rev 2.1   Oct 13 2003 12:04:28   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 2.0   May 19 2003 09:59:46   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * --------------------------------------------------------------*
      * AKI00114!17.07.03!    ! Migration Adressarten     ! P. Pohl   *
      *         !        !    ! Satzlänge 145 Byte        !           *
      *         !        !    !                           !           *
      *****************************************************************
       01 DRKI1565.
           05  MHA-SATZART                  PIC X(1).
      *        1 = Personensatz der verarbeitet wird
      *        3 = ID-Satz Hauptadresse des Haushaltes Person 1
      *        4 = ID-Satz Hauptadresse des Haushaltes Person > 1
      *        9 = ID-Satz Hauptadresse der Person
           05  MHA.
               10  FILLER                      PIC X(144).

           05  PERSON REDEFINES MHA.
               10  PERS-PERSONENID             PIC X(13).
               10  PERS-PERSONKUNDENART        PIC X(01).
               10  PERS-PERSONANZNAME          PIC X(30).
               10  FILLER                      PIC X(01).
               10  PERS-PERSONANZVORNAME       PIC X(30).
               10  FILLER                      PIC X(01).
               10  PERS-PERSONGEBGRUENDDAT     PIC X(10).
               10  FILLER                      PIC X(01).
               10  FILLER                      PIC X(30).
               10  FILLER                      PIC X(27).

           05  IDS    REDEFINES MHA.
               10  MHA-PERSONID                PIC X(13).
      *        Personen-ID der natürlichen Person
               10  MHA-PERSONID-HEX            PIC X(26).

               10  MHA-ADRESSEID               PIC X(13).
      *        Adresse-ID der Hauoptadresse des Haushalts
      *        bzw.       der Vertragsadresse
               10  MHA-AdresseID-HEX           PIC X(26).

               10  MHA-PERSHHID                PIC X(13).
      *        Personen-ID des Haushaltes
               10  MHA-PERSHHID-HEX            PIC X(26).
               10  MHA-LETZTEAENDERUNG         PIC X(26).
               10  FILLER                      PIC X(01).

      *-----------------------------------------------------


