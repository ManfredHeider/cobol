      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2720.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2720.CPv  $
      *
      *   Rev 2.1   Mar 25 2013 13:10:48   t13ex19
      *AKI01210: Ermitteln Anzahl Kunden/vers. Personen
      *
      *   Rev 2.0   Feb 26 2013 16:52:10   t13ex19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01210!26.02.13!    ! Neu                       ! B. Stoldt *
      * --------------------------------------------------------------*
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Eingabe f. Auswertung versicherte Personen
      *
      * Satzlänge  241
      *
       01 DRKI2720.
           05  DRKI2720-VERTRAGSID-CHAR.
             10 Filler                           PIC X(02).
             10 DRKI2720-GESELLSCHID             PIC X(02).
             10 FILLER                           PIC X(22).
           05  DRKI2720-VERTRAGSID               PIC X(13).
           05  DRKI2720-VETLFDNR                 PIC 9(04) COMP.
           05  DRKI2720-PRODUKTID                PIC 9(04) COMP.
           05  FILLER                            PIC X(03).
           05  DRKI2720-VBNUMMER                 PIC 9(07).
           05  FILLER                            PIC X(01).
           05  DRKI2720-VP-PERSONENID            PIC X(13).
           05  FILLER                            PIC X(01).
           05  DRKI2720-VP-NAME.
             10  DRKI2720-VP-NACHNAME            PIC X(30).
             10  FILLER                          PIC X(01).
             10  DRKI2720-VP-VORNAME             PIC X(30).
           05  Filler                            PIC x(01).
           05  DRKI2720-VP-GEBDATUM              PIC X(10).
           05  Filler                            PIC x(01).
           05  DRKI2720-ADRESSEPLZ               PIC X(15).
           05  Filler                            PIC x(01).
           05  DRKI2720-ADRESSEORT               PIC X(30).
           05  Filler                            PIC x(01).
           05  DRKI2720-ADRESSESUCHSTRASSE       PIC X(45).
           05  Filler                            PIC x(01).
           05  DRKI2720-ADRESSEGEPRUEFT          PIC X(01).
           05  Filler                            PIC x(01).
           05  DRKI2720-VERVERTRAGSNUMMER        PIC X(15).
           05  Filler                            PIC x(03).
           05  DRKI2720-ROLLE                    PIC X(01).

      *----------------Ende DRKI2720 ----------------
