      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.02.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0016.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRSL0016.CPv  $
      *
      *   Rev 2.1   Feb 15 2006 09:12:28   t13ex43
      *ASL00224: PGM - Dimension 7 Aufbaubonus
      *
      *   Rev 2.0   Feb 02 2006 13:51:44   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME     *
      * --------------------------------------------------------------*
      * ASL00224!02.02.06!2.1 ! NEU                        ! KLEEMEIER*
      *         !        !    !                            !          *
      *         !        !    !                            !          *
      *                                                               *
      *****************************************************************
      *                                                               *
      *            INHALT    :  DIMENSION 7 - AUFBAUBONUS             *
      *            DATEI     :  CV002300                              *
      *            MEMBER    :  DRSL0016                              *
      *            ORGANISAT.:  VSAM                                  *
      *            SATZLÄNGE :  362 BYTE                              *
      *            KEY       :  VBNR (1-7)                            *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRSL0016-SATZ.
           05 DRSL0016-VBNR                     PIC 9(007).
           05 DRSL0016-ABBA-BETRAG              PIC 9(007)V99 COMP-3.
           05 DRSL0016-PART01-VBNR              PIC X(008).
           05 DRSL0016-PART01-NAME              PIC X(022).
           05 DRSL0016-PART01-BETRAG            PIC 9(007)V99 COMP-3.
           05 DRSL0016-PART02-VBNR              PIC X(008).
           05 DRSL0016-PART02-NAME              PIC X(022).
           05 DRSL0016-PART02-BETRAG            PIC 9(007)V99 COMP-3.
           05 DRSL0016-PART03-VBNR              PIC X(008).
           05 DRSL0016-PART03-NAME              PIC X(022).
           05 DRSL0016-PART03-BETRAG            PIC 9(007)V99 COMP-3.
           05 DRSL0016-PART04-VBNR              PIC X(008).
           05 DRSL0016-PART04-NAME              PIC X(022).
           05 DRSL0016-PART04-BETRAG            PIC 9(007)V99 COMP-3.
           05 DRSL0016-PART05-VBNR              PIC X(008).
           05 DRSL0016-PART05-NAME              PIC X(022).
           05 DRSL0016-PART05-BETRAG            PIC 9(007)V99 COMP-3.
           05 DRSL0016-PART06-VBNR              PIC X(008).
           05 DRSL0016-PART06-NAME              PIC X(022).
           05 DRSL0016-PART06-BETRAG            PIC 9(007)V99 COMP-3.
           05 DRSL0016-PART07-VBNR              PIC X(008).
           05 DRSL0016-PART07-NAME              PIC X(022).
           05 DRSL0016-PART07-BETRAG            PIC 9(007)V99 COMP-3.
           05 DRSL0016-PART08-VBNR              PIC X(008).
           05 DRSL0016-PART08-NAME              PIC X(022).
           05 DRSL0016-PART08-BETRAG            PIC 9(007)V99 COMP-3.
           05 DRSL0016-PART09-VBNR              PIC X(008).
           05 DRSL0016-PART09-NAME              PIC X(022).
           05 DRSL0016-PART09-BETRAG            PIC 9(007)V99 COMP-3.
           05 DRSL0016-PART10-VBNR              PIC X(008).
           05 DRSL0016-PART10-NAME              PIC X(022).
           05 DRSL0016-PART10-BETRAG            PIC 9(007)V99 COMP-3.
      *****
      *****   ENDE DRSL0016
      *****
