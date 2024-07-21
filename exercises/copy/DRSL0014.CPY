      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.07.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0014.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRSL0014.CPv  $
      *
      *   Rev 2.2   Sep 15 2016 15:59:34   t13ae35
      *AVB01400: Projekt Lucy2 - Leistungsbonus DVAG CTV-Anschreibe
      *n (jhe)
      *
      *   Rev 2.1   Jul 08 2005 10:16:38   t13ex43
      *ASL00204: CTV - K-Leistungszuschuss
      *
      *   Rev 2.0   Jul 01 2005 16:21:48   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME     *
      * --------------------------------------------------------------*
      * ASL00204!01.07.05!2.1 ! NEU                        ! KLEEMEIER*
070916* AVB01400!07.09.16!2.2 ! Lucy2-Anpassung            ! J.Hering *
      *         !        !    !                            !          *
      *         !        !    !                            !          *
      *                                                               *
      *****************************************************************
      *                                                               *
      *            INHALT    :  LEISTUNGSZUSCHUSS (NEU)               *
      *            DATEI     :  SL011800 (PRAXIS), SL011900 (BESTAND) *
      *            MEMBER    :  DRSL0014                              *
      *            ORGANISAT.:  VSAM                                  *
      *            SATZLÄNGE :  91 BYTE  -> 117 BYTE (ab Lucy2)       *
      *            KEY       :  VBNR (1-7)                            *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRSL0014-SATZ.
           05 DRSL0014-VBNR                     PIC 9(007).
           05 DRSL0014-VBNR-DLEITER             PIC 9(007).
           05 DRSL0014-DIR-NR                   PIC 9(003).
           05 DRSL0014-QUARTAL                  PIC 9(001).
           05 DRSL0014-JAHR                     PIC 9(004).
           05 DRSL0014-LAUFDATUM                PIC X(010).
           05 DRSL0014-STUFE-BEZ                PIC X(004).
           05 DRSL0014-STUFE-PRAXIS             PIC X(002).
           05 DRSL0014-12MON-ZEIT               PIC X(013).
           05 DRSL0014-FAKTOR                   PIC 9(003)V99 COMP-3.
070916     05 DRSL0014-12MON-EH-FREI            PIC S9(010)V9  COMP-3.
070916     05 DRSL0014-12MON-EH-PFL             PIC S9(010)V9  COMP-3.
070916     05 DRSL0014-12MON-EH-SUMME           PIC S9(012)V9  COMP-3.
           05 DRSL0014-03MON-ZEIT               PIC X(013).
070916     05 DRSL0014-03MON-EH-FREI            PIC S9(010)V9  COMP-3.
070916     05 DRSL0014-03MON-EH-PFL             PIC S9(010)V9  COMP-3.
070916     05 DRSL0014-03MON-EH-SUMME           PIC S9(012)V9  COMP-3.
070916     05 DRSL0014-01MON-EH-FREI            PIC S9(010)V9  COMP-3.
070916     05 DRSL0014-01MON-EH-PFL             PIC S9(010)V9  COMP-3.
070916     05 DRSL0014-BETRAG-FREI              PIC S9(011)V99 COMP-3.
070916     05 DRSL0014-BETRAG-PFL               PIC S9(011)V99 COMP-3.
      *****
      *****   ENDE DRSL0014
      *****

