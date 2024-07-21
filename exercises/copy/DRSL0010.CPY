      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.10.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0010.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRSL0010.CPv  $
      *
      *   Rev 2.1   Nov 28 2003 17:17:30   t13ae35
      *ASL00176 - Neuerstellung
      *
      *   Rev 2.0   Oct 29 2003 16:25:44   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00176!29.10.03!2.1 ! Neu                       ! Hering   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *            Inhalt    :  Zusatzbonus-CTV-Datei                *
      *            Datei     :  SL008600 und SL008700                *
      *            Member    :  DRSL0010                             *
      *            Organisat.:  VSAM                                 *
      *            Satzlänge :  37 Byte                              *
      *            Key       :  VBNR (1-7)                           *
      *                                                              *
      *--------------------------------------------------------------*
       01  DRSL0010-SATZ.
           05 DRSL0010-VBNR                     PIC 9(007).
           05 DRSL0010-DIR-AKT                  PIC 9(004).
           05 DRSL0010-JAHR                     PIC 9(004).
           05 DRSL0010-ZUBO-BETRAG              PIC 9(009)V99.
           05 DRSL0010-ZUBO-EH                  PIC 9(010)V9.

