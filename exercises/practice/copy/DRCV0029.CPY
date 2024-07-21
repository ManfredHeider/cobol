      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  19.05.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0029.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRCV0029.CPv  $
      *
      *   Rev 2.1   Jun 15 2020 15:52:30   t13ae35
      *ACV00223: K-Wechselwettbewerb 2019 / CTV-Brief (VB-System)
      *
      *   Rev 2.0   May 19 2020 16:26:38   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ACV00223!20.05.20!2.1 ! Neu                       ! Hering   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *            Inhalt    :  Parameter für BAT2102 (K-Wechsel-WBW)*
      *            Datei     :  CV017000                             *
      *            Member    :  DRCV0029                             *
      *            Organisat.:  VSAM                                 *
      *            Satzlaenge:  56 Byte                              *
      *            Key       :  VBNR,VersNr                          *
      *                                                              *
      *--------------------------------------------------------------*
       01  DRCV0029-SATZ.
           05 DRCV0029-KEY.
              10 DRCV0029-VBNR                     PIC 9(007).
              10 DRCV0029-VERSNR                   PIC X(009).
           05 DRCV0029-TYP                         PIC X(01).
           05 DRCV0029-KUNDE                       PIC X(020).
           05 DRCV0029-ANZ                         PIC Z(4)9.
           05 DRCV0029-ANZE                        PIC Z(4)9.
           05 DRCV0029-BETRAG                      PIC Z(5)9,99.

