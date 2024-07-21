      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.04.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0015.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRCV0015.CPv  $
      *
      *   Rev 2.1   Apr 06 2009 14:51:52   i13az25
      *ACV00111: AVAD-Abmeldungen -> Beschaffungsprogramm
      *
      *   Rev 2.0   Apr 06 2009 11:07:04   i13az25
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ACV00111!06.04.09!2.1 ! NEU                       ! MERTSCH  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *            INHALT    :  AVAD-INFOS FÜR CTV-BRIEF             *
      *            DATEI     :  CVXXXXXX                             *
      *            MEMBER    :  DRCV0015                             *
      *            ORGANISAT.:  VSAM                                 *
      *            SATZLÄNGE :  93 BYTE                              *
      *            KEY       :  VBNR (1-7)                           *
      *                                                              *
      *--------------------------------------------------------------*
       01  DRCV0015-SATZ.
           05 DRCV0015-VBNR                     PIC 9(007).
           05 DRCV0015-PZ                       PIC X(001).
           05 DRCV0015-GESELLSCHAFT             PIC 9(005).
           05 DRCV0015-NR-GRUND                 PIC 9(005).
           05 DRCV0015-KTO-SPERRE               PIC 9(005).
           05 DRCV0015-ABMELDUNGS-TYP           PIC X(011).
           05 DRCV0015-GRUND                    PIC X(049).
           05 DRCV0015-RESTSCHULD               PIC 9(008)V99.
