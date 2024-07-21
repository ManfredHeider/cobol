      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.05.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0006.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRCV0006.CPv  $
      *
      *   Rev 2.2   Jun 28 2006 10:44:44   t13ex43
      *ASL00226: PGM9 - Dimension 7 Zukunft - MAP
      *
      *   Rev 2.1   Jun 26 2006 11:21:02   t13ex43
      *ASL00226: PGM8 - Dimension 7 Zukunft - MAP
      *
      *   Rev 2.0   May 30 2006 15:23:38   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00226!30.05.06!2.1 ! NEU                       ! KLEEMEIER*
270606* ASL00226!27.06.06!2.2 ! 10 PARTNER PRO BRIEF      ! KLEEMEIER*
      *                                                              *
      ****************************************************************
      *                                                              *
      *            INHALT    :  MAP-CTV-DATEI (BETREUER-MAP)         *
      *            DATEI     :  CV001201                             *
      *            MEMBER    :  DRCV0006                             *
      *            ORGANISAT.:  VSAM                                 *
      *            SATZLÄNGE :  643 BYTE                             *
      *            KEY       :  VBNR (1-7)                           *
      *                                                              *
      *--------------------------------------------------------------*
       01  DRCV0006-SATZ.
           05 DRCV0006-VBNR                     PIC 9(007).
           05 DRCV0006-ANSPR-VOM                PIC X(010).
           05 DRCV0006-STUFE-BEZ                PIC X(010).
           05 DRCV0006-MAP-BETRAG               PIC 9(009)V99.
           05 DRCV0006-DIR-AKT                  PIC 9(004).
           05 DRCV0006-VB-ANREDE                PIC X(001).
270606**** 05 DRCV0006-VB-NAME                  PIC X(060).
270606     05 DRCV0006-PART-01                  PIC X(060).
270606     05 DRCV0006-PART-02                  PIC X(060).
270606     05 DRCV0006-PART-03                  PIC X(060).
270606     05 DRCV0006-PART-04                  PIC X(060).
270606     05 DRCV0006-PART-05                  PIC X(060).
270606     05 DRCV0006-PART-06                  PIC X(060).
270606     05 DRCV0006-PART-07                  PIC X(060).
270606     05 DRCV0006-PART-08                  PIC X(060).
270606     05 DRCV0006-PART-09                  PIC X(060).
270606     05 DRCV0006-PART-10                  PIC X(060).
      *
