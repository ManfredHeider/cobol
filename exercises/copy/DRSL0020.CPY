      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.10.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0020.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRSL0020.CPv  $
      *
      *   Rev 2.1   Nov 20 2008 10:08:30   t13ex43
      *ASL00309: PGM - CTV-Briefe Weiterführungsangebot
      *
      *   Rev 2.0   Oct 07 2008 16:27:20   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME     *
      * --------+--------+----+----------------------------+----------*
      * ASL00309!07.10.08!2.1 ! NEU                        ! KLEEMEIER*
      *         !        !    !                            !          *
      *         !        !    !                            !          *
      *                                                               *
      *****************************************************************
      *                                                               *
      *            INHALT    :  VSW 2008 - WEITERFÜHRUNGSANGEBOT      *
      *            DATEI     :  CV004600                              *
      *            MEMBER    :  DRSL0020                              *
      *            ORGANISAT.:  VSAM                                  *
      *            SATZLÄNGE :  70 BYTE                               *
      *            KEY       :  VBNR (1-7)                            *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRSL0020-SATZ.
           05 DRSL0020-VBNR                     PIC 9(007).
           05 DRSL0020-STICHTAG                 PIC X(010).
           05 DRSL0020-BIS-DATUM                PIC X(010).
           05 DRSL0020-LV-VSNR                  PIC 9(010).
           05 DRSL0020-LV-BEITRAG               PIC 9(006),99.
           05 DRSL0020-RV-VSNR                  PIC 9(010).
           05 DRSL0020-RV-BEITRAG               PIC 9(006),99.
           05 DRSL0020-VSWKZ-01                 PIC X(001).
           05 DRSL0020-VSWKZ-02                 PIC X(001).
           05 DRSL0020-VSWKZ-03                 PIC X(001).
           05 DRSL0020-LEIST-KZ                 PIC X(001).
           05 DRSL0020-GESELL                   PIC 9(001).
      *****
      *****   ENDE DRSL0020
      *****
