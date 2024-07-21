      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0021.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRCV0021.CPv  $
      *
      *   Rev 2.2   Jun 08 2016 13:42:18   t13ae35
      *FSL00267: Jira: Fehler bei Zusatzleistungen CoBo 2.RB DVAG
      *
      *   Rev 2.1   Nov 18 2011 09:47:28   t13ex43
      *ASL00380: PGM - CTV-Briefe 2. Rückbelastung Columbusbonus
      *
      *   Rev 2.0   Nov 15 2011 09:05:08   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00380!15.11.11!2.1 ! NEU                       ! KLEEMEIER *
080616* FSL00267!08.06.16!2.2 ! Anspruch_VOM hinzu        ! J.Hering *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *            INHALT    :  2. RÜCKBELASTUNG COLUMBUS-BONUS       *
      *            DATEI     :  CV010300                              *
      *            MEMBER    :  DRCV0021                              *
      *            ORGANISAT.:  VSAM                                  *
      *            SATZLÄNGE :  96 BYTE                               *
      *            KEY       :  VBNR-BETREUER, VBNR-PARTNER           *
      *                                                               *
      *****************************************************************
      *
       01  DRCV0021-SATZ.
           05 DRCV0021-KEY.
              08 DRCV0021-VBNR-BETR             PIC 9(007).
              08 DRCV0021-VBNR-PART             PIC 9(007).
           05 DRCV0021-WERT-VON                 PIC X(007).
           05 DRCV0021-WERT-BIS                 PIC X(007).
           05 DRCV0021-WERT-BIS-2               PIC X(007).
           05 DRCV0021-NAME-PART                PIC X(040).
           05 DRCV0021-AUSZAHL                  PIC 9(009)V99 COMP-3.
           05 DRCV0021-RUECKBEL                 PIC 9(009)V99 COMP-3.
           05 DRCV0021-GESELL                   PIC X(001).
           05 DRCV0021-ANREDE                   PIC X(001).
080616     05 DRCV0021-ANSPR-VOM                PIC X(007).
      *

