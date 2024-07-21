      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.01.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0019.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRCV0019.CPv  $
      *
      *   Rev 2.2   Jun 07 2016 17:55:22   t13ae35
      *FSL00267: Jira: Fehler bei Zusatzleistungen - CoBo 1.RB DVAG
      *
      *   Rev 2.1   Mar 14 2011 15:07:54   t13ex43
      *AAS00902: PGM - Parameter für CTV-Brief Rückbelastung
      *
      *   Rev 2.0   Jan 26 2011 13:04:30   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AAS00902!26.01.11!2.1 ! NEU                       ! KLEEMEIER *
070616* FSL00267!07.06.16!2.2 ! Anspruch_VOM hinzu        ! J.Hering *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *            INHALT    :  RÜCKBELASTUNG COLUMBUS-BONUS          *
      *            DATEI     :  CV009000                              *
      *            MEMBER    :  DRCV0019                              *
      *            ORGANISAT.:  VSAM                                  *
      *            SATZLÄNGE :  82 BYTE                               *
      *            KEY       :  VBNR-BETREUER, VBNR-PARTNER           *
      *                                                               *
      *****************************************************************
      *
       01  DRCV0019-SATZ.
           05 DRCV0019-KEY.
              08 DRCV0019-VBNR-BETR             PIC 9(007).
              08 DRCV0019-VBNR-PART             PIC 9(007).
           05 DRCV0019-WERT-VON                 PIC X(007).
           05 DRCV0019-WERT-BIS                 PIC X(007).
           05 DRCV0019-NAME-PART                PIC X(040).
           05 DRCV0019-AUSZAHL                  PIC 9(009)V99 COMP-3.
           05 DRCV0019-RUECKBEL                 PIC 9(009)V99 COMP-3.
           05 DRCV0019-GESELL                   PIC X(001).
           05 DRCV0019-ANREDE                   PIC X(001).
070616     05 DRCV0019-ANSPR-VOM                PIC X(007).
      *

