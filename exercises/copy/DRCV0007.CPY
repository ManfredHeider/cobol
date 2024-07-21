      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.06.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0007.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRCV0007.CPv  $
      *
      *   Rev 2.1   Jun 08 2006 10:25:26   t13ex43
      *AVB00850: PGM2 - Autom. Rückstufung von VBA-K zu VBA-S
      *
      *   Rev 2.0   Jun 06 2006 16:26:26   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AVB00850!06.06.06!2.1 ! NEU                       ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
      *                                                              *
      *            INHALT    :  AUTOMATISCHE RÜCKSTUFUNG             *
      *            DATEI     :  CV002400                             *
      *            MEMBER    :  DRCV0007                             *
      *            ORGANISAT.:  VSAM                                 *
      *            SATZLÄNGE :  27 BYTE                              *
      *            KEY       :  VBNR (1-7)                           *
      *                                                              *
      *--------------------------------------------------------------*
       01  DRCV0007-SATZ.
           05 DRCV0007-VBNR                     PIC 9(007).
           05 DRCV0007-VERTR-VOM                PIC X(010).
           05 DRCV0007-RUECKSTF-ZUM             PIC X(010).
      *
