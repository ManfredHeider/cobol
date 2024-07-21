      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0010.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRCV0010.CPv  $
      *
      *   Rev 2.1   Sep 14 2006 10:57:42   t13ex43
      *ACV00085: PGM - Brief an Konzessionsinhaber (§34c ) ohne BDV
      *-Mitgliedschaft
      *
      *   Rev 2.0   Sep 04 2006 10:54:48   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ACV00085!04.09.06!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *            INHALT    :  §34C OHNE BDV-MITGLIEDSCHAFT          *
      *            DATEI     :  CV002900                              *
      *            MEMBER    :  DRCV0010                              *
      *            ORGANISAT.:  VSAM                                  *
      *            SATZLÄNGE :  17 BYTE                               *
      *            KEY       :  VBNR (1-7)                            *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRCV0010-SATZ.
           05 DRCV0010-VBNR                     PIC 9(007).
           05 DRCV0010-DAT34C                   PIC X(010).
      *
