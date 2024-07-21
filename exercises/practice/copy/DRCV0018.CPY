      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0018.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRCV0018.CPv  $
      *
      *   Rev 2.1   Sep 29 2010 11:33:50   t13ex43
      *ASL00354: PGM - Parameter für CTV-Brief Columbus-GSH VM-Über
      *sicht
      *
      *   Rev 2.0   Aug 30 2010 10:48:44   T13EX43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00354!30.08.10!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *            INHALT    :  VM-ÜBERSICHT COLUMBUS-GELDSCHEINHEFTE *
      *            DATEI     :  CV009300                              *
      *            MEMBER    :  DRCV0018                              *
      *            ORGANISAT.:  VSAM                                  *
      *            SATZLÄNGE :  201 BYTE                              *
      *            KEY       :  VBNR-DL, VBNR-VBA, VBNR-VM            *
      *                                                               *
      *****************************************************************
      *
       01  DRCV0018-SATZ.
           05 DRCV0018-KEY.
              08 DRCV0018-VBNR-DL               PIC 9(007).
              08 DRCV0018-VBNR-VBA              PIC 9(007).
              08 DRCV0018-VBNR-VM               PIC 9(007).
           05 DRCV0018-NAME-DL                  PIC X(040).
           05 DRCV0018-DIR                      PIC 9(003).
           05 DRCV0018-NAME-VBA                 PIC X(040).
           05 DRCV0018-NAME-VM                  PIC X(040).
           05 DRCV0018-NAME-BETR                PIC X(040).
           05 DRCV0018-MONAT                    PIC X(007).
           05 DRCV0018-BIS-DATUM                PIC X(010).
      *
