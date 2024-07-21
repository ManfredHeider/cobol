      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.11.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0017.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAS0017.CPv  $
      *
      *   Rev 2.2   May 29 2008 09:06:04   t13ex57
      *AVB00875: Verlängerung der Adressfelder im VB-Datenbestand -
      * UBO
      *
      *   Rev 2.1   Dec 08 2004 15:53:34   t13ex57
      *AAS00500: Akt. Handynummer und Adresse im VB-Portal
      *
      *   Rev 2.0   Nov 18 2004 10:41:10   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAS00500!18.11.04!2.1 ! Neu                       ! U. Boll   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Steuernummer zur Übernahme in Tabelle MAAN
      *
      *
      *
      *
      *
       01 DRAS0017.
           05  DRAS0017-VBNR                      PIC 9(07).
           05  DRAS0017-STEUERNUMMER              PIC X(12).
           05  FILLER                             PIC X(81).

      *----------- Ende DRAS0017 ---------------------------
