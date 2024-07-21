      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.11.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0016.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAS0016.CPv  $
      *
      *   Rev 2.2   May 29 2008 09:05:46   t13ex57
      *AVB00875: Verlängerung der Adressfelder im VB-Datenbestand -
      * UBO
      *
      *   Rev 2.1   Dec 08 2004 15:53:26   t13ex57
      *AAS00500: Akt. Handynummer und Adresse im VB-Portal
      *
      *   Rev 2.0   Nov 18 2004 10:40:56   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAS00500!18.11.04!2.1 ! Neu                       ! U. Boll   *
080207* AVB00875!08.02.07!2.2 ! ERWEITERUNG ORT/STRASSE   ! U. Boll   *
      *         !        !    ! TEL/FAX/HANDY             !           *
      *****************************************************************
      *
      * geänderte Handynummer zur Übernahme in Tabelle MAAN
      *
      *
      *
      *
      *
       01 DRAS0016.
           05  DRAS0016-VBNR                      PIC 9(07).
080207*    05  DRAS0016-HANDYNR                   PIC X(14).
080207*    05  FILLER                             PIC X(79).
080207     05  DRAS0016-HANDYNR1                  PIC X(26).
080207     05  DRAS0016-HANDYNR2                  PIC X(26).
080207     05  DRAS0016-NOTFALL-KZ                PIC X(01).
080207     05  FILLER                             PIC X(40).
      *----------- Ende DRAS0016 ---------------------------
