      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.12.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0840.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0840.CPv  $
      *
      *   Rev 2.3   Mar 07 2013 14:56:46   t13ex57
      *AVB01244: Projekt SEPA - Realisierung VB und AN - PMVB0037
      *
      *   Rev 2.2   Feb 28 2013 13:28:30   t13ex57
      *AVB01244: Projekt SEPA - Realisierung VB und AN - PMVB0037
      *
      *   Rev 2.1   15 May 2007 14:45:32   t13ex57
      *AVB00897: Verlängerung der Adressfelder im VB-Datenbestand 
      *
      *   Rev 2.0   07 Dec 2006 16:03:26   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AVB00897!07.12.06!    ! Neu                       ! U. Boll   *
      * AVB01244!25.02.13!    ! Projekt SEPA (IBAN/BIC)   ! U. Boll   *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Eingabedatei der Bankverbindungen aus VBI für die
      * Übernahme in Tabelle TBVB0021 BV
      *
      * Satzlänge 200
      *
      *
       01 DRVB0840.
           05  DRVB0840-VBNR                      PIC 9(07).
           05  DRVB0840-BV-TYP                    PIC 9(02).
           05  DRVB0840-KTO-NR                    PIC X(18).
           05  DRVB0840-BLZ                       PIC X(11).
           05  DRVB0840-BANKNAME                  PIC X(40).
           05  DRVB0840-KTO-INHABER               PIC X(40).
250213     05  DRVB0840-IBAN                      PIC X(34).
250213     05  DRVB0840-BIC                       PIC X(11).
           05  DRVB0840-LOESCH-KZ                 PIC X.
           05  FILLER                             PIC X(81).
