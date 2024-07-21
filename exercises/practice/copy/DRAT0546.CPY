      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0546.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0546.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:05:52   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      * MEMBER DRAT0546
      *
000010     05 STATUS-BEREICH.
000020*    allgemeiner Status-Bereich
000030        07 DB2-INFO.
000040*    allgemeine Dump-Info von OZPs
000050           09 PROGRAMMNAME        PICTURE X(8).
000060*    PROGRAMMNAME
000070           09 VIEWNAME            PICTURE X(18).
000080*    VIEWNAME
000090           09 OPERATION           PICTURE XXX.
000100*    OPERATION
000110           09 SQL-RC              PICTURE S9(9).
000130*    SQL-RC
000140        07 RC                     PICTURE 99.
000150*    RC Returncode aus Unterprogrammen
000160        07 MELDUNGS-BESCHREIBUNG.
000170*    allgemeine Meldungs-Beschreibung
000180           09 MELDUNGS-NR         PICTURE 9(8).
000190*    Standard-Meldungsnummer
000200           09 SUB-VAR-1           PICTURE X(20).
000210*    Text-Langbezeichnung
000220           09 SUB-VAR-2           PICTURE X(20).
000230*    Text-Langbezeichnung
