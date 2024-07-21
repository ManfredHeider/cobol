      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0548.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0548.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:06:18   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      * MEMBER DRAT0548
      *
000010     05 MELDUNG.
000020*    Meldung
000030        07 MELDUNGS-TEXT          PICTURE X(80).
000040*    Text-Langbezeichnung
000050        07 MELDUNGS-TYP           PICTURE X.
000060*    Meldungstyp-Kennzeichen
000070        07 MELDUNGS-ATTRIBUT      PICTURE S9(4).
000090*    Attribut-Byte
