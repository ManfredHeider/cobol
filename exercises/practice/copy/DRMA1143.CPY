      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1143.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1143.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:13:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRMA1143.
000020        07 BUFFER-EINGABE-BEREICH.
000030           09 PROGRAMMNAME        PICTURE X(8).
000040           09 LAENGE              PICTURE S9(4)  BINARY.
000050           09 MAXIMALE-ANZAHL     PICTURE S9(4)  BINARY.
