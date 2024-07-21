      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1145.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1145.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:13:28   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRMA1145.
000020        07 STEUER-BEREICH-BLAETTERN.
000030           09 USERID              PICTURE X(8).
000040           09 TRANSACTION-CODE    PICTURE X(8).
000050           09 OPERATION           PICTURE XX.
000060           09 GEFORDERTE-ANZAHL   PICTURE 9(4).
000070           09 GELIEFERTE-ANZAHL   PICTURE 9(4).
000080           09 LETZTE-POSITION     PICTURE 9(4).
000090           09 GESAMT-ANZAHL       PICTURE 9(4).
