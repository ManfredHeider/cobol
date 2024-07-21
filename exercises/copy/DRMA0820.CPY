      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0820.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA0820.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:04:52   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRMA0820.
000020        07 BUCHDAT-JJJJ           PICTURE 9(4).
000030        07 BUCHDAT-MM             PICTURE 99.
000040        07 GESKZ                  PICTURE 99.
000050        07 ANZAHL                 PICTURE S9(9)  PACKED-DECIMAL.
000060        07 FILLER                 PICTURE X(7).
