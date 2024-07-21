      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0533.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0533.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:03:16   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRAT0533.
000020*    Ausgabeparameter UPAT0063
000030        07 DATUM.
000040*    STANDARD: Datum
000050           09 TAG                 PIC 9(2).
000060*    Tagesangabe eines Datum
000070           09 FILLER              PIC X.
000080*    Filler-1
000090           09 MONAT               PIC 9(2).
000100*    Monatsangabe eines Datum
000110           09 FILLER              PIC X.
000120*    Filler-1
000130           09 JAHR                PIC 9(4).
000140*    Jahresangabe eines Datum
000150        07 RC                     PIC S9(2).
000160*    RC Returncode aus Unterprogrammen
000170        07 MELDUNGS-NR            PIC S9(8).
000180*    Meldungsnummer
