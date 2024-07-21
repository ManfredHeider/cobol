      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2376.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA2376.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 11:31:42   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Regionalisierung pro Direktion
      *
      *     SOURCE-NAME      :  DRMA2376
      *     DMR-NAME         :  DR-OV2376
      *
      *     AUTOR            :  BOOST, MANFRED
      *     GENERIERUNG      :  06.08.09/13:43
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA2376.
              07 DIR-NR                 PIC S9(4)      BINARY.
              07 KTY-REGION             PIC S9(4)      BINARY.
              07 KNR-REGION             PIC S9(4)      BINARY.
              07 LANGE-BEZEICHNUNG      PIC X(20).
              07 KURZBEZEICHNUNG        PIC X(4).
              07 10-STELLIGE-BEZEICHNUNG
                                        PIC X(10).
              07 KTY-REGION-VTRB        PIC S9(4)      BINARY.
              07 KNR-REGION-VTRB        PIC S9(4)      BINARY.
              07 LANGE-BEZEICHNUNG-VTRB PIC X(20).
              07 KURZBEZEICHNUNG-VTRB   PIC X(4).
              07 10-STELLIGE-BEZEICHNUNG-VTRB
                                        PIC X(10).
      *
      **************************************************************
