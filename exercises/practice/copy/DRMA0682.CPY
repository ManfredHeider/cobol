      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0682.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA0682.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:01:52   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Direktionendatei DVAG
      *
      *     SOURCE-NAME      :  DRMA0682
      *     DMR-NAME         :  DR-OV0682
      *
      *     AUTOR            :  BOOST, MANFRED
      *     GENERIERUNG      :  29.05.07/14:49
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA0682.
              07 DIR                    PIC 999.
              07 VBNR                   PIC 9(7).
              07 ZUNAME                 PIC X(40).
              07 VORNAME                PIC X(20).
              07 FILLER                 OCCURS 10 TIMES
                                        PIC X.
      *
      **************************************************************
