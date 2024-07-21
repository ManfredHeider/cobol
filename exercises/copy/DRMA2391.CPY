      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2391.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA2391.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 11:31:52   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Stornoneutrale Aufh.LV-Verträge/Eigen no.
      *
      *     SOURCE-NAME      :  DRMA2391
      *     DMR-NAME         :  DR-OV2391
      *
      *     AUTOR            :  BOOST, MANFRED
      *     GENERIERUNG      :  12.06.09/13:45
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA2391.
              07 VERTRAGS-ID            PIC X(15).
              07 VBNR                   PIC X(10).
              07 AE-41XXXX              PIC S9(10)V9   PACKED-DECIMAL.
              07 AE-110000              PIC S9(10)V9   PACKED-DECIMAL.
              07 BUCHDAT-41XXXX         PIC X(10).
              07 BUCHDAT-110000         PIC X(10).
              07 AVERM                  PIC X(10).
              07 AE-KZN                 PIC X.
      *
      **************************************************************
