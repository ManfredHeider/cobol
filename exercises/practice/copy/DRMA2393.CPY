      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2393.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA2393.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 11:32:04   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Stornoneutrale Aufh.LV-Verträge/Eigen no. (Regionen)
      *
      *     SOURCE-NAME      :  DRMA2393
      *     DMR-NAME         :  DR-OV2393
      *
      *     AUTOR            :  BOOST, MANFRED
      *     GENERIERUNG      :  16.06.09/10:43
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA2393.
              07 VERTRAGS-ID            PIC X(15).
              07 VBNR                   PIC X(10).
              07 AE-41XXXX              PIC S9(10)V9   PACKED-DECIMAL.
              07 AE-110000              PIC S9(10)V9   PACKED-DECIMAL.
              07 BUCHDAT-41XXXX         PIC X(10).
              07 BUCHDAT-110000         PIC X(10).
              07 AVERM                  PIC X(10).
              07 AE-KZN                 PIC X.
              07 KNR-REGION             PIC 9(8).
              07 KNR-REGION-BEZ         PIC X(20).
      *
      **************************************************************
