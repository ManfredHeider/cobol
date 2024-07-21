      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1978.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1978.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:39:44   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Datensatz: Stornoneutralstellung aufbereitet
      *
      *     SOURCE-NAME      :  DRMA1978
      *     DMR-NAME         :  DR-OV1978
      *
      *     AUTOR            :  WALBERT, GABY
      *     GENERIERUNG      :  16.06.09/10:06
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA1978.
              07 VBNR                   PIC 9(7).
              07 GESKZ                  PIC 99.
              07 VERTRAGS-ID            PIC X(10).
              07 VN                     PIC X(20).
              07 BUCHDAT-AUFHEBUNG      PIC X(6).
              07 BUCHDAT-STORNO         PIC X(6).
              07 AE-STORNO              PIC S9(10)V9   PACKED-DECIMAL.
              07 BEWART                 PIC 9(6).
              07 BEWART-LANG            PIC X(20).
              07 BEARB-DATUM-STORNO     PIC X(10).
      *
      **************************************************************
