      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0502.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0502.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 14:59:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     LVV-SATZ - KL_LVV_VERTRAG
      *     RDz
      *     SOURCE-NAME      :  DRAT0502
      *     DMR-NAME         :  DG-KL6627
      *
      *     AUTOR            :  BURKARD, CLAUDIA
      *     GENERIERUNG      :  01.06.2012/11:30:04
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 LV0-OM690101.
              07 LV0-LV0KEY.
                 09 LV0-PG-KZ           PIC XX.
                 09 LV0-VERMGES         PIC 99.
                 09 LV0-ORDBEGR.
                    11 LV0-SPARTE       PIC 9.
                    11 LV0-VS-NUMMER    PIC 9(7).
                    11 LV0-VS-UNUMMER   PIC 999.
                 09 LV0-TARIFKZ         PIC 99.
                 09 LV0-PROVSCHICHTNR   PIC 999.
              07 LV0-PIF                PIC X(15).
              07 LV0-VN                 PIC X(20).
              07 LV0-UMW-BEARBTERM.
                 09 LV0-UMW-BEARB-DATUM PIC 9(8).
                 09 LV0-UMW-BEARB-ZEITNR
                                        PIC 9(7).
              07 LV0-ANZ-UZUGAENGE      PIC S999       PACKED-DECIMAL.
              07 LV0-ANZ-UABGAENGE      PIC S999       PACKED-DECIMAL.
              07 LV0-KZ-ZUABGANG        PIC 9.
              07 LV0-KZ-UMWAND          PIC 9.
              07 LV0-EINLOES            PIC X.
              07 LV0-AVERM-VBAS-KZN     PIC X.
              07 LV0-AVERM-KDBETR-KZN   PIC X.
              07 LV0-AEKZ               PIC X.
              07 LV0-POL-DATUM.
                 09 LV0-POL-JAHR        PIC 9(4).
                 09 LV0-POL-JAHR-R REDEFINES LV0-POL-JAHR.
                    11 LV0-POL-JRH      PIC 99.
                    11 LV0-POL-JHR      PIC 99.
                 09 LV0-POL-MON         PIC 99.
                 09 LV0-POL-TAG         PIC 99.
              07 LV0-SONDERBER          PIC XX.
              07 LV0-URSPRPOLDAT        PIC 9(8).
              07 LV0-VERFUEGART         PIC 999.
              07 FILLER                 PIC XXX.
              07 FILLER                 PIC X(10).
              07 LV0-HT-TARIFSCHL-GRP   PIC S999       PACKED-DECIMAL.
              07 LV0-HT-TARIFSCHL       PIC S9(5)      PACKED-DECIMAL.
              07 LV0-STRUK-ANF-KZ       PIC X.
      *
      **************************************************************
