      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3213.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA3213.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 11:42:32   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     normierter Arbeitssatz Stornobegleitliste - neu
      *
      *     SOURCE-NAME      :  DRMA3213
      *     DMR-NAME         :  DR-OV3213
      *
      *     AUTOR            :  WALBERT, GABY
      *     GENERIERUNG      :  14.11.06/08:21
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA3213.
              07 VERTRAGSNR-EXTERN      PIC X(15).
              07 BETREUER               PIC X(10).
              07 NAME-BETR              PIC X(25).
              07 VORN-BETR              PIC X(20).
              07 STRUK-DIR-BETR         PIC 999.
              07 ANREDE-BETR            PIC 9.
              07 PROVKZ-BETR            PIC X.
              07 AVERM                  PIC X(10).
              07 NAME-VB                PIC X(25).
              07 VORN-VB                PIC X(20).
              07 STRUK-DIR-VB           PIC 999.
              07 ANREDE-VB              PIC 9.
              07 PROVKZ-VB              PIC X.
              07 GESKZ                  PIC 99.
              07 PRODSCHL               PIC 999.
              07 TARIFBEZ               PIC X(10).
              07 NAME-KD                PIC X(25).
              07 VORN-KD                PIC X(20).
              07 STR-KD                 PIC X(25).
              07 PLZ                    PIC X(5).
              07 ORT                    PIC X(40).
              07 BEWSCHL-ART            PIC X(6).
              07 BEWSCHL-LANG           PIC X(15).
              07 INKST-DATUM.
                 09 INK-JRH             PIC 99.
                 09 INKST.
                    11 INK-JHR          PIC 99.
                    11 INK-MON          PIC 99.
                 09 INK-TAG             PIC 99.
              07 PROVBETRAG-BETR        PIC S9(11)V99  PACKED-DECIMAL.
              07 PROVBETRAG-AVERM       PIC S9(11)V99  PACKED-DECIMAL.
              07 PROVEINH               PIC S9(9)V9    PACKED-DECIMAL.
              07 BUCHDAT                PIC X(10).
              07 FILLER                 PIC X(8).
      *
      **************************************************************
