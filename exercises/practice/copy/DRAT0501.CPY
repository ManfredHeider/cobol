      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0501.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0501.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 14:58:50   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     FlexN Eingabe-Bereich PMKG2740 - Buchungssatz
      *
      *     SOURCE-NAME      :  DRAT0501
      *     DMR-NAME         :  DG-KG2736
      *
      *     AUTOR            :  POHL, SEBASTIAN
      *     GENERIERUNG      :  15.04.08/11:02
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRAT0501.
              07 MANDANT                PIC XX.
              07 PG-KZ                  PIC XX.
              07 HERK-BASIS-SCHL        PIC XXX.
              07 RABATTSPARTE           PIC X(10).
              07 PRODUKTGRUPPE          PIC 9(4).
              07 EINZELPRODUKT          PIC 9(4).
              07 VSNR                   PIC X(15).
              07 AKZ                    PIC X(15).
              07 QNR                    PIC S9(15)     PACKED-DECIMAL.
              07 WIRKSAM-AB             PIC X(10).
              07 VG                     PIC XX.
              07 AD-STELLE              PIC S9(7)      BINARY.
              07 PIF                    PIC X(15).
              07 NAME                   PIC X(40).
              07 VORNAME                PIC X(40).
              07 PRODUKT                PIC 9(4).
              07 PRODUKTTYP             PIC 9(4).
              07 VERTRAGSSPARTE         PIC X(10).
              07 GEVO                   PIC S9(9)      BINARY.
              07 GEVO-ID-TEIL-1         PIC S9(5)      BINARY.
              07 GEVO-ID-TEIL-2         PIC S9(5)      BINARY.
              07 TIMESTAMP-IN-K         PIC X(26).
              07 AKTIONS-KZN            PIC X(5).
              07 BETRAG-1               PIC S9(11)V99  PACKED-DECIMAL.
              07 ANZ-EINHEITEN          PIC S9(11)V9   PACKED-DECIMAL.
              07 BETRAG-2               PIC S9(11)V99  PACKED-DECIMAL.
              07 BEWSCHL                PIC X(6).
              07 PDPT-ID                PIC X(15).
              07 KONTO-GES-KZ           PIC X.
              07 INFO-KZ                PIC X(5).
              07 FILLER                 PIC X(45).
      *
      **************************************************************
