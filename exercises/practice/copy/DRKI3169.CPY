      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3169.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3169.CPv  $
      *
      *   Rev 2.1   Nov 22 2017 09:44:26   t13ae31
      *AKI01549: CKV Tarifdetails Copies
      *
      *   Rev 2.0   Sep 27 2017 08:34:32   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *                                                               *
      *    Copy-Member für die Bestandsdatenuebermittling mit         *
      *    CKV Tarifdetails G50                                       *
      *                                                               *
      *---------------------------------------------------------------*
      *    LOGBUCH                                                    *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01549!04.10.17!1.0 ! Neu                       ! HHU       *
      * --------------------------------------------------------------*
      *****************************************************************
           05  'XX'-VORSATZ.
      ** DTA VORSATZ / A-Satz wegen Kennzeichnung Gesamt- bzw.
      ** Aenderungsbestand. Kann nur 1 x je DTA vorhanden sein.
             10  'XX'-SAA                PIC X(01).
      ** 'A' Konstant
             10  'XX'-SAA-ABSENDER       PIC X(27).
      ** 'CENTRAL KRANKENVERS KOELN'
             10  'XX'-SAA-ERSTELLDATUM   PIC 9(08).
      ** ErstellDatum in der Form TTMMJJJJ
             10  'XX'-SAA-DTASTAND       PIC 9(08).
      ** Stichtag in der Form TTMMJJJJ
             10  'XX'-SAA-EMPFAENGER     PIC X(04).
      ** 'DVAG'
             10  'XX'-SAA-BESTAND-ART    PIC X(32).
             10  'XX'-SAA-RESERVE0       PIC X(520).
           05  'XX'-SATZ REDEFINES 'XX'-VORSATZ.
               10  'XX'-SAD                      PIC X(01).
      ** 'D' Konstant
 
      *  Tarifbezeichnung
               10  'XX'-TARIFBEZ                 PIC  X(08).
 
      *  Tarifname
               10  'XX'-TARIFNAME                PIC  X(030).
 
      *  Version
      *    MM.JJJJ
               10  'XX'-VERSION                  PIC  X(007).
 
      *  Tarifart
      *    A I V
               10  'XX'-TARIFART                 PIC  X(001).
 
      *  Tarifnummer (LFDNR)
               10  'XX'-TARIFLFDNR               PIC  X(2).
 
      *  Tariftext
               10  'XX'-TARIFTEXT                PIC  X(500).
      *  Reserve
               10  'XX'-RESERVE                  PIC  X(51).
 
           05  'XX'-NACHSATZ REDEFINES 'XX'-VORSATZ.
      ** DTA VORSATZ / A-Satz wegen Kennzeichnung Gesamt- bzw.
      ** Aenderungsbestand. Kann nur 1 x je DTA vorhanden sein.
             10  'XX'-SAE                PIC X(01).
      ** 'E' Konstant
             10  'XX'-SAE-SATZANZAHL     PIC 9(09).
      *  Reserve
             10  'XX'-SAE-RESERVE                  PIC  X(590).
 
      *****************************************************************
      *---------------------------------------------------------------*

