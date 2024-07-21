      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.10.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3170.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3170.CPv  $
      *
      *   Rev 2.1   Nov 22 2017 09:44:38   t13ae31
      *AKI01549: CKV Tarifdetails Copies
      *
      *   Rev 2.0   Oct 06 2017 09:22:28   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *                                                               *
      *    Copy-Member für die Bestandsdatenuebermittling mit         *
      *    CKV Tarifdetails G50 Ausgabe für die Aktualisierung        *
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
           05  'XX'-DATEN.
 
      *  Tarifkeylfdnr
               10  'XX'-TARIFKEY                 PIC  9(05).
 
      *  Tarifbezeichnung
               10  'XX'-TARIFBEZ                 PIC  X(08).
 
      *  Tarifname
               10  'XX'-TARIFNAME                PIC  X(030).
 
      *  Version
      *    MM.JJJJ
               10  'XX'-VERSION                  PIC  X(007).
 
      *  Tarifart
      *      l v
               10  'XX'-TARIFART                 PIC  X(001).
 
      *  Tarifnummer (LFDNR)
               10  'XX'-TARIFLFDNR               PIC  X(2).
 
      *  Tariftext
               10  'XX'-TARIFTEXT                PIC  X(500).
      *  Reserve
               10  'XX'-LEER                     PIC  X(47).
 
      *****************************************************************
      *---------------------------------------------------------------*

