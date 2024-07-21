      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3289.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3289.CPv  $
      *
      *   Rev 2.1   Jun 11 2018 14:32:40   t13ex27
      *AKI01570: FW - Generali-Partnergesellschaften-E/A/A-Kunden
      *
      *   Rev 2.0   Feb 22 2018 13:31:46   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01570!21.02.18!2.1 ! neu                       ! IFR
      *         !        !    !                           !
      *         !        !    !                           !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *
         05  'P'-KEY.

           10  'P'-GES                     PIC  9(002).
           10  'P'-VNR                     PIC  9(015).
           10  'P'-EZPNR                   PIC  9(004).
           10  'P'-LFDNR                   PIC  9(004).

         05  'P'-DATEN.
           10  'P'-NNAME                   PIC  X(040).
           10  'P'-AEND-NNAME-KZ           PIC  X(001).

           10  'P'-VNAME                   PIC  X(030).
           10  'P'-AEND-VNAME-KZ           PIC  X(001).

           10  'P'-GEBDAT                  PIC  X(010).
           10  'P'-AEND-GEBDAT-KZ          PIC  X(001).
      *
      *    Die Anrede wird nur für die Anonymisierung benötigt.
      *
           10  'P'-ANREDE                  PIC S9(002).
      *
           05  'P'-AKTUAL-KZ               PIC  X(001).
      *----------------------------------------------------------------*

