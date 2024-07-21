      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.11.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3113.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3113.CPv  $
      *
      *   Rev 2.2   Jan 11 2017 12:18:58   t13ac17
      *AKI01466: Aufbereitung
      *
      *   Rev 2.1   Dec 07 2016 13:25:56   t13ac17
      *AKI01466: Aufbereitung
      *
      *   Rev 2.0   Nov 02 2016 15:28:08   t13ac17
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    Copy-Member für die Aktualisieung der Vertraege            *
      *    BKK Linde - G51                                            *
      *                                                               *
      *---------------------------------------------------------------*
      *    LOGBUCH                                                    *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01466!02.11.16! 2.1! Neuerstellung             ! Walbert   *
      *         !11.01.17! 2.2! Entfernen Änderungskennz. ! Walbert   *
      *         !        !    ! VBNR,                     !           *
      *         !        !    ! NEU: VERFREMDVERTRAGKZN   !           *
      * --------------------------------------------------------------*
      *         !        !    !                           !           *
      *****************************************************************
      *    Verarbeitungskennzeichen  I = Insert U = Update
           05 'XX'-VERARB-KZN                   PIC  X(001).
      *        Die Gesellschaftsnummer, sollte immer die 51 sein
           05 'XX'-GES                          PIC  9(002).
      *
      *    Die VB-Nummer des Abschlussvermittlers
           05 'XX'-VBNR                         PIC  9(007).
      *
      *    Vertragsnummern
           05 'XX'-VERNR                        PIC  X(015).
           05 'XX'-VERNR-INTERN                 PIC  9(010).
           05 'XX'-VERNR-INTERN-KZN             PIC  X(001).
      *
      *    Vertragszustand/Datum
           05 'XX'-VERTRAGSZUSTAND              PIC S9(02).
           05 'XX'-VERTRAGSZUSTAND-DATUM        PIC  X(10).
           05 'XX'-VERTRAGSZUSTAND-KZN          PIC  X(01).
      *
      *    Vertragsbeginn- und Endedatum im DB2 Format
           05 'XX'-VERTRAGSBEGINN               PIC  X(010).
           05 'XX'-VERTRAGSBEGINN-KZN           PIC  X(001).
           05 'XX'-VERTRAGSABLAUF               PIC  X(010).
           05 'XX'-VERTRAGSABLAUF-KZN           PIC  X(001).
      *
      *    Kennzeichen Eigenvemittelt Fremdvertrag
           05 'XX'-EIGENVERMKZN                 PIC  X(001).
           05 'XX'-FREMDVERTRAGKZN              PIC  X(001).
      *
      ******************************************************************
 

