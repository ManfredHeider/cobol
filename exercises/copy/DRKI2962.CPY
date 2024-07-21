      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2962.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2962.CPv  $
      *
      *   Rev 2.2   Sep 02 2015 15:10:16   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:46:14   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Jan 28 2015 11:39:30   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 9 6 2    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER G90 DVM Aktualisierung fuer VERTRAG          *
      *                                                                *
      *                                                                *
      *    SATZLAENGE  111 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      AENDERUNG                        !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *01.02.15!NEUERSTELLUNG                          !DDY      ! 01  *
      *--------!---------------------------------------!---------!-----*
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
      *    Verarbeitungskennzeichen  I = Insert U = Update
           05 'XX'-VERARB-KZ                    PIC  X(001).
      *        Die Gesellschaftsnummer, sollte immer die 90 sein
           05 'XX'-GES                          PIC  9(002).
      *
      *    Die VB-Nummer des Abschlussvermittlers
           05 'XX'-VB-NR                        PIC  9(007).
      *
      *    Die Vertragsnummer mit fuehrenden Nullen
           05 'XX'-VNR                          PIC  9(015).
           05 'XX'-VERTRAGSZUSTAND              PIC S9(02).
           05 'XX'-VERTRAGSZUSTAND-DATUM        PIC  X(10).
           05 'XX'-VERTRAGSZUSTAND-KZ           PIC  X(01).
      *        J WENN VERTRAGSZUSTAND GEAENDERT
      *
      *    Vertragsbeginn- und Endedatum im DB2 Format
           05 'XX'-VERTRAGSBEGINN               PIC  X(010).
           05 'XX'-VERTRAGSBEGINN-KZ            PIC  X(001).
           05 'XX'-VERTRAGSABLAUF               PIC  X(010).
           05 'XX'-VERTRAGSABLAUF-KZ            PIC  X(001).
      *
           05 'XX'-EIGENVERMKZ                  PIC  X(001).
           05 FILLER                            PIC  X(050).
      ******************************************************************
 

