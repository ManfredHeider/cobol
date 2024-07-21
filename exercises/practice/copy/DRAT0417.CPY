      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0417.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0417.CPv  $
      *
      *   Rev 2.2   09 Nov 2006 14:55:56   t13ae34
      *AAT00006: DRAT0044 für AAN00655
      *
      *   Rev 2.1   Sep 26 2006 11:26:22   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   Sep 20 2006 11:39:22   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    KONSTANTEN FÜR     KTY = 218                              *
      *    ANTRAG STATUS                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!20.09.06! 2.1 ! NEUERSTELLUNG            ! HEIDER   *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-AN-STATUS-VORLAEUFIG PIC S9(004) BINARY VALUE 1.
           05 K-KNR-AN-STATUS-ERFASST    PIC S9(004) BINARY VALUE 2.
           05 K-KNR-AN-STATUS-PROV-WARTE PIC S9(004) BINARY VALUE 3.
           05 K-KNR-AN-STATUS-STORNIERT  PIC S9(004) BINARY VALUE 4.
           05 K-KNR-AN-STATUS-ABGERECH   PIC S9(004) BINARY VALUE 5.
