      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0420.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0420.CPv  $
      *
      *   Rev 2.2   09 Nov 2006 15:05:14   t13ae34
      *AAT00006: DRAT0044 für AAN00655
      *
      *   Rev 2.1   Sep 26 2006 11:38:26   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   Sep 20 2006 11:57:48   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    KONSTANTEN FÜR     KTY = 221                              *
      *    ANTRAG AG8 PROVISIONSEMPFÄNGERSTRUKTUR                    *
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
           05 K-KNR-AN-AG8-DEFAULT       PIC S9(004) BINARY VALUE 0.
           05 K-KNR-AN-AG8-ABWEICHUNG    PIC S9(004) BINARY VALUE 1.
           05 K-KNR-AN-AG8-NACHTRAEGLICH PIC S9(004) BINARY VALUE 2.
