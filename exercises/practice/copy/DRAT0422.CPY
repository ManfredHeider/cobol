      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0422.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0422.CPv  $
      *
      *   Rev 2.2   Jun 26 2009 08:26:50   t13ae34
      *AAN00895: DWS Darstellung der Einheiten EG
      *
      *   Rev 2.1   09 Nov 2006 15:12:08   t13ae34
      *AAT00006: DRAT0044 für AAN00655
      *
      *   Rev 2.0   12 Oct 2006 12:09:44   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    KONSTANTEN FÜR     KTY = 224                              *
      *    ANTRAG PRODUKT- ODER TARIFEINGABE ERFORDERLICH            *
      *    HINWEIS:                                                  *
      *    WERTE BEZIEHEN SICH AUF FELD KNR-BEZ-10                   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!12.10.06! 2.1 ! NEUERSTELLUNG            ! HEIDER   *
      * AAN00895!09.06.09! 2.2 ! KEINE ODER PROD          ! HEIDER   *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-AN-PRODUKT           PIC  X(010) VALUE
              'PRODUKT   '.
           05 K-KNR-AN-TARIF             PIC  X(010) VALUE
              'TARIF     '.
           05 K-KNR-AN-PROD-ODER-TARIF   PIC  X(010) VALUE
              'PROD-TARIF'.
090609     05 K-KNR-AN-KEINE-ODER-PROD   PIC  X(010) VALUE
090609        'KEINE-PROD'.
