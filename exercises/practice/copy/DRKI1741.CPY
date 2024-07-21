      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.12.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1741.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1741.CPv  $
      *
      *   Rev 2.1   Feb 08 2005 09:16:50   t13ex27
      *AKI00457: Korrektur Kundenstatus
      *
      *   Rev 2.0   Dec 15 2004 14:14:38   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00457!03.12.04!2.1 ! NEU                       ! IFR      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *                                                              *
      *   Protokoll-Datei für die Kundenstatus-Korrektur             *
      *                                                              *
      ****************************************************************
       01  PROT-SATZ.
           05 PROT-KIKUNDENNUMMER                PIC 9(010).
           05 PROT-KIKUNDENSTATUSID-ALT          PIC X(001).
           05 PROT-KIKUNDENSTATUSID-NEU          PIC X(001).
           05 PROT-TAGESDATUM-EUR                PIC X(010).
           05 PROT-KIPERSONENID                  PIC X(013).
           05 PROT-KIPERSONENID-D                PIC X(026).

