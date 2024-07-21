      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.05.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1890.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1890.CPv  $
      *
      *   Rev 2.2   01 Jun 2007 09:53:24   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB KUZUSAFU im 
      *ID/AD und Vertragsbereinigung
      *
      *   Rev 2.1   27 Mar 2007 10:25:38   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - Programme
      *
      *   Rev 2.0   May 10 2006 16:16:18   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00557!10.03.06! 2.1! NEU                       !I. Frenzel*
      *         !20.04.07! 2.2! Anzahl Betreuer für       !          *
      *         !        !    ! KUZUSAFU im ID            !I. Frenzel*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRKI1890.
           05  PI-DATEN.
               10  PI-DRKI1890-OTTICKETID             PIC 9(018) COMP-3.
               10  PI-DRKI1890-OTAENDGRUND-KNR        PIC S9(004) COMP.
               10  PI-DRKI1890-OTERSTELLT             PIC X(026).
               10  PI-DRKI1890-OTERSTELLTVON          PIC X(030).
               10  PI-DRKI1890-OTHOSTKUNDENID         PIC X(013).

       01  PO-DRKI1890.
           05  PO-DRKI1890-RC               PIC  X(003).
               88 PO-DRKI1890-RC-OK              VALUE 'OK '.
               88 PO-DRKI1890-RC-NOK             VALUE 'NOK'.
           05  PO-DATEN.
               10  PO-DRKI1890-OTFEHLER-KNR        PIC S9(4)  COMP.
200407         10  PO-DRKI1890-KIPERSONENID        PIC X(013).
200407         10  PO-DRKI1890-KIANZAHLBETREUER    PIC S9(4)  COMP.


