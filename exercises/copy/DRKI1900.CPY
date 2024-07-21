      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.05.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1900.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1900.CPv  $
      *
      *   Rev 2.1   27 Mar 2007 10:27:18   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - Programme
      *
      *   Rev 2.0   May 17 2006 15:49:46   t13ex27
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
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRKI1900.
           05  PI-DATEN.
               10  PI-DRKI1900-OTTICKETID            PIC  9(018) COMP-3.
               10  PI-DRKI1900-OTERSTELLT            PIC  X(026).
               10  PI-DRKI1900-OTERSTELLTVON         PIC  X(030).
               10  PI-DRKI1900-OTHOSTKUNDENID        PIC  X(013).
               10  PI-DRKI1900-OTHOSTVERTRAGID       PIC  X(013).
               10  PI-DRKI1900-OTAENDGRUND-KNR       PIC S9(004) COMP.

       01  PO-DRKI1900.
           05  PO-DRKI1900-RC               PIC  X(003).
               88 PO-DRKI1900-RC-OK              VALUE 'OK '.
               88 PO-DRKI1900-RC-NOK             VALUE 'NOK'.
           05  PO-DATEN.
               10  PO-DRKI1900-OTFEHLER-KNR        PIC S9(4)  COMP.

