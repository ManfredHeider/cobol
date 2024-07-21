      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.05.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1902.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1902.CPv  $
      *
      *   Rev 2.1   27 Mar 2007 10:28:08   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - Programme
      *
      *   Rev 2.0   May 17 2006 15:50:20   t13ex27
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
       01  PI-DRKI1902.
           05  PI-DATEN.
               10  PI-DRKI1902-OTTICKETID             PIC 9(018).
               10  PI-DRKI1902-OTERSTELLER            PIC X(030).
               10  PI-DRKI1902-OTHOSTKUNDENID         PIC X(013).

       01  PO-DRKI1902.
           05  PO-DRKI1902-RC               PIC  X(003).
               88 PO-DRKI1902-RC-OK              VALUE 'OK '.
               88 PO-DRKI1902-RC-NOK             VALUE 'NOK'.
           05  PO-DATEN.
               10  PO-DRKI1902-OTFEHLER-KNR        PIC S9(4)  COMP.

