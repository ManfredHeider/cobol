      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1882.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1882.CPv  $
      *
      *   Rev 2.1   27 Mar 2007 10:24:58   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - Programme
      *
      *   Rev 2.0   Mar 31 2006 12:33:50   t13ex27
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
       01  PI-DRKI1882.
           05  PI-DATEN.
               10  PI-DRKI1882-OTTICKETID             PIC 9(018) COMP-3.
               10  PI-DRKI1882-OTERSTELLT             PIC X(026).
               10  PI-DRKI1882-OTERSTELLER            PIC X(030).
               10  PI-DRKI1882-OTHOSTKUNDENID         PIC X(013).
               10  PI-DRKI1882-OTHOSTVERTRAGID        PIC X(013).
               10  PI-DRKI1882-OTAENDGRUND-KTY        PIC S9(4)  COMP.
               10  PI-DRKI1882-OTAENDGRUND-KNR        PIC S9(4)  COMP.


       01  PO-DRKI1882.
           05  PO-DRKI1882-RC               PIC  X(003).
               88 PO-DRKI1882-RC-OK              VALUE 'OK '.
               88 PO-DRKI1882-RC-NOK             VALUE 'NOK'.
           05  PO-DATEN.
               10  PO-DRKI1882-OTFEHLER-KNR        PIC S9(4)  COMP.

