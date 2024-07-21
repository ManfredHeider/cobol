      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1873.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1873.CPv  $
      *
      *   Rev 2.2   Jun 12 2013 14:58:44   t13ex27
      *FKI01394: Überarbeitung Ticketfehler
      *
      *   Rev 2.1   27 Mar 2007 10:24:20   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - Programme
      *
      *   Rev 2.0   Mar 09 2006 10:14:22   t13ex27
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
      * FKI01394!25.03.13! 2.2! Attribut geändert         !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRKI1873.
           05  PI-DATEN.
               10  PI-DRKI1873-OTTICKETID            PIC 9(018) COMP-3.
               10  PI-DRKI1873-OTERSTELLT            PIC X(026).
250313*        10  PI-DRKI1873-OTERSTELLER           PIC X(030).
250313         10  PI-DRKI1873-OTERSTELLTVON         PIC X(030).
               10  PI-DRKI1873-OTHOSTKUNDENID        PIC X(013).
               10  PI-DRKI1873-OTAENDGRUND-KTY       PIC S9(4)  COMP.
               10  PI-DRKI1873-OTAENDGRUND-KNR       PIC S9(4)  COMP.


       01  PO-DRKI1873.
           05  PO-DRKI1873-RC               PIC  X(003).
               88 PO-DRKI1873-RC-OK              VALUE 'OK '.
               88 PO-DRKI1873-RC-NOK             VALUE 'NOK'.
           05  PO-DATEN.
               10  PO-DRKI1873-OTFEHLER-KNR       PIC S9(4)  COMP.


