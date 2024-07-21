      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1995.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1995.CPv  $
      *
      *   Rev 2.1   27 Mar 2007 10:36:48   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - Programme
      *
      *   Rev 2.0   Nov 09 2006 14:44:26   t13ex27
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
       01  PI-DRKI1995.
           05  PI-DATEN.
               10  PI-DRKI1995-OTTICKETID            PIC 9(018).
               10  PI-DRKI1995-OTHOSTKUNDENID        PIC X(013).

       01  PO-DRKI1995.
           05  PO-DRKI1995-RC               PIC  X(003).
               88 PO-DRKI1995-RC-OK              VALUE 'OK '.
               88 PO-DRKI1995-RC-NOK             VALUE 'NOK'.
               88 PO-DRKI1995-RC-NFD             VALUE 'NFD'.
           05  PO-DATEN.
               10  PO-DRKI1995-OTFEHLER-KNR       PIC S9(4)  COMP.

