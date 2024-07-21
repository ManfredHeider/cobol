      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0004.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0004.CPv  $
      *
      *   Rev 2.1   Feb 27 2007 12:54:10   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - OZPs
      *
      *   Rev 2.0   Mar 17 2006 16:29:48   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00557!10.03.06! 2.0! NEU                       !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DROK0004.

           05  PI-DROK0004-HNAKUNDENID              PIC  X(013).

       01  PO-DROK0004.
           05  PO-DROK0004-RC                          PIC  X(003).
               88 PO-DROK0004-RC-OK                        VALUE 'OK '.
               88 PO-DROK0004-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DROK0004-DATEN.

             10 PO-DROK0004-HNALFDNR                PIC S9(4)  COMP.
      ********************** Datenende *********************************
