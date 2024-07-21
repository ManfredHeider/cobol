      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.04.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0017.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0017.CPv  $
      *
      *   Rev 2.1   Feb 27 2007 12:55:46   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - OZPs
      *
      *   Rev 2.0   Apr 06 2006 15:40:10   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
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
       01  PI-DROK0017.

           05  PI-DROK0017-HADPERSONENID            PIC  X(013).
           05  PI-DROK0017-HADVERTRAGID             PIC  X(013).
           05  PI-DROK0017-HADADRESSEALTID          PIC  X(013).

       01  PO-DROK0017.
           05  PO-DROK0017-RC                          PIC  X(003).
               88 PO-DROK0017-RC-OK                        VALUE 'OK '.
               88 PO-DROK0017-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DROK0017-DATEN.

             10 PO-DROK0017-HADLFDNR                PIC S9(4)  COMP.
      ********************** Datenende *********************************
