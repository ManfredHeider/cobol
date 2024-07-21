      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.04.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0015.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0015.CPv  $
      *
      *   Rev 2.1   Feb 27 2007 12:55:26   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - OZPs
      *
      *   Rev 2.0   Apr 04 2006 12:19:40   t13ex27
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
       01  PI-DROK0015.

           05  PI-DROK0015-OTTICKETTYP-KTY      PIC S9(4) COMP.
           05  PI-DROK0015-OTTICKETTYP-KNR      PIC S9(4) COMP.
           05  PI-DROK0015-OTAENDGRUND-KTY      PIC S9(4) COMP.
           05  PI-DROK0015-OTERSTELLT           PIC X(26).
           05  PI-DROK0015-OTHOSTKUNDENID       PIC X(13).
           05  PI-DROK0015-OTHOSTVERTRAGID      PIC X(13).

       01  PO-DROK0015.
           05  PO-DROK0015-RC                          PIC  X(003).
               88 PO-DROK0015-RC-OK                        VALUE 'OK '.
               88 PO-DROK0015-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DROK0015-DATEN.

             10 PO-DROK0015-OTTICKETID           PIC S9(18)V COMP-3.
             10 PO-DROK0015-OTERSTELLTVON        PIC X(30).
             10 PO-DROK0015-OTOKIKUNDENID        PIC S9(18)V COMP-3.
             10 PO-DROK0015-OTOKIVERTRAGID       PIC S9(18)V COMP-3.
             10 PO-DROK0015-OTSWVERSION.
                49 PO-DROK0015-OTSWVERSION-LEN   PIC S9(4) COMP.
                49 PO-DROK0015-OTSWVERSION-TEXT  PIC X(100).
             10 PO-DROK0015-OTSTATUS-KTY         PIC S9(4) COMP.
             10 PO-DROK0015-OTSTATUS-KNR         PIC S9(4) COMP.
             10 PO-DROK0015-OTAUSLESEJOBID       PIC S9(18)V COMP-3.
             10 PO-DROK0015-OTFEHLER-KTY         PIC S9(4) COMP.
             10 PO-DROK0015-OTFEHLER-KNR         PIC S9(4) COMP.
             10 PO-DROK0015-OTPARAMETER.
                49 PO-DROK0015-OTPARAMETER-LEN   PIC S9(4) COMP.
                49 PO-DROK0015-OTPARAMETER-TEXT  PIC X(1000).
      ********************** Datenende *********************************
