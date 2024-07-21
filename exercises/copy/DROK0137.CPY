      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.06.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0137.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0137.CPv  $
      *
      *   Rev 2.1   Jul 03 2013 15:22:36   t13ex27
      *AKI01204: SEPA - DB-Online-AML-Korrektur Beitragszahler
      *
      *   Rev 2.0   Jun 27 2013 11:00:02   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01204!27.06.13! 2.1! neu                       !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DROK0137.
           05  PI-DROK0137-KEY.

             10 PI-DROK0137-HNAPARTGESELLSCHID   PIC S9(4)  COMP.
             10 PI-DROK0137-HNAVERTRAGSNUMMER    PIC X(15).

       01  PO-DROK0137.
           05  PO-DROK0137-RC                     PIC  X(003).
               88 PO-DROK0137-RC-OK               VALUE 'OK '.
               88 PO-DROK0137-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DROK0137-DATEN.
             10 PO-DROK0137-HNAKUNDENID          PIC X(13).
             10 PO-DROK0137-HNALFDNR             PIC S9(4)  COMP.
             10 PO-DROK0137-HNAERSTELLTVON       PIC X(30).
             10 PO-DROK0137-HNAERSTELLT          PIC X(26).
             10 PO-DROK0137-HNATICKETID          PIC S9(18)V  COMP-3.
             10 PO-DROK0137-LETZTEAENDERUNG      PIC X(26).
             10 PO-DROK0137-HNA-KTY              PIC S9(4)  COMP.
             10 PO-DROK0137-HNA-KNR              PIC S9(4)  COMP.
             10 PO-DROK0137-HNAALTANREDEID       PIC S9(4)  COMP.
             10 PO-DROK0137-HNAALTVORNAME        PIC X(30).
             10 PO-DROK0137-HNAALTNACHNAME       PIC X(30).
             10 PO-DROK0137-HNAALTGEBGRUENDDAT   PIC X(10).
             10 PO-DROK0137-HNANEUANREDEID       PIC S9(4)  COMP.
             10 PO-DROK0137-HNANEUVORNAME        PIC X(30).
             10 PO-DROK0137-HNANEUNACHNAME       PIC X(30).
             10 PO-DROK0137-HNANEUGEBGRUENDDAT   PIC X(10).
      *
      ******* ENDE COPY-MEMBER ***************************************
