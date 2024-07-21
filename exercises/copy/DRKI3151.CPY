      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.04.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3151.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3151.CPv  $
      *
      *   Rev 2.2   Jul 24 2017 13:58:08   t13ex27
      *AKI01506: Deuba KK aus GHP und DBP nach BANKPRODUKT überführ
      *en-Abgleich
      *
      *   Rev 2.1   Jun 20 2017 14:02:08   t13ex27
      *AKI01506: Deuba KK aus GHP und DBP nach BANKPRODUKT überführ
      *en-Abgleich
      *
      *   Rev 2.0   Apr 24 2017 18:26:30   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2413  FUNKTIONEN LIST-OZP                *
      *                                                              *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00966!13.08.10!2.1 ! NEUERSTELLUNG             !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRKI3151.
           05  PI-DRKI3151-KEY.
               10 PI-DRKI3151-VERTRAGSID       PIC  X(13).
               10 PI-DRKI3151-PRODLFDNR        PIC S9(09) BINARY.

      *
       01  PO-DRKI3151.
           05  PO-DRKI3151-RC                 PIC  X(003).
               88 PO-DRKI3151-RC-OK           VALUE 'OK '.
               88 PO-DRKI3151-RC-NOT-FOUND    VALUE 'NFD'.
      *
           05  PO-DRKI3151-DATEN.
               10 PO-DRKI3151-PRODUKTID       PIC S9(4) COMP.
      *
      ******* ENDE COPY-MEMBER ***************************************


