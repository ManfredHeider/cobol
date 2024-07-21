      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.08.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2671.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2671.CPv  $
      *
      *   Rev 2.2   Sep 28 2012 10:53:50   t13ex27
      *AKI01173: Rücksetzen der BDSG-Sperre
      *
      *   Rev 2.1   Sep 28 2012 10:43:20   t13ex27
      *AKI01173: Rücksetzen der BDSG-Sperre
      *
      *   Rev 2.0   Aug 28 2012 10:03:26   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: UPKI0240                                     *
      *    ----------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO UPKI0240                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    ----------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01173!28.08.12! 2.1! Neu                       ! IFR      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-DRKI2671.
           05  PI-DATEN.
               10  PI-DRKI2671-OTTICKETID             PIC  9(018).
               10  PI-DRKI2671-OTERSTELLT             PIC  X(026).
               10  PI-DRKI2671-OTERSTELLTVON          PIC  X(030).
               10  PI-DRKI2671-OTHOSTKUNDENID         PIC  X(013).
               10  PI-DRKI2671-OTHOSTVERTRAGID        PIC  X(013).
               10  PI-DRKI2671-OTAENDGRUND-KNR        PIC S9(004) COMP.

       01  PO-DRKI2671.
           05  PO-DRKI2671-RC               PIC  X(003).
               88 PO-DRKI2671-RC-OK              VALUE 'OK '.
               88 PO-DRKI2671-RC-NOK             VALUE 'NOK'.
           05  PO-DATEN.
               10  PO-DRKI2671-OTFEHLER-KNR           PIC S9(004) COMP.

      ******* ENDE COPY-MEMBER **************************************
