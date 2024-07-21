      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.06.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2633.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2633.CPv  $
      *
      *   Rev 2.2   Apr 14 2014 10:33:44   t13ex39
      *FKI01461:  ungültigen VBs für DeuBa-Verträge
      *
      *   Rev 2.1   Aug 15 2012 13:56:08   t13ex14
      *AKI01155: VerwFond berein. 1: Auswertung VBUNGUELTIGAV
      *
      *   Rev 2.0   Jun 29 2012 11:07:20   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2633                                     *
      *    ----------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZXKI0448                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    ----------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01155!02.07.12!2.0 ! NEU                       ! WMI      *
      * -------------------------------------------------------------*
      * FKI01461!07.04.14! 1.0! zus. Fkt.  2854           ! WHO      *
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
       01  PI-ZXKI0448.
           05  PI-ZXKI0448-FUNKTION              PIC  X(008).


             88   PI-ZXKI0448-DRKI2634-LIST     VALUE 'DRKI2634'.
             88   PI-ZXKI0448-DRKI2854-LIST     VALUE 'DRKI2854'.

      *
      ******* ENDE COPY-MEMBER **************************************
