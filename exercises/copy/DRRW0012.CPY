      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0012.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRRW0012.CPv  $
      *
      *   Rev 2.1   Feb 12 2013 18:52:32   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.0   Feb 11 2013 10:12:46   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRRW0012                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPRW0001                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PRETSCH                                          *
      *    DATUM:    11.12.2012                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01244!11.12.12! 01 ! NEU (aus DRRW0009)        !B.PRETSCH *
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
       01  PI-UPRW0001.
           05  PI-UPRW0001-FUNKTION            PIC  X(008).
      *--- Funktion: IBAN-Validierung
               88 PI-UPRW0001-DRRW0007              VALUE 'DRRW0007'.
      *--- Funktion: IBAN-Berechnung
               88 PI-UPRW0001-DRRW0008              VALUE 'DRRW0008'.
      *--- Funktion: IBAN-Splittung
               88 PI-UPRW0001-DRRW0009              VALUE 'DRRW0009'.
      *--- Funktion: IBAN-Kurzprüfung
               88 PI-UPRW0001-DRRW0010              VALUE 'DRRW0010'.
      *--- Funktion: BIC-Validierung
               88 PI-UPRW0001-DRRW0011              VALUE 'DRRW0011'.
      *
      ******* ENDE COPY-MEMBER **************************************
