      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.10.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0433.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRAT0433.CPv  $
      *
      *   Rev 2.5   Jan 22 2015 15:25:00   t13ex27
      *AKI01339: Redesign ZTAT0039
      *
      *   Rev 2.4   Nov 11 2011 14:21:18   t13ex14
      *AKI01079: DB-Objekte: TBAT0039 / VERTRAGSARTBEZ
      *
      *   Rev 2.3   Jan 28 2009 16:08:08   t13ex51
      *AKI00772: Reisegepäckversicherung Aktualisierung
      *
      *   Rev 2.2   Sep 17 2008 09:27:48   t13ex51
      *AKI00731: Anpassen für Lesen TBAT0039 mit UPKI
      *
      *   Rev 2.1   Oct 03 2007 12:18:34   t13ex51
      *AKL00664: Allfinanz: Supporttabelle KTY_KNR_SCHLUESSEL
      *
      *   Rev 2.0   Oct 01 2007 15:17:40   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: UPAT0040                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZTKI0039                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKL00644!01.10.07!1.0 ! NEU                       ! HRO      *
      * -------------------------------------------------------------*
      * AKI00731!16.08.08!2.0 ! ANPASSEN AN VERARB. MIT   ! HRO      *
      *         !        !    ! UPKI0216                  !          *
      * -------------------------------------------------------------*
      * AKI00772!22.12.08!3.0 ! Neue Function SUH-TEXTE   ! HRO      *
      * -------------------------------------------------------------*
071011* AKI01079!07.10.11!4.0 ! Neue Function DRAT0488    ! WMI      *
      *         !        !    ! f. Advocard-Wagnistexte   !          *
      * -------------------------------------------------------------*
      * AKI01339!16.12.14!2.5 ! Funtions-CPY für UPAT0040 ! IFR      *
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
       01  PI-UPAT0040.
           05  PI-UPAT0040-FUNKTION            PIC  X(008).


             88   PI-UPAT0040-DRAT0434     VALUE 'DRAT0434'.
160808       88   PI-UPAT0040-DRAT0439     VALUE 'DRAT0439'.
221208       88   PI-UPAT0040-DRAT0445     VALUE 'DRAT0445'.
071011       88   PI-UPAT0040-DRAT0488     VALUE 'DRAT0488'.

      *
      ******* ENDE COPY-MEMBER **************************************

