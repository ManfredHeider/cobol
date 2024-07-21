      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0303.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAT0303.CPv  $
      *
      *   Rev 2.4   04 Dec 2006 14:24:24   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.3   Dec 02 2004 11:38:58   t13ex22
      *FVB00590: Meldung bei mehreren Orten zu einer PLZ
      *
      *   Rev 2.2   Nov 29 1999 17:39:50   T13AE34
      *AAN00236-PLZ
      *
      *   Rev 2.1   Nov 29 1999 10:59:58   T13AE34
      *AAN00236-PLZ
      *
      *   Rev 2.0   Mar 16 1999 09:57:20   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0303                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONS-SCHNITTSTELLE FÜR DAS UPRO UPAT0028              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. HEIDER                                        *
      *    DATUM:    07.10.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00236!07.10.99!2.1 ! NEU                       ! HEIDER   *
      * FVB00590!29.11.04!2.2 ! FKT. DRVB0405 NEU         ! MUND     *
      * AAN00655!16.11.06!2.4 ! FKT. DRAT0426             ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.   NUR DIE FUNKTIONEN
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-UPAT0028.
           05  PI-UPAT0028-FUNKTION            PIC  X(008).
               88 PI-UPAT0028-DRAT0304              VALUE 'DRAT0304'.
291104         88 PI-UPAT0028-DRAT0405              VALUE 'DRAT0405'.
161106         88 PI-UPAT0028-DRAT0426              VALUE 'DRAT0426'.
      *
      ******* ENDE COPY-MEMBER **************************************
