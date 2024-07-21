      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0312.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0312.CPv  $
      *
      *   Rev 2.2   Jun 21 2004 16:39:14   t13ae34
      *Werkzeug für Auswertungen im eingereichten Geschäft
      *
      *   Rev 2.1   Jan 11 2000 10:49:00   T13AE34
      *AAN00237 - MIS Antragsbereich
      *
      *   Rev 2.0   Mar 16 1999 09:57:22   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0312  FUNKTIONEN FüR ZXAT0017            *
      *                           PRODUKTGEBERGESELLSCHAFTEN         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    07.12.1999                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00177!07.12.99!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN00492!21.05.04!2.2 ! NEUE FKT. DRAT0400        ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZXAT0017.
           03 PI-ZXAT0017-FUNKTION            PIC X(008).
              88 PI-ZXAT0017-DRAT0313             VALUE 'DRAT0313'.
              88 PI-ZXAT0017-DRAT0400             VALUE 'DRAT0400'.
