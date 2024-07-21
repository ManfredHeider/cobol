      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0033.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0033.CPv  $
      *
      *   Rev 2.3   Nov 26 2009 14:33:10   t13ae34
      *AAN00896: OptimA Stufe 1 - SST OKI -> Antragsbestand
      *
      *   Rev 2.2   04 Dec 2006 12:38:24   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Apr 23 2002 16:59:46   t13ae34
      *AAN00357-POA-AMV
      *
      *   Rev 2.0   Feb 27 1999 11:05:36   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:36:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0034  FUNKTIONEN STANDARD-OZP            *
      *                           POAA TBAN0003                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00357!18.03.02!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN00655!01.12.06!2.2 ! FKT. DRAN0180             ! HEIDER   *
      * AAN00896!01.10.09!2.3 ! FKT. DRAN0248             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTAN0003.
           03 PI-ZTAN0003-FUNKTION            PIC X(008).
              88 PI-ZTAN0003-DRAN0041-INS         VALUE 'DRAN0041'.
              88 PI-ZTAN0003-DRAN0042-UPD         VALUE 'DRAN0042'.
              88 PI-ZTAN0003-DRAN0045-SEL         VALUE 'DRAN0045'.
              88 PI-ZTAN0003-DRAN0058-SEL         VALUE 'DRAN0058'.
              88 PI-ZTAN0003-DRAN0180-UPD         VALUE 'DRAN0180'.
              88 PI-ZTAN0003-DRAN0248-DEL         VALUE 'DRAN0248'.
