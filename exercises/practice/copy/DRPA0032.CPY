      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0032.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0032.CPv  $
      *
      *   Rev 2.2   Jul 04 2016 10:06:20   t13ae34
      *AAN01269: Anträge Fondsdepotbank - Berücksichtigung Fremdfonds
      *
      *   Rev 2.1   Jan 04 2000 13:04:38   T13AE18
      *APA00068
      *
      *   Rev 2.0   Feb 27 1999 14:27:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:49:34   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    Copy-Member: DRPA0032  Funktionen Standard-OZP            *
      *                           PGTA TBPA0009                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    20.08.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Daisy 1 !20.08.96! 01 ! Neuerstellung             ! Priggen  *
      * APA00068!18.11.99!2.01!NEUE FUNKTION DRPA0923     ! BOHLINGER*
      * AAN01269!01.07.16!2.3 !NEUE FUNKTION DRPA1846     ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0009.
           05 PI-ZTPA0009-FUNKTION            PIC X(008).
              88 PI-ZTPA0009-DRPA0062-INS         VALUE 'DRPA0062'.
              88 PI-ZTPA0009-DRPA0063-UPD         VALUE 'DRPA0063'.
              88 PI-ZTPA0009-DRPA0064-DEL         VALUE 'DRPA0064'.
              88 PI-ZTPA0009-DRPA0065-SEL-ALL     VALUE 'DRPA0065'.
              88 PI-ZTPA0009-DRPA0923             VALUE 'DRPA0923'.
              88 PI-ZTPA0009-DRPA1846             VALUE 'DRPA1846'.

