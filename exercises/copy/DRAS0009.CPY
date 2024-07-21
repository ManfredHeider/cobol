      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0009.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAS0009.CPv  $
      *
      *   Rev 2.1   26 Mar 2001 08:02:58   T13EX57
      *AAS00264 -  Datenzeile für Druckaufbereitung
      * Wettbewerb/Leistungsplan 2001 
      *
      *   Rev 2.0   Feb 27 1999 11:26:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Jan 1999 15:56:06   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: SATZBESCHREIBUNG SCHNITTSTELLENDATEI FÜR      *
      *                 GRUPPEN-ERMITTLUNG (GW ÜBER RANG)             *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHÖLLHAMMER                                     *
      *****************************************************************
      *    VERSION   01   25.02.1994                                  *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAS00264!06.03.01! 01 ! NEU                       ! U.BOLL    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      ****************************************************************
      *
      *    SCHNITTSTELLE ZWISCHEN EINEM DATENERMITTLUNGS- UND EINEM
      *    GRUPPENAUFBEREITUNGS-PGM, DAS EINEN GRUPPENWECHSEL ÜBER
      *    'RANG' ODER 'ANZAHL' AUSFÜHREN SOLL
      *
      *    RANGW-KRITERIUM: FALLS RANG-KZ = 'J', ENTHÄLT ES DAS
      *             RANGWECHSEL-KRITERIM (KANN ZUSAMMENGESETZT SEIN)
      *    DATEN  : DRUCKAUFBEREITETE DATENZEILE - OHNE RANG-NR
      *
       01  DRAS0009.
           05  DRAS0009-RANGW-KRITERIUM  PIC  X(030).
           05  DRAS0009-DATEN            PIC  X(132).
      *
      ******** ENDE COPY-MEMBER (DRAS0009) ************
      *



