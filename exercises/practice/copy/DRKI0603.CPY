      * P13SP99   Stand: 05/10/98    11:41:56
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0603.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0603.CPv  $
      *
      *   Rev 2.2   Nov 30 2016 14:21:18   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-Rück-DTA
      *
      *   Rev 2.1   Mar 31 2005 12:31:02   t13ex48
      *AKI00456: Projekt e-Post: Import und Prüfung der Metadaten d
      *er AML
      *
      *   Rev 2.0   Apr 16 1999 13:17:24   T13EX19
      *aki00028
      *
      *   Rev 1.0   25 Aug 1998 09:22:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0603  FUNKTIONEN Lese-OZP                *
      *                             VB-DATEN ERMITTELN               *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    23.08.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!30.08.98! 01 ! NEUERSTELLUNG             ! BRENNECKE*
      * -------------------------------------------------------------*
      * AKI00456!10.01.05! 01 ! Neue Funktion             ! HSE      *
      * -------------------------------------------------------------*
      * AKI01461!09.09.16!2.2 ! DRKI_1744 entfernt        ! IFR      *
      * -------------------------------------------------------------*
      *                                                              *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZJKI0009.
           05 PI-ZJKI0009-FUNKTION             PIC X(008).
              88 PI-ZJKI0009-DRKI0604-SELECT       VALUE 'DRKI0604'.

