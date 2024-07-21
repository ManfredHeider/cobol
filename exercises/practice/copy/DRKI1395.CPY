      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1395.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1395.CPv  $
      *
      *   Rev 2.2   Nov 28 2018 11:50:56   t13ex27
      *AKI01639: Zuordnung von Verträgen anhand ihrer Herkunft, auc
      *h bei Deutsche Bank Kunden, die mehr als einen Vornamen habe
      *
      *   Rev 2.1   Oct 13 2003 11:47:42   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 2.0   Apr 09 2002 08:33:00   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1395  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    23.06.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!23.06.98! 01 ! NEUERSTELLUNG             ! Veit     *
      * AKI01639!24.10.18!2.2 ! Korrektur für Kundensuche ! IFR      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1395                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<                                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1395.
          05 PI-DRKI1395-KEY.
             10 PI-DRKI1395-ADRESSEID            PIC  X(013).
      *
       01  PO-DRKI1395.
           05  PO-DRKI1395-RC                     PIC  X(003).
               88 PO-DRKI1395-RC-OK               VALUE 'OK '.
               88 PO-DRKI1395-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI1395-DATEN.

             10 PO-DRKI1395-ADRESSELANDID   PIC  X(0003).
             10 PO-DRKI1395-ADRESSEPLZ      PIC  X(0015).
      *
      ****** Ende Copy-Member ****************************************

