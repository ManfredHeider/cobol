      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0708.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0708.CPv  $
      *
      *   Rev 2.0   May 27 1999 12:47:00   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.1   May 27 1999 12:46:22   T13EX25
      * 
      *
      *   Rev 1.0   12 Dec 1997 09:41:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0708 FUNKTION Prüfe PM auf Folgefehler   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J. Jäger-Sokoluk                                *
      *    DATUM:    19.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!19.12.97!01.01!NEU                       ! Jäger    *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0708                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0708.
          05 PI-DRPA0708-DATEN.
             10 PI-DRPA0708-PRINTE           PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA0708-PNAP-ID          PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA0708.
          05 PO-DRPA0708-RC                  PIC  X(003).
             88 PO-DRPA0708-RC-OK                 VALUE 'OK '.
             88 PO-DRPA0708-RC-NOT-FOUND          VALUE 'NFD'.
      *
      ******** Ende Copy-Member **************************************
