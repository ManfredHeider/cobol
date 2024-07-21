      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1109.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1109.cpv  $
      *
      *   Rev 2.1   May 06 1999 08:48:58   T01JM2S
      *APA00012 - NEUE FUNKTION DRPA1225
      *
      *   Rev 2.0   May 05 1999 17:54:08   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.0   21 Oct 1998 13:57:08   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1109                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERM. SST NEUTR.     >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    29.10.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!29.10.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!05.05.99!2.1 ! EINBAU DRPA1226           ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1109                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1109.
          05 PI-DRPA1109-DATEN.
             10 PI-DRPA1109-MIN-DATEN-KONTR.
                COPY DRPA1226.
      *
       01 PO-DRPA1109.
          05 PO-DRPA1109-RC                         PIC  X(003).
             88 PO-DRPA1109-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1109-RC-NICHT-BUCHEN               VALUE 'NBU'.
          05 PO-DRPA1109-DATEN.
             10 PO-DRPA1109-KOFV-TEIL.
                COPY DRPA0847.
      *
      ******* ENDE COPY-MEMBER **************************************
