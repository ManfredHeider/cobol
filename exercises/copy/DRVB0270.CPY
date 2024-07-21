      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0270.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0270.cpv  $
      *
      *   Rev 2.1   Feb 14 2000 16:05:26   T13AE42
      *AVB00474 - Anpassung neuer VB-Vertrag
      *
      *   Rev 2.0   Feb 27 1999 15:58:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:24:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0270                                     *
      *    FUNKTION: ERMITTELT OB EINE VBNR / PRUEFZIFFER BELEGT     *
      *    IST                                                       *
      *    --------------------------------------------------------- *
      *    ERSTELLT: SCHMIDT,B.                                      *
      *    DATUM:    14.02.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00474!14.02.00! 01 ! NEUERSTELLUNG             !SCHMIDT,B.*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0270                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0270.
          05 PI-DRVB0270-DATEN.
             10 PI-DRVB0270-ST-NR            PIC S9(009)
                                             BINARY.
             10 PI-DRVB0270-ST-PRUEFZIFFER   PIC  X(001).
      *
       01 PO-DRVB0270.
          05 PO-DRVB0270-RC                  PIC  X(003).
             88 PO-DRVB0270-RC-OK            VALUE 'OK '.
             88 PO-DRVB0270-RC-NFD           VALUE 'NFD'.

      *
      ******** ENDE COPY-MEMBER **************************************
