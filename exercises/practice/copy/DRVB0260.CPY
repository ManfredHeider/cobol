      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0260.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0260.CPv  $
      *
      *   Rev 2.2   May 20 2008 12:11:48   t13ae48
      *AVB00875: Verlängerung der Adressfelder und Kontaktdaten - P
      *rogramme (JMS) - 3
      *
      *   Rev 2.1   Dec 13 1999 12:18:32   T13AE42
      *AAS00195 - Einbau Handy-Nr.
      *fuer Hr. Dr. Pohl
      *
      *   Rev 2.0   Feb 27 1999 15:58:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:23:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
     ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00195!12.06.99! 01 ! NEUERSTELLUNG             !B.SCHMIDT *
      * AVB00875!11.03.08! 02 ! AUTOTELEFON      14->26   !J.MAESS   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0260                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0260.
          05 PI-DRVB0260-DATEN.
             10 PI-DRVB0260-MA-ST-NR         PIC S9(009)
                                             BINARY.
      *
       01 PO-DRVB0260.
          05 PO-DRVB0260-RC                  PIC  X(003).
             88 PO-DRVB0260-RC-OK            VALUE 'OK '.
             88 PO-DRVB0260-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0260-DATEN.
110308*      10 PO-DRVB0260-MA-AUTOTELEFON   PIC  X(14).
110308       10 PO-DRVB0260-MA-AUTOTELEFON   PIC  X(26).

      ******** ENDE COPY-MEMBER **************************************
