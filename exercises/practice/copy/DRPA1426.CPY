      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1426.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1426.CPv  $
      *
      *   Rev 2.1   23 May 2001 13:51:58   t13ex21
      *APA00096 - Neuerstellung
      *
      *   Rev 2.0   12 Jul 2000 12:32:54   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1426                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1426                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    19.10.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00096!19.10.00!2.01! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1426
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1426 DES UNTERPROGRAMMS UPPA0078 LÖSCHT DIE
      *    DIE TS-QUEUE-KONFIGURATION.
      *
      ****************************************************************
      *
       01  PI-DRPA1426.
           05  PI-DRPA1426-DUMMY               PIC  X(01).
      *
       01  PO-DRPA1426.
           05  PO-DRPA1426-RC                  PIC  X(03).
               88 PO-DRPA1426-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1426-RC-NOK                       VALUE 'NOK'.
      *
      ******* ENDE COPY-MEMBER **************************************
