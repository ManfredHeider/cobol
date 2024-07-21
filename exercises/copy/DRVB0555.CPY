      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0555.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0555.CPv  $
      *
      *   Rev 2.1   10 Jul 2001 11:19:20   t13ae13
      *AVB00534: Einführung PIN
      *
      *   Rev 2.0   15 May 2001 13:33:34   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0555  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WALB                                            *
      *    DATUM:    10.07.2001                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00534!10.07.01! 2.1! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0555                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN  <<<        LESEN INAKTIV-DATEI                *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0555.
          10 PI-DRVB0555-UFKT                  PIC  X.
              88 PI-DRVB0555-UFKT-READ-FIRST   VALUE '1'.
              88 PI-DRVB0555-UFKT-READ-NEXT    VALUE '2'.
              88 PI-DRVB0555-UFKT-READ-LAST    VALUE '3'.
          10 PI-DRVB0555-VBNR                  PIC 9(007).
      *
       01 PO-DRVB0555.
          10 PO-DRVB0555-RC                    PIC  X(003).
             88 PO-DRVB0555-RC-OK              VALUE 'OK '.
             88 PO-DRVB0555-RC-NOT-FOUND       VALUE 'NFD'.

