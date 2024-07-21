      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1224.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1224.CPv  $
      *
      *   Rev 2.1   Apr 30 1999 15:37:02   T13EX21
      * 
      *
      *   Rev 2.0   Mar 30 1999 08:44:06   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:26   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1224  FUNKTION LESE-OZP FÜR TABELLE      *
      *                                    PVSG                      *
      *    --------------------------------------------------------  *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    28.04.1999                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!28.04.99! 01  !NEUERSTELLUNG             ! STORCH   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1224                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1224.
          05  PI-DRPA1224-AUSWAHL-KRITERIEN.
               10 PI-DRPA1224-VSTATUS            PIC  X(04).
      *
       01 PO-DRPA1224.
          05 PO-DRPA1224-RC                  PIC  X(003).
             88 PO-DRPA1224-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1224-RC-NOT-FOUND                 VALUE 'NFD'.

