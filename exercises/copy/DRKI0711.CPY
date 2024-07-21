      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0711.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0711.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:58:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   29 Oct 1998 17:55:58   T13EX19
      *FKI00010 - Neuerstellung
      *
      *   Rev 1.0   06 Oct 1998 15:31:40   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0711 Funktion Single SELECT              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    08.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !15.10.98! 01 ! NEUERSTELLUNG             ! gbe      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0711                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT <<<        PERSONENROLLE                      *
      ****************************************************************
      *
       01 PI-DRKI0711.
          05 PI-DRKI0711-PERSKEY.
             10 PI-DRKI0711-PERSROLLENID       PIC X(13).
             10 PI-DRKI0711-PERSROLLENKZ       PIC X(1).
      *
       01 PO-DRKI0711.
          05 PO-DRKI0711-RC                    PIC  X(003).
             88 PO-DRKI0711-RC-OK                         VALUE 'OK '.
             88 PO-DRKI0711-RC-NFD                        VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
