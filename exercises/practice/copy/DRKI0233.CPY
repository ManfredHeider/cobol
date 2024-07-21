      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0233.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0233.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:48:28   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jun 1998 13:50:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0233  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    13.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!13.07.98! 01 ! NEUERSTELLUNG             ! Veit     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0233                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE <<<        MITARBEITER                        *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0233.
          05 PI-DRKI0233-MARBKEY.
             10 PI-DRKI0233-MITARBPERSONENID   PIC X(013).
             10 PI-DRKI0233-MITARBPERSROLLE    PIC X(01).
          05 PI-DRKI0233-MARBDATEN.
             10 PI-DRKI0233-MITARBVORGESPERSID PIC X(013).
      *
       01 PO-DRKI0233.
          05 PO-DRKI0233-RC                  PIC  X(003).
             88 PO-DRKI0233-RC-OK                         VALUE 'OK '.
             88 PO-DRKI0233-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
