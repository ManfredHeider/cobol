      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0220.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0220.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:47:56   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jun 1998 13:49:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0220  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    14.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!14.07.98! 01 ! NEUERSTELLUNG             ! Veit     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0220                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE <<<        AUSSENDIENSTMA    "ADMA"           *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0220.
          05 PI-DRKI0220-ADMAKEY.
             10 PI-DRKI0220-ADMAPERSONENID     PIC X(013).
             10 PI-DRKI0220-ADMAPERSONENROLLE  PIC X(001).
          05 PI-DRKI0220-ADMADATEN.
             10 PI-DRKI0220-NI-ARBSTATIONID    PIC S9(4)  COMP.
             10 PI-DRKI0220-ADMAARBSTATIONID   PIC S9(9)  COMP.
      *
       01 PO-DRKI0220.
          05 PO-DRKI0220-RC                  PIC  X(003).
             88 PO-DRKI0220-RC-OK                         VALUE 'OK '.
             88 PO-DRKI0220-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRKI0220-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
