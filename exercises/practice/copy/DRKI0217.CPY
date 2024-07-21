      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0217.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0217.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:47:48   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jun 1998 13:49:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0217  FUNKTION UPDATE STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRKI0217                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE <<<        AUSSENDIENSTMA    "ADMA"           *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0217.
          05 PI-DRKI0217-ADMAKEY.
             10 PI-DRKI0217-ADMAPERSONENID     PIC X(013).
             10 PI-DRKI0217-ADMAPERSONENROLLE  PIC X(001).
          05 PI-DRKI0217-ADMADATEN.
             10 PI-DRKI0217-ADMAINPROBEKZ      PIC X(001).
      *
       01 PO-DRKI0217.
          05 PO-DRKI0217-RC                  PIC  X(003).
             88 PO-DRKI0217-RC-OK                         VALUE 'OK '.
             88 PO-DRKI0217-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRKI0217-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
