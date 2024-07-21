      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0736.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0736.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:58:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   29 Oct 1998 17:50:36   T13EX19
      *FKI00010 - Neuerstellung
      *
      *   Rev 1.0   06 Oct 1998 15:34:14   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0736 Funktion DELETE eine Zeile in PERSON *
      *    -------------------------------------------------------    *
      *    ERSTELLT: gbe                                             *
      *    DATUM:    16.10.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!15.10.98! 01 ! NEUERSTELLUNG             ! gbe      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0736                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE <<<        PERSON                             *
      ****************************************************************
      *
       01 PI-DRKI0736.
          05 PI-DRKI0736-PERSONENKEY.
             10 PI-DRKI0736-PERSONENID         PIC X(13).
      *
       01 PO-DRKI0736.
          05 PO-DRKI0736-RC                    PIC  X(003).
             88 PO-DRKI0736-RC-OK                         VALUE 'OK '.
             88 PO-DRKI0736-RC-NFD                        VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
