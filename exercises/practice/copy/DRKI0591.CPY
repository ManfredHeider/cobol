      * P13SP99   Stand: 06/12/98    18:02:02
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0591.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0591.CPv  $
      *
      *   Rev 2.0   Apr 16 1999 12:11:44   T13EX49
      *AKI00028 - Prod.übernahme
      *
      *   Rev 1.1   Apr 16 1999 12:09:58   T13EX49
      *AKI00028 - Prod.übernahme
      *
      *   Rev 1.0   07 Aug 1998 14:50:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0591 Funktion UPDATE DIREKTIONSÄNDERUNG  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    18.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!18.08.98! 01 ! NEUERSTELLUNG             !BRENNECKE *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0591                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE/DIRAENDDTAERLEDIGT <<<   DIREKTIONSÄNDERUNG   *
      ****************************************************************
      *
       01 PI-DRKI0591.
          05 PI-DRKI0591-DIRE-KEY.
             10 PI-DRKI0591-DIRAENDID            PIC X(13).
          05 PI-DRKI0591-DIRE-DATEN.
             10 PI-DRKI0591-DIRAENDDTAERLEDIGT   PIC  X(001).
      *
       01 PO-DRKI0591.
          05 PO-DRKI0591-RC                      PIC  X(003).
             88 PO-DRKI0591-RC-OK                     VALUE 'OK '.
             88 PO-DRKI0591-RC-NOT-FOUND              VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
