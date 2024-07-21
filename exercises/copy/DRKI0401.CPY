      * P13SP99   Stand: 11/09/98    12:34:50
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0401.CPY  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0401.CPv  $
      *
      *   Rev 1.1   Apr 16 1999 12:09:08   T13EX49
      *AKI00028 - Prod
      *
      *   Rev 1.0   14 Jul 1998 17:05:02   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0401 Funktion INSERT DIREKTIONSÄNDERUNG  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    23.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!23.07.98! 01 ! NEUERSTELLUNG             ! Veit     *
      * AKI00006!09.09.98! 01 ! MODIFIKATION              ! BRENNECKE*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0401                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<        DIREKTIONSÄNDERUNG "DIRE"          *
      ****************************************************************
      *
       01 PI-DRKI0401.
          05 PI-DRKI0401-DIRE-KEY.
             10 PI-DRKI0401-DIRAENDID            PIC X(13).
          05 PI-DRKI0401-DIRE-DATEN.
             10 PI-DRKI0401-DIRAENDPERSONENID    PIC  X(013).
             10 PI-DRKI0401-DIRAENDPERSROLLE     PIC  X(001).
             10 PI-DRKI0401-DIRAENDDTAERLEDIGT   PIC  X(001).
      ****   DEFAULT-WERT: 'N'
      *

      *
       01 PO-DRKI0401.
          05 PO-DRKI0401-RC                      PIC  X(003).
             88 PO-DRKI0401-RC-OK                     VALUE 'OK '.
             88 PO-DRKI0401-RC-DUPLICATE-VALUE        VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
