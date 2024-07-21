      * T13EX48   Stand: 18/11/98    12:09:37
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0764.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0764.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 19:32:08   T13EX48
      *AKI00028
      *PI/PO Funktion DRKI0764
      *
      *   Rev 2.0   Apr 16 1999 18:07:52   T13EX48
      *AKI00028
      *PI/PO FUNKTION DRKI0764
      *
      *   Rev 1.0   06 Oct 1998 15:37:48   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0764  FUNKTION UPDATE RLK, REPLIKATION   *
      *    --------------------------------------------------------  *
      *    ERSTELLT:                                                 *
      *    DATUM:                                                    *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * KI      !18.11.98! 01 ! NEUERSTELLUNG             ! HSE      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0764                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRKI0764.
          05 PI-DRKI0764-PROGRAMM                 PIC  X(008).
          05 PI-DRKI0764-UFKT                     PIC  9(002).
          05 PI-DRKI0764-DATEN.
             10 PI-DRKI0764-RUECKLAUFKONTRID      PIC X(013).
      *
       01 PO-DRKI0764.
          05 PO-DRKI0764-RC                       PIC  X(003).
             88 PO-DRKI0764-RC-OK                        VALUE 'OK '.
             88 PO-DRKI0764-RC-NOK                       VALUE 'NOK'.
          05 PO-DRKI0764-DATEN.
             10 FILER                             PIC  X(001).
      *
      ***** ENDE-COPY-MEMBER *****************************************
