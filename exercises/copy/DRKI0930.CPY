      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0930.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0930.CPv  $
      *
      *   Rev 2.1   Oct 21 1999 14:52:22   T13EX50
      *AKI0003 - Produktionsübergabe :
      *                Neues Bestellverfahren 
      *                "Der Vermögensberater"
      *
      *   Rev 2.0   Jul 30 1999 14:50:42   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0095  FUNKTION DATEN INIT KOMPETENZSATZ  *
      *    --------------------------------------------------------  *
      *    ERSTELLT:                                                 *
      *    DATUM:                                                    *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00003!        ! 01 ! NEUERSTELLUNG             ! W.SCHMIDT*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0095                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRKI0930.
          05 PI-DRKI0930-PZVP-VERSION             PIC S9(004) BINARY.
          05 PI-DRKI0930-PROGRAMM                 PIC  X(008).
          05 PI-DRKI0930-UFKT                     PIC  9(002).
          05 PI-DRKI0930-SCHALTER                 PIC  9.
          05 PI-DRKI0930-DATEN.
             10 PI-DRKI0930-VORNAME               PIC  X(015).
             10 PI-DRKI0930-NACHNAME              PIC  X(030).
      *
       01 PO-DRKI0930.
          05 PO-DRKI0930-RC                       PIC  X(003).
             88 PO-DRKI0930-RC-OK                        VALUE 'OK '.
          05 PO-DRKI0930-DATEN.
             10 PO-DRKI0930-VORZUNAME             PIC  X(030).
      *

