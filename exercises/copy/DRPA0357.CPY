      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0357.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0357.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:39:00   T13EX25
      * 
      *
      *   Rev 1.1   May 26 1999 15:38:20   T13EX25
      *APA00042, Erstellen V2.0
      *
      *   Rev 1.0   09 Jul 1997 16:02:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0357  EVS-Struktur anpassen              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    08.07.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !08.07.97! 01 ! NEUERSTELLUNG             ! Jäger    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0357                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM WORKFLOW                                      *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0357.
          05 PI-DRPA0357-PZVP-VERSION           PIC S9(004) BINARY.
          05 PI-DRPA0357-PROV-TERMIN            PIC  X(010).

       01 PO-DRPA0357.
          05 PO-DRPA0357-RC                     PIC  X(003).
             88 PO-DRPA0357-RC-OK                    VALUE 'OK '.
             88 PO-DRPA0357-RC-VSTAT-GEFUELLT        VALUE 'VSG'.
          05 PO-DRPA0357-DATEN.
             10 PO-DRPA0357-STRK-KZ             PIC  X(001).
      *
      ******** ENDE COPY-MEMBER DRPA0357 *****************************
