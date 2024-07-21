      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0986.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0986.CPv  $
      *
      *   Rev 2.1   Mar 19 1999 15:47:06   T13EX21
      *APA00012 - MFE-Umstellung 
      *
      *   Rev 2.0   Feb 27 1999 14:29:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:20:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0986                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF:  VERTRAG UPDATEN                          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GRÜNER                                           *
      *    DATUM:    02.07.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!02.07.98! 01 ! NEU                       ! GRÜNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0983                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0986.
          05 PI-DRPA0986-PROGRAMM              PIC  X(008).
          05 PI-DRPA0986-UFKT                  PIC  9(002).
          05 PI-DRPA0986-PVER-VERSION-ALT      PIC S9(4) USAGE COMP.
          05 PI-DRPA0986-DATEN.
          COPY DRPA0988.
      *
       01 PO-DRPA0986.
          05 PO-DRPA0986-RC                         PIC  X(003).
             88 PO-DRPA0986-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0986-RC-DUPLICATE-VALUE            VALUE 'DVA'.
             88 PO-DRPA0986-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0986-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
