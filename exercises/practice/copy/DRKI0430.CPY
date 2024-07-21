      * I13PC18   Stand: 27/08/98    13:06:43
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0430.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0430.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 13:54:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 17:07:00   I13PC18
      *FUNKTION UPDATE FUER ZTKI0357
      *
      *   Rev 1.0   28 Jul 1998 09:53:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*


      *  Update: Vertragsablauf, Vertragszustand (ID u. Datum)

      *****************************************************************
      *    COPY-MEMBER: DRKI0430                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0430                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  27.08.98/12.59.21
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0430
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0430.
           05  PI-DRKI0430-KEY.


             10 PI-DRKI0430-VERVERTRAGSID   PIC X(13).


           05  PI-DRKI0430-DATEN.


             10 PI-DRKI0430-VERDATVERTRAEND   PIC X(10).
             10 PI-DRKI0430-VERVERTRAGSABLAUF   PIC X(10).
             10 PI-DRKI0430-VERVERTRZUSTANDDAT   PIC X(10).
             10 PI-DRKI0430-VERVERTRZUSTANDID   PIC S9(4) COMP.


      *
       01  PO-DRKI0430.
           05  PO-DRKI0430-RC                 PIC  X(003).
               88 PO-DRKI0430-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0430-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
