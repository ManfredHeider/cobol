      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0462.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0462.CPv  $
      *
      *   Rev 2.1   Mar 10 1999 12:05:06   I13PC48
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 02 1999 11:24:22   I13PC48
      *Version 2.0 nach Umstellung auf NT
      *
      *   Rev 1.0   28 Jul 1998 09:56:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0462                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0462                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  27.08.98/09.58.26
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
      *    FÜR DIE FUNKTION DRKI0462
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  MIN / MAX <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0462.
           05  PI-DRKI0462-KEY.


             10 PI-DRKI0462-VETVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0462.
           05  PO-DRKI0462-RC                 PIC  X(003).
               88 PO-DRKI0462-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0462-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0462-DATEN.


             10 PO-DRKI0462-VETVERSTARLFDNR   PIC S9(4) COMP.


      *
      ******* ENDE COPY-MEMBER **************************************
