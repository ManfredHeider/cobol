      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0581.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0581.CPv  $
      *
      *   Rev 2.1   Mar 10 1999 12:06:22   I13PC48
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 02 1999 11:25:36   I13PC48
      *Version 2.0 nach Umstellung auf NT
      *
      *   Rev 1.0   07 Aug 1998 14:49:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0581                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0581                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  14.08.98/15.26.03
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
      *    FÜR DIE FUNKTION DRKI0581
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  MIN / MAX <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0581.
           05  PI-DRKI0581-KEY.


             10 PI-DRKI0581-VRAVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0581.
           05  PO-DRKI0581-RC                 PIC  X(003).
               88 PO-DRKI0581-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0581-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0581-DATEN.


             10 PO-DRKI0581-VRALFDNR   PIC S9(4) COMP.


      *
      ******* ENDE COPY-MEMBER **************************************
