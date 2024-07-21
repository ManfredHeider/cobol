      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0942.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0942.CPv  $
      *
      *   Rev 2.1   Nov 04 1999 14:36:42   T13EX40
      *Erweiterung für Bestandstrennung
      *
      *   Rev 2.0   Nov 04 1999 13:55:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0942                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI0942                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  28.10.99/19.23.01
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
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI0942
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0942.
           05  PI-DRKI0942-KEY.


             10 PI-DRKI0942-CLIENT-NR   PIC S9(9) COMP.

      *
       01  PO-DRKI0942.
           05  PO-DRKI0942-RC                 PIC  X(003).
               88 PO-DRKI0942-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0942-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0942-DATEN.

             10 PO-DRKI0942-SERVER-NR   PIC S9(9) COMP.




      *
      ******* ENDE COPY-MEMBER **************************************
