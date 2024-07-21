      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0530.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0530.CPv  $
      *
      *   Rev 2.1   Apr 12 2000 10:40:56   T13EX39
      *AKI00065 Support-Tabellen
      *
      *   Rev 1.0   07 Aug 1998 14:45:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0530                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0530                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.04.00/12.20.16
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
      *    FÜR DIE FUNKTION DRKI0530
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  MIN / MAX <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0530.
           05  PI-DRKI0530-KEY.


             10 PI-DRKI0530-VBBVERSBEDINGUNGID   PIC S9(9) COMP.
             10 PI-DRKI0530-VBBVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0530-VBBVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0530.
           05  PO-DRKI0530-RC                 PIC  X(003).
               88 PO-DRKI0530-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0530-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0530-DATEN.


             10 PO-DRKI0530-VBBVERSBEDINGLFDNR   PIC S9(4) COMP.


      *
      ******* ENDE COPY-MEMBER **************************************
