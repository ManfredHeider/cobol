      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0469.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0469.CPv  $
      *
      *   Rev 2.1   Apr 14 2000 16:02:02   T13EX41
      *AKI00065
      *- Support-Tabellen
      *
      *   Rev 1.0   28 Jul 1998 09:56:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0469                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0469                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.04.00/12.20.26
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
      *    FÜR DIE FUNKTION DRKI0469
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0469.
           05  PI-DRKI0469-KEY.


             10 PI-DRKI0469-VKBKLAUSELID   PIC S9(9) COMP.
             10 PI-DRKI0469-VKBKLAUSELLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0469-VKBVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0469-VKBVERTRAGSID   PIC X(13).


           05  PI-DRKI0469-DATEN.


             10 PI-DRKI0469-VKBKLAUSEL   PIC X(10).
             10 PI-DRKI0469-VKBKLAUSELPARTGID   PIC S9(4) COMP.

      *
       01  PO-DRKI0469.
           05  PO-DRKI0469-RC                 PIC  X(003).
               88 PO-DRKI0469-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0469-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
