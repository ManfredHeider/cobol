      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0941.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0941.CPv  $
      *
      *   Rev 2.1   Nov 04 1999 14:36:40   T13EX40
      *Erweiterung für Bestandstrennung
      *
      *   Rev 2.0   Nov 04 1999 13:55:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0941                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FüR DIE FUNKTION DRKI0941                    *
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
      *    FšR DIE FUNKTION DRKI0941
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0941.
           05  PI-DRKI0941-KEY.


             10 PI-DRKI0941-CLIENT-NR   PIC S9(9) COMP.
             10 PI-DRKI0941-SERVER-NR   PIC S9(9) COMP.


           05  PI-DRKI0941-DATEN.


             10 PI-DRKI0941-AKTIV   PIC X(01).

      *
       01  PO-DRKI0941.
           05  PO-DRKI0941-RC                 PIC  X(003).
               88 PO-DRKI0941-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0941-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
