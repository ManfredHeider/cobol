      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0448.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drki0448.cpv  $
      *
      *   Rev 2.2   Sep 15 1999 13:09:50   T13EX39
      *AKI00049 WKZ eingebaut
      *
      *   Rev 2.1   Mar 10 1999 11:55:52   I13PC48
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 02 1999 11:24:02   I13PC48
      *Version 2.0 nach Umstellung auf NT
      *
      *   Rev 1.0   28 Jul 1998 09:54:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0448                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0448                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  15.09.99/08.17.21
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
      *    FÜR DIE FUNKTION DRKI0448
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0448.
           05  PI-DRKI0448-KEY.


             10 PI-DRKI0448-KFUVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0448-KFUVERTRAGSID   PIC X(13).


           05  PI-DRKI0448-DATEN.


             10 PI-DRKI0448-KFUBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0448-KFUBEITRAGWKZ   PIC X(01).
             10 PI-DRKI0448-KFUINVALIDSUMME   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0448-KFUINVSUMMEWKZ   PIC X(01).
             10 PI-DRKI0448-KFUPROGRESSINVPROZ   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI0448-KFUTODSUMME   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0448-KFUTODSUMMEWKZ   PIC X(01).
             10 PI-DRKI0448-KFUUNFALLART   PIC X(01).


      *
       01  PO-DRKI0448.
           05  PO-DRKI0448-RC                 PIC  X(003).
               88 PO-DRKI0448-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0448-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
