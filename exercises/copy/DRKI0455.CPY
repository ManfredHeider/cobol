      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0455.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI0455.CPv  $
      *
      *   Rev 2.2   Sep 15 1999 13:08:34   T13EX39
      *AKI00049 WKZ eingebaut
      *
      *   Rev 2.1   Mar 10 1999 11:56:08   I13PC48
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 02 1999 11:24:20   I13PC48
      *Version 2.0 nach Umstellung auf NT
      *
      *   Rev 1.0   28 Jul 1998 09:55:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0455                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0455                    *
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
      *    FÜR DIE FUNKTION DRKI0455
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0455.
           05  PI-DRKI0455-KEY.


             10 PI-DRKI0455-KFUVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0455-KFUVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0455.
           05  PO-DRKI0455-RC                 PIC  X(003).
               88 PO-DRKI0455-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0455-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0455-DATEN.


             10 PO-DRKI0455-KFUBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0455-KFUBEITRAGWKZ   PIC X(01).
             10 PO-DRKI0455-KFUINVALIDSUMME   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0455-KFUINVSUMMEWKZ   PIC X(01).
             10 PO-DRKI0455-KFUPROGRESSINVPROZ   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI0455-KFUTODSUMME   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0455-KFUTODSUMMEWKZ   PIC X(01).
             10 PO-DRKI0455-KFUUNFALLART   PIC X(01).


      *
      ******* ENDE COPY-MEMBER **************************************
