      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0410.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0410.CPv  $
      *
      *   Rev 2.1   Mar 10 1999 14:37:20   I13PC18
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 10 1999 10:27:58   I13PC18
      *Version 2.0 wg. Umstellung auf NT
      *
      *   Rev 1.0   28 Jul 1998 08:39:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0410                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0410                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  27.08.98/09.58.25
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
      *    FÜR DIE FUNKTION DRKI0410
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0410.
           05  PI-DRKI0410-KEY.


             10 PI-DRKI0410-VETVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0410-VETVERTRAGSID   PIC X(13).


           05  PI-DRKI0410-DATEN.


             10 PI-DRKI0410-VETPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI0410-VETSUBTYPE   PIC X(01).
             10 PI-DRKI0410-VETVERSSUMME   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0410-NI-VETVERSSUMME   PIC S9(4) COMP.
             10 PI-DRKI0410-VETVERSSUMMEWKZ   PIC X(01).


      *
       01  PO-DRKI0410.
           05  PO-DRKI0410-RC                 PIC  X(003).
               88 PO-DRKI0410-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0410-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
