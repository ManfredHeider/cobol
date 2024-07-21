      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0412.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0412.CPv  $
      *
      *   Rev 2.1   Mar 10 1999 14:37:20   I13PC18
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 10 1999 10:28:00   I13PC18
      *Version 2.0 wg. Umstellung auf NT
      *
      *   Rev 1.0   28 Jul 1998 08:39:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0412                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0412                    *
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
      *    FÜR DIE FUNKTION DRKI0412
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0412.
           05  PI-DRKI0412-KEY.


             10 PI-DRKI0412-VETVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0412-VETVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0412.
           05  PO-DRKI0412-RC                 PIC  X(003).
               88 PO-DRKI0412-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0412-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0412-DATEN.


             10 PO-DRKI0412-VETPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI0412-VETSUBTYPE   PIC X(01).
             10 PO-DRKI0412-VETVERSSUMME   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0412-NI-VETVERSSUMME   PIC S9(4) COMP.
             10 PO-DRKI0412-VETVERSSUMMEWKZ   PIC X(01).


      *
      ******* ENDE COPY-MEMBER **************************************
