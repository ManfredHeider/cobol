      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0434.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0434.CPv  $
      *
      *   Rev 2.1   Mar 10 1999 14:37:26   I13PC18
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 10 1999 10:28:08   I13PC18
      *Version 2.0 wg. Umstellung auf NT
      *
      *   Rev 1.0   28 Jul 1998 09:53:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*


      *  nur Produkt-ID

      *****************************************************************
      *    COPY-MEMBER: DRKI0434                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0434                    *
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
      *    FÜR DIE FUNKTION DRKI0434
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0434.
           05  PI-DRKI0434-KEY.


             10 PI-DRKI0434-VETVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0434-VETVERTRAGSID   PIC X(13).


           05  PI-DRKI0434-DATEN.


             10 PI-DRKI0434-VETPRODUKTID   PIC S9(4) COMP.


      *
       01  PO-DRKI0434.
           05  PO-DRKI0434-RC                 PIC  X(003).
               88 PO-DRKI0434-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0434-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
