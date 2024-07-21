      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0402.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0402.CPv  $
      *
      *   Rev 2.1   Mar 10 1999 11:50:26   I13PC48
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 02 1999 11:23:08   I13PC48
      *Version 2.0 nach Umstellung auf NT
      *
      *   Rev 1.0   14 Jul 1998 17:05:06   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0402                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0402                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  27.08.98/09.58.26
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
      *    FÜR DIE FUNKTION DRKI0402
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0402.
           05  PI-DRKI0402-KEY.


             10 PI-DRKI0402-VETVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0402-VETVERTRAGSID   PIC X(13).


           05  PI-DRKI0402-DATEN.


             10 PI-DRKI0402-VETPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI0402-VETSUBTYPE   PIC X(01).
             10 PI-DRKI0402-VETVERSSUMME   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0402-NI-VETVERSSUMME   PIC S9(4) COMP.
             10 PI-DRKI0402-VETVERSSUMMEWKZ   PIC X(01).

      *
       01  PO-DRKI0402.
           05  PO-DRKI0402-RC                 PIC  X(003).
               88 PO-DRKI0402-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0402-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
