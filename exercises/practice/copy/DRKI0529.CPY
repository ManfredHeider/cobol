      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0529.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drki0529.cpv  $
      *
      *   Rev 2.1   Apr 12 2000 10:41:06   T13EX39
      *AKI00065 Support-Tabellen
      *
      *   Rev 1.0   07 Aug 1998 14:45:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0529                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0529                    *
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
      *    FÜR DIE FUNKTION DRKI0529
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0529.
           05  PI-DRKI0529-KEY.


             10 PI-DRKI0529-VBBVERSBEDINGLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0529-VBBVERSBEDINGUNGID   PIC S9(9) COMP.
             10 PI-DRKI0529-VBBVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0529-VBBVERTRAGSID   PIC X(13).
             
      *
       01  PO-DRKI0529.
           05  PO-DRKI0529-RC                 PIC  X(003).
               88 PO-DRKI0529-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0529-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
