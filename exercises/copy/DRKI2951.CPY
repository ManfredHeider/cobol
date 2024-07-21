      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2951.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2951.CPv  $
      *
      *   Rev 2.1   Mar 18 2015 14:58:22   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - Standard-OZP 
      *für TCKI0474
      *
      *   Rev 2.0   Jan 19 2015 11:14:50   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2951                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2951                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.01.2015 / 11:37:05 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2951
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2951.
           05  PI-DRKI2951-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2951-FRVRSKGRUPPEID   PIC S9(4) COMP.
             10 PI-DRKI2951-FRVTHEMGSCHL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2951-FRVUNIVLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2951-FRVVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2951-FRVVETLFDNR   PIC S9(4) COMP.
END   *
 
      *
       01  PO-DRKI2951.
           05  PO-DRKI2951-RC                     PIC  X(003).
               88 PO-DRKI2951-RC-OK               VALUE 'OK '.
               88 PO-DRKI2951-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************

