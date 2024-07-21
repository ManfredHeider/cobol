      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2945.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2945.CPv  $
      *
      *   Rev 2.2   Mar 18 2015 14:49:10   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - Standard-OZP 
      *für TCKI0473
      *
      *   Rev 2.1   Jan 13 2015 13:24:24   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - Standard-OZP
      *für TCKI0473
      *
      *   Rev 2.0   Jan 8 2015 17:10:34 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2945                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2945                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.01.2015 / 09:39:02 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2945
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2945.
           05  PI-DRKI2945-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2945-PVVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2945-PVVETLFDNR   PIC S9(4) COMP.
END   *
 
 
           05  PI-DRKI2945-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2945-PVBATTERIEKZ   PIC  X(0002).
             10 PI-DRKI2945-PVBAUJAHR   PIC  X(0004).
             10 PI-DRKI2945-PVBEDINGUNG-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2945-PVBETREIBER   PIC  X(0035).
             10 PI-DRKI2945-PVEINZVTGHOSTID   PIC  X(0015).
             10 PI-DRKI2945-PVGEBART-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2945-PVINFOQUELLE-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2945-PVLEISTUNG   PIC S9(4)V9(2) COMP-3.
             10 PI-DRKI2945-PVMAILING-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2945-PVZUSATZINFO-KNR   PIC S9(15)V COMP-3.
END   *
 
 
      *
       01  PO-DRKI2945.
           05  PO-DRKI2945-RC                     PIC  X(003).
               88 PO-DRKI2945-RC-OK               VALUE 'OK '.
               88 PO-DRKI2945-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

