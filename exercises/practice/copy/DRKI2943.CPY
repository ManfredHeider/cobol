      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2943.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2943.CPv  $
      *
      *   Rev 2.2   Mar 18 2015 14:49:02   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - Standard-OZP 
      *für TCKI0473
      *
      *   Rev 2.1   Jan 13 2015 13:24:14   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - Standard-OZP
      *für TCKI0473
      *
      *   Rev 2.0   Jan 8 2015 17:10:30 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2943                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2943                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.01.2015 / 09:38:58 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2943
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2943.
           05  PI-DRKI2943-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2943-PVVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2943-PVVETLFDNR   PIC S9(4) COMP.
END   *
 
      *
       01  PO-DRKI2943.
           05  PO-DRKI2943-RC                     PIC  X(003).
               88 PO-DRKI2943-RC-OK               VALUE 'OK '.
               88 PO-DRKI2943-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2943-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2943-LETZTEAENDERUNG   PIC  X(026).
             10 PO-DRKI2943-PVBATTERIEKZ   PIC  X(0002).
             10 PO-DRKI2943-PVBAUJAHR   PIC  X(0004).
             10 PO-DRKI2943-PVBEDINGUNG-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2943-PVBEDINGUNG-KTY   PIC S9(4) COMP.
             10 PO-DRKI2943-PVBETREIBER   PIC  X(0035).
             10 PO-DRKI2943-PVEINZVTGHOSTID   PIC  X(0015).
             10 PO-DRKI2943-PVGEBART-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2943-PVGEBART-KTY   PIC S9(4) COMP.
             10 PO-DRKI2943-PVINFOQUELLE-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2943-PVINFOQUELLE-KTY   PIC S9(4) COMP.
             10 PO-DRKI2943-PVLEISTUNG   PIC S9(4)V9(2) COMP-3.
             10 PO-DRKI2943-PVMAILING-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2943-PVMAILING-KTY   PIC S9(4) COMP.
             10 PO-DRKI2943-PVZUSATZINFO-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2943-PVZUSATZINFO-KTY   PIC S9(4) COMP.
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

