      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2944.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2944.CPv  $
      *
      *   Rev 2.2   Mar 18 2015 14:49:06   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - Standard-OZP 
      *für TCKI0473
      *
      *   Rev 2.1   Jan 13 2015 13:24:18   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - Standard-OZP
      *für TCKI0473
      *
      *   Rev 2.0   Jan 8 2015 17:10:32 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2944                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2944                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.01.2015 / 09:39:00 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2944
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2944.
           05  PI-DRKI2944-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2944-PVVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2944-PVVETLFDNR   PIC S9(4) COMP.
END   *
 
 
           05  PI-DRKI2944-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2944-PVBATTERIEKZ   PIC  X(0002).
             10 PI-DRKI2944-PVBAUJAHR   PIC  X(0004).
             10 PI-DRKI2944-PVBEDINGUNG-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2944-PVBETREIBER   PIC  X(0035).
             10 PI-DRKI2944-PVEINZVTGHOSTID   PIC  X(0015).
             10 PI-DRKI2944-PVGEBART-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2944-PVINFOQUELLE-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2944-PVLEISTUNG   PIC S9(4)V9(2) COMP-3.
             10 PI-DRKI2944-PVMAILING-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2944-PVZUSATZINFO-KNR   PIC S9(15)V COMP-3.
END   *
 
      *
       01  PO-DRKI2944.
           05  PO-DRKI2944-RC                     PIC  X(003).
               88 PO-DRKI2944-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2944-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

