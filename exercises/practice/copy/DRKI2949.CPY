      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2949.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2949.CPv  $
      *
      *   Rev 2.1   Mar 18 2015 14:58:10   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - Standard-OZP 
      *für TCKI0474
      *
      *   Rev 2.0   Jan 19 2015 11:14:34   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2949                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2949                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.01.2015 / 11:37:02 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2949
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2949.
           05  PI-DRKI2949-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2949-FRVRSKGRUPPEID   PIC S9(4) COMP.
             10 PI-DRKI2949-FRVTHEMGSCHL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2949-FRVUNIVLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2949-FRVVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2949-FRVVETLFDNR   PIC S9(4) COMP.
END   *
 
 
           05  PI-DRKI2949-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2949-FRVDIFFGEF-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2949-FRVDIFFGES-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2949-FRVEINZVTGHOSTID   PIC  X(0015).
             10 PI-DRKI2949-FRVKUENDIGUNGKZ   PIC  X(0002).
             10 PI-DRKI2949-FRVRSKMERKMAL   PIC S9(9)V COMP-3.
             10 PI-DRKI2949-FRVUMSTELLTERMIN   PIC  X(010).
             10 PI-DRKI2949-FRVVERSNR   PIC  X(0040).
END   *
 
      *
       01  PO-DRKI2949.
           05  PO-DRKI2949-RC                     PIC  X(003).
               88 PO-DRKI2949-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2949-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

