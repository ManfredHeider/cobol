      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2948.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2948.CPv  $
      *
      *   Rev 2.1   Mar 18 2015 14:58:06   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - Standard-OZP 
      *für TCKI0474
      *
      *   Rev 2.0   Jan 19 2015 11:14:26   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2948                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2948                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.01.2015 / 11:37:00 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2948
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2948.
           05  PI-DRKI2948-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2948-FRVRSKGRUPPEID   PIC S9(4) COMP.
             10 PI-DRKI2948-FRVTHEMGSCHL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2948-FRVUNIVLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2948-FRVVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2948-FRVVETLFDNR   PIC S9(4) COMP.
END   *
 
      *
       01  PO-DRKI2948.
           05  PO-DRKI2948-RC                     PIC  X(003).
               88 PO-DRKI2948-RC-OK               VALUE 'OK '.
               88 PO-DRKI2948-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2948-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2948-FRVDIFFGEF-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2948-FRVDIFFGEF-KTY   PIC S9(4) COMP.
             10 PO-DRKI2948-FRVDIFFGES-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2948-FRVDIFFGES-KTY   PIC S9(4) COMP.
             10 PO-DRKI2948-FRVEINZVTGHOSTID   PIC  X(0015).
             10 PO-DRKI2948-FRVKUENDIGUNGKZ   PIC  X(0002).
             10 PO-DRKI2948-FRVRSKMERKMAL   PIC S9(9)V COMP-3.
             10 PO-DRKI2948-FRVTHEMGSCHL-KTY   PIC S9(4) COMP.
             10 PO-DRKI2948-FRVUMSTELLTERMIN   PIC  X(010).
             10 PO-DRKI2948-FRVVERSNR   PIC  X(0040).
             10 PO-DRKI2948-LETZTEAENDERUNG   PIC  X(026).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

