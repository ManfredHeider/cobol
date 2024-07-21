      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2950.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2950.CPv  $
      *
      *   Rev 2.1   Mar 18 2015 14:58:16   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - Standard-OZP 
      *für TCKI0474
      *
      *   Rev 2.0   Jan 19 2015 11:14:42   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2950                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2950                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.01.2015 / 11:37:04 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2950
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2950.
           05  PI-DRKI2950-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2950-FRVRSKGRUPPEID   PIC S9(4) COMP.
             10 PI-DRKI2950-FRVTHEMGSCHL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2950-FRVUNIVLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2950-FRVVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2950-FRVVETLFDNR   PIC S9(4) COMP.
END   *
 
 
           05  PI-DRKI2950-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2950-FRVDIFFGEF-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2950-FRVDIFFGES-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2950-FRVEINZVTGHOSTID   PIC  X(0015).
             10 PI-DRKI2950-FRVKUENDIGUNGKZ   PIC  X(0002).
             10 PI-DRKI2950-FRVRSKMERKMAL   PIC S9(9)V COMP-3.
             10 PI-DRKI2950-FRVUMSTELLTERMIN   PIC  X(010).
             10 PI-DRKI2950-FRVVERSNR   PIC  X(0040).
END   *
 
 
      *
       01  PO-DRKI2950.
           05  PO-DRKI2950-RC                     PIC  X(003).
               88 PO-DRKI2950-RC-OK               VALUE 'OK '.
               88 PO-DRKI2950-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

