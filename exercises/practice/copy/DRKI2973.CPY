      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.02.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2973.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2973.CPv  $
      *
      *   Rev 2.2   Sep 02 2015 15:11:22   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:47:24   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Feb 11 2015 14:19:07 T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2973                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2973                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         31.07.2015 / 15:49:50 / T13EX73
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
      *    FÜR DIE FUNKTION DRKI2973
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2973.
           05  PI-DRKI2973-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2973-G90VVERTRAGSNR   PIC  X(0015).
END   *
 
 
           05  PI-DRKI2973-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2973-G90VABRGEBSATZ   PIC S9(1)V9(2) COMP-3.
             10 PI-DRKI2973-G90VABSCHLVBNR   PIC S9(9) COMP.
             10 PI-DRKI2973-G90VABWBZKZ   PIC  X(0001).
             10 PI-DRKI2973-G90VABWBZNNAME   PIC  X(0030).
             10 PI-DRKI2973-G90VABWBZORT   PIC  X(0030).
             10 PI-DRKI2973-G90VABWBZPLZ   PIC S9(9) COMP.
             10 PI-DRKI2973-G90VABWBZSTRASSE   PIC  X(0040).
             10 PI-DRKI2973-G90VABWBZVNAME   PIC  X(0030).
             10 PI-DRKI2973-G90VAENDKZ   PIC  X(0002).
             10 PI-DRKI2973-G90VBEGINN   PIC  X(010).
             10 PI-DRKI2973-G90VEINRGEBBETR   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2973-G90VENDE   PIC  X(010).
             10 PI-DRKI2973-G90VFACTANKBETR   PIC S9(10)V9(2) COMP-3.
             10 PI-DRKI2973-G90VFACTGEBSATZ   PIC S9(1)V9(2) COMP-3.
             10 PI-DRKI2973-G90VGESID   PIC S9(4) COMP.
             10 PI-DRKI2973-G90VIBAN   PIC  X(0034).
             10 PI-DRKI2973-G90VINFOSPERRE   PIC  X(0001).
             10 PI-DRKI2973-G90VMANDANT   PIC S9(9) COMP.
             10 PI-DRKI2973-G90VMANDATREFNR   PIC  X(0035).
             10 PI-DRKI2973-G90VPRODUKTART   PIC S9(4) COMP.
             10 PI-DRKI2973-G90VPROGANZRECHN   PIC S9(9) COMP.
             10 PI-DRKI2973-G90VPROGBISDAT   PIC  X(010).
             10 PI-DRKI2973-G90VPROGGESUMS   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2973-G90VPROGVONDAT   PIC  X(010).
END   *
 
      *
       01  PO-DRKI2973.
           05  PO-DRKI2973-RC                     PIC  X(003).
               88 PO-DRKI2973-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2973-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

