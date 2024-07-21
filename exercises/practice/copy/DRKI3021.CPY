      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3021.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3021.CPv  $
      *
      *   Rev 2.2   Jul 09 2015 10:15:40   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: OZPs
      *
      *   Rev 2.1   Jul 09 2015 09:50:20   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: OZPs
      *
      *   Rev 2.0   Jul 1 2015 10:46:04 T13EX48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3021                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3021                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         06.07.2015 / 11:40:18 / T13EX48
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
      *    FÜR DIE FUNKTION DRKI3021
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3021.
           05  PI-DRKI3021-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3021-LGRRNTART   PIC S9(4) COMP.
             10 PI-DRKI3021-LGRRNTBEGINN   PIC  X(010).
             10 PI-DRKI3021-LGRVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI3021-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI3021-LGRGSMTHOEHE   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3021-LGRRNTABLAUF   PIC  X(010).
             10 PI-DRKI3021-LGRRNTZAHLW-KNR   PIC S9(4) COMP.
             10 PI-DRKI3021-LGRRNTZAHLW-KTY   PIC S9(4) COMP.
END   *

      *
       01  PO-DRKI3021.
           05  PO-DRKI3021-RC                     PIC  X(003).
               88 PO-DRKI3021-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3021-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************


